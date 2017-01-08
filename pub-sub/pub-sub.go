package main

import (
	"fmt"
	"os"
	"os/signal"
	"strings"
	"time"

	"github.com/nats-io/go-nats"
)

const (
	addressSufix = "nats://"
)

type PubSub struct {
	conn          *nats.EncodedConn
	subscriptions map[string]*nats.Subscription
}

func NewPubSub(config *Config) (*PubSub, error) {
	address := addressSufix +
		config.User + ":" + config.Password + "@" + config.Address

	options := &nats.Options{
		Url:     address,
		Verbose: true,
		AsyncErrorCB: func(conn *nats.Conn, subscription *nats.Subscription, err error) {
			logger.Printf("Error (subject: %s, queue: %s): %s",
				subscription.Subject, subscription.Queue, err)
		},
	}

	logger.Printf("connecting to %s...", address)

	conn, err := options.Connect()
	if err != nil {
		return nil, err
	}

	encodedConn, err := nats.NewEncodedConn(conn, config.Codec)
	if err != nil {
		return nil, err
	}

	return &PubSub{
		conn:          encodedConn,
		subscriptions: make(map[string]*nats.Subscription),
	}, nil
}

func (p *PubSub) NewPubSubRetries(config *Config, retries int) (pubsub *PubSub, err error) {
	for i := 0; i < retries; i++ {
		pubsub, err = NewPubSub(config)
		if err == nil {
			break
		}

		logger.Printf("#%d NewPubSub failed (retry in 1 sec): %s", i, err)
		time.Sleep(time.Second)
	}

	return pubsub, err
}

func (p *PubSub) Start(config *Config) (err error) {
	switch config.Action {
	case "sub", "SUB":
		err = p.subscribe(config.Subject,
			func(subject string, v interface{}) {
				logger.Printf("SUB %s %v", subject, v)
			})

	case "pub", "PUB":
		err = p.publish(config.Subject, config.Message)

	case "req", "REQ":
		err = p.request(config.Subject, config.Message,
			time.Duration(config.Timeout)*time.Second)

	case "resp", "RESP":
		err = p.respond(config.Subject, config.Message)

	default:
		return fmt.Errorf("unknown action '%s'", config.Action)
	}

	return err
}

func (p *PubSub) subscribe(subject string, cb interface{}) error {
	subjects := strings.Split(subject, ",")

	for _, subject := range subjects {
		subscription, err := p.conn.Subscribe(subject, cb)
		if err != nil {
			return fmt.Errorf("Subscribe failed: %s", err)
		}

		p.subscriptions[subject] = subscription
	}

	waitForInterrupt()

	return nil
}

func (p *PubSub) publish(subject string, msg interface{}) error {
	subjects := strings.Split(subject, ",")

	for _, subject := range subjects {
		err := p.conn.Publish(subject, msg)
		if err != nil {
			return fmt.Errorf("Publish failed: %s", err)
		}

		logger.Printf("PUB %s %v", subject, msg)
	}

	return nil
}

func (p *PubSub) request(subject string, msg interface{}, timeout time.Duration) error {
	subjects := strings.Split(subject, ",")
	errorChan := make(chan error, len(subjects))

	for _, subject := range subjects {
		go func(subject string, msg interface{}) {
			logger.Printf("REQ %s %v", subject, msg)

			var resp interface{}
			err := p.conn.Request(subject, msg, &resp, timeout)
			if err != nil {
				errorChan <- fmt.Errorf("Request failed: %s", err)
			}

			logger.Printf("RESP to %s: %v", subject, resp)

			errorChan <- nil
		}(subject, msg)
	}

	var str string
	for err := range errorChan {
		if err != nil {
			str += err.Error()
		}
	}
	close(errorChan)

	if str != "" {
		return fmt.Errorf(str)
	}

	return nil
}

func (p *PubSub) respond(subject string, answer interface{}) error {
	cb := func(subject, reply string, msg interface{}) {
		logger.Printf("REQ for %s with body %v", subject, msg)

		err := p.conn.Publish(reply, answer)
		if err != nil {
			logger.Printf("Respond failed: %s", err)
		}
	}

	subscription, err := p.conn.Subscribe(subject, cb)
	if err != nil {
		return fmt.Errorf("Respond subscribe failed: %s", err)
	}

	p.subscriptions[subject] = subscription
	waitForInterrupt()

	return nil
}

func (p *PubSub) Stop() {
	p.UnsubscribeAll()

	p.conn.Close()
}

func (p *PubSub) UnsubscribeAll() {
	for _, subscription := range p.subscriptions {
		subscription.Unsubscribe()
	}
}

func waitForInterrupt() {
	signalChan := make(chan os.Signal)
	signal.Notify(signalChan, os.Interrupt)

	<-signalChan
}
