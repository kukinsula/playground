package main

import (
	"flag"
	"fmt"
)

const (
	DefaultHost           = "127.0.0.1"
	DefaultPort           = "20000"
	DefaultAddress        = DefaultHost + ":" + DefaultPort
	defaultRequestTimeout = 1

	usage = `usage: ./pub-sub [OPTIONS]

NATS options:
  -address          NATS server address (default: %s)
  -host             NATS server host address (default: %s)
  -port             NATS server port address (default: %s)
  -user             NATS user name
  -password         NATS usr password
  -codec            NATS serializer: json | gob (default: json)

  -subject          Action's subject message
  -message          Action's message

  -action           What to do: PUB | SUB | REQ | RESP ?
    Publish message at subject:         PUB -subject foo -message bar
    Subscribe to subject:               SUB -subject foo
    Request for subject with message:   REQ -subject foo -message bar
    Respond to subject with message:    RESP -subject foo -message bar

  -timeout          Request timeout
`
)

var (
	errEmptyAction = fmt.Errorf("empty action")
)

func init() {
	flag.Usage = func() {
		fmt.Printf(usage, DefaultAddress, DefaultHost, DefaultPort)
	}
}

type Config struct {
	Address  string
	Host     string
	Port     string
	User     string
	Password string
	Codec    string
	Action   string
	Subject  string
	Message  string
	Timeout  int
}

func GetConfig() (*Config, error) {
	config := &Config{}

	flag.StringVar(&config.Address, "address", DefaultAddress,
		"NATS server addres")
	flag.StringVar(&config.Host, "host", DefaultHost,
		"NATS server host")
	flag.StringVar(&config.Port, "port", DefaultPort,
		"NATS server port")
	flag.StringVar(&config.User, "user", "",
		"NATS user name")
	flag.StringVar(&config.Password, "password", "",
		"NATS user password")
	flag.StringVar(&config.Codec, "codec", "json",
		"Codec user over the conn (json | gob)")
	flag.StringVar(&config.Action, "action", "pub",
		"what to do ? (pub | sub)")
	flag.StringVar(&config.Subject, "subject", "",
		"NATS subject to subscribe/publish at/to")
	flag.StringVar(&config.Message, "message", "foobar",
		"the message to publish")
	flag.IntVar(&config.Timeout, "timeout", defaultRequestTimeout,
		"timeout for request")

	flag.Parse()

	if config.Action == "" {
		return nil, errEmptyAction
	}

	if config.Host != "" || config.Port != "" {
		config.Address = config.Host + ":" + config.Port
	}

	return config, nil
}
