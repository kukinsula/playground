package main

import (
	"bufio"
	"context"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"net"
	"os"
	"os/signal"
	"strings"
	"time"

	"github.com/kukinsula/birpc"
	"github.com/kukinsula/birpc/examples/chat/protocol"
)

// TODO : KeepAlive

const (
	network = "tcp"
	ip      = "127.0.0.1"
	port    = "20000"
	address = ip + ":" + port
)

type Client struct {
	*birpc.Client
	Login string
	stop  chan struct{}
}

func main() {
	var login, to, serializer string

	flag.StringVar(&login, "login", "<empty>", "Chat server login")
	flag.StringVar(&to, "to", "<empty>", "Chat server peer login")
	flag.StringVar(&serializer, "serializer", "", "Serializer between clients")

	flag.Parse()

	err := start(login, to, serializer)
	if err != nil {
		log.Printf("Error: %s", err)
	}
}

func start(login, to, serializer string) error {
	client := NewClient(login)

	err := client.Connect(login, serializer, ip, port)
	if err != nil {
		return err
	}

	client.Register("chat", client)

	birpc.SetLogger(log.New(ioutil.Discard, "", 0))

	// RPC serving
	serveErrChan := make(chan error)
	go func() { serveErrChan <- client.Serve() }()
	time.Sleep(100 * time.Millisecond)

	var resp protocol.Message
	err = client.Call("chat.Login", &protocol.Login{Login: login}, &resp)
	if err != nil {
		return err
	}

	if resp.Code != protocol.RC_OK {
		return fmt.Errorf("Login failed: %s", resp)
	}

	// Waits for CTRL-C
	go func() {
		interruptChan := make(chan os.Signal, 1)
		signal.Notify(interruptChan, os.Interrupt)
		<-interruptChan

		client.Close()
	}()

	err = client.Chat(to)
	if err != nil {
		return err
	}

	return <-serveErrChan
}

var defaultSerializers = "jsonrpc;gob;jsonrpc2"

func NewClient(login string) *Client {
	return &Client{
		Login: login,
		stop:  make(chan struct{}, 1),
	}
}

func (c *Client) Connect(login, serializer, ip, port string) error {
	address := ip + ":" + port

	log.Printf("Dialing %s:%s...", network, address)

	conn, err := net.Dial(network, address)
	if err != nil {
		return err
	}

	serializers := defaultSerializers
	if serializer != "" {
		serializers = serializer
	}

	_, err = conn.Write([]byte(serializers + "\n"))
	if err != nil {
		return err
	}

	choosenSerializer := make([]byte, 16)
	n, err := conn.Read(choosenSerializer)
	if err != nil {
		return err
	}

	serializer = strings.TrimSpace(string(choosenSerializer[:n]))

	codec, err := protocol.Serializer(serializer).Codec(conn)
	if err != nil {
		return err
	}

	log.Printf("Connected to %s:%s!", network, address)

	c.Client = birpc.NewClientWithCodec(conn, codec)

	return nil
}

func (c *Client) Chat(to string) (err error) {
	scanner := bufio.NewScanner(os.Stdin)

	for {
		select {
		case <-c.stop:
			return nil

		default:
			fmt.Printf("> ")
			scanner.Scan()

			text := scanner.Text()
			if text == "" {
				continue
			}

			var resp protocol.Message

			err = c.Call("chat.Message", protocol.Msg(c.Login, to, 0, text), &resp)
			if err != nil {
				return err
			}

			if resp.Code != protocol.RC_OK {
				if resp.Code == protocol.RC_NOT_FOUND {
					log.Println("Message not sent:", resp.Data)
					continue
				}

				return fmt.Errorf("Call 'chat.Message' failed: %s", err)
			}
		}
	}

	return err
}

func (c *Client) Message(ctx context.Context,
	client *birpc.Client,
	params *protocol.Message,
	result *protocol.Message,
) error {

	fmt.Printf("%v < %s\n", params.Data, params.From)

	*result = protocol.MsgOK(c.Login)

	return nil
}

func (c *Client) Close() error {
	c.stop <- struct{}{}

	var resp protocol.Message

	err := c.Call("chat.Logout", &protocol.Login{Login: c.Login}, &resp)
	if err != nil {
		log.Printf("Logout failed: %s", err)
	} else if resp.Code != protocol.RC_OK {
		log.Printf("Logout failed: Data:%v, Code:%s", resp.Code, resp.Data)
	}

	close(c.stop)

	return c.Client.Close()
}
