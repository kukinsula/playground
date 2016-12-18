package birpc

import (
	"flag"
	"net"
	"testing"
	"time"
)

const (
	network = "tcp"
	host    = "127.0.0.1"
	port    = "20000"
	address = host + ":" + port
)

func init() {
	var v bool

	flag.BoolVar(&verbose, "debug", false, "Print debug information")

	SetVerbose(v)
}

func sleep(ms int) {
	time.Sleep(time.Duration(ms) * time.Millisecond)
}

func connect(network, host, port string) (net.Conn, error) {
	return net.Dial(network, address)
}

func newClientWithCodec(name string) (*Client, error) {
	conn, err := connect(network, host, port)
	if err != nil {
		return nil, err
	}

	codec, err := CreateBufferedCode(name, conn, bufSize)
	if err != nil {
		return nil, err
	}

	return NewClientWithCodec(conn, codec, nil), nil
}

func TestServerStartStop(t *testing.T) {
	codecs := []string{JSON, MSGPACK, CBOR, BINC /* GOB */}

	expextedResult := 42
	method := "foo"

	for _, name := range codecs {
		if verbose {
			logger.Printf("TestServerStartStop with codec %s", name)
		}

		server, err := NewTCPServer(host, port)
		if err != nil {
			t.Fatal(err)
		}

		server.RegisterFunc(method, func(req *Request, v interface{}) (interface{}, error) {
			return expextedResult, nil
		})

		go server.StartHandler(func(conn net.Conn) {
			server.Register(conn)

			codec, err := CreateBufferedCode(name, conn, bufSize)
			if err != nil {
				t.Errorf("CreateBufferedCode: %s", err)
				return
			}

			server.ServeCodec(conn, codec)
			server.Unregister(conn)
		})
		sleep(100)

		client, err := newClientWithCodec(name)
		go client.Start()
		sleep(100)

		logger.Printf("Client calling %s...", method)

		var res int
		err = client.Call(method, 1234, &res)
		if err != nil {
			t.Errorf("Call %s should not fail: %s", err)
		}

		logger.Printf("Client call %s finished!", method)

		if res != 42 {
			t.Errorf("Call %s should return %d, not %d", method, expextedResult, res)
		}

		client.Stop()

		server.Stop()
	}
}
