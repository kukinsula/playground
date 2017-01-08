package birpc

import (
	"flag"
	_ "fmt"
	"net"
	"testing"
	"time"
)

const (
	network = "tcp"
	host    = "127.0.0.1"
	port    = "22000"
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

func newClientWithCodec(name SerializerType) (*Client, error) {
	conn, err := connect(network, host, port)
	if err != nil {
		return nil, err
	}

	codec, err := CreateBufferedCodec(name, conn, bufSize)
	if err != nil {
		return nil, err
	}

	return NewClientWithCodec(conn, codec, nil), nil
}

func TestServerStartStop(t *testing.T) {
	codecs := []SerializerType{JSON /*, MSGPACK, CBOR, BINC, GOB*/}

	expextedResult := uint64(42)
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

			codec, err := CreateBufferedCodec(name, conn, bufSize)
			if err != nil {
				t.Errorf("CreateBufferedCode: %s", err)
				return
			}

			server.ServeCodec(conn, codec)
			server.Unregister(conn)

			conn.Close()
		})
		sleep(100)

		client, err := newClientWithCodec(name)
		go client.Start()
		sleep(100)

		var res uint64
		err = client.Call(method, 1234, &res)
		if err != nil {
			t.Errorf("Call %s should not fail: %s", method, err)
		}

		if res != expextedResult {
			t.Errorf("Call %s should returned %d, but expected not %d",
				method, res, expextedResult)
		}

		client.Stop()
		server.CloseAllClients()
		server.Stop()

		if verbose {
			logger.Println()
		}
	}
}
