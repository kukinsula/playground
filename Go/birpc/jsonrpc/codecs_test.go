package jsonrpc

import (
	"context"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"net"
	"reflect"
	"runtime"
	"testing"
	"time"

	"github.com/kukinsula/birpc"
)

const (
	network = "tcp"
	host    = "127.0.0.1"
	port    = "5556"
	address = host + ":" + port
)

type Error struct {
	Code    int         `json:"code"`
	Message string      `json:"message"`
	Data    interface{} `json:"data"`
}

func (e *Error) Error() string {
	b, _ := json.Marshal(e)

	return string(b)
}

var (
	nbRPC = 1 // Number of RPC made
	mute  = false

	ErrServiceError = errors.New("ERROR")

	expectedError = &Error{200, "OK", map[string]interface{}{
		"a": 1, "b": 2, "c": 3, "d": 3.14159,
	}}
)

func init() {
	flag.BoolVar(&mute, "mute", false, "Mute tests")
	flag.IntVar(&nbRPC, "nbRPC", nbRPC, "Number of RPC")

	flag.Parse()

	// Mute log output
	if mute {
		birpc.SetLogger(log.New(ioutil.Discard, "", 0))
	}
}

// func service42(ctx context.Context, c *birpc.Client, param interface{}) (interface{}, error) {
// 	return 42, nil
// }

// func servicePI(ctx context.Context, c *birpc.Client, param interface{}) (interface{}, error) {
// 	return 3.14159, nil
// }

// func serviceError(ctx context.Context, c *birpc.Client, param interface{}) (interface{}, error) {
// 	return nil, expectedError
// }

// func servicePanic(ctx context.Context, c *birpc.Client, param interface{}) (interface{}, error) {
// 	panic("This is a panic")

// 	return nil, nil
// }

type testService struct{}

func (t *testService) FortyTwo(ctx context.Context,
	client *birpc.Client,
	params int,
	result *int,
) error {

	*result = 42

	return nil
}

func (t *testService) Pi(ctx context.Context,
	client *birpc.Client,
	params int,
	result *float64,
) error {

	*result = float64(3.14159)

	return nil
}

func (t *testService) Error(ctx context.Context,
	client *birpc.Client,
	params int,
	result *float64,
) error {

	return fmt.Errorf("JUST AN ERROR")
}

func (t *testService) Panic(ctx context.Context,
	client *birpc.Client,
	params int,
	result *float64,
) error {

	panic("THIS IS A PANIC")

	return nil
}

func newClientTest(codecBuilder func(rwc io.ReadWriteCloser) birpc.Codec) (*birpc.Client, error) {
	conn, err := net.Dial(network, address)
	if err != nil {
		return nil, err
	}

	return birpc.NewClientWithCodec(conn, codecBuilder(conn)), nil
}

// wait gives 100 milliseconds to ListenAndServe to start or fails now.
func wait(t *testing.T, errChan chan error) {
	select {
	case <-time.After(time.Duration(100) * time.Millisecond):
	case err := <-errChan:
		if err != nil && err != birpc.ErrShutdown {
			t.Fatalf("ListenAndServe failed: %s", err)
		}
	}
}

func New2(rwc io.ReadWriteCloser) birpc.Codec {
	return NewWithVersion(rwc, Version2)
}

func TestRPCWithAllCodecs(t *testing.T) {
	testCodec(birpc.NewGobCodec, t)
	testCodec(New, t)
	testCodec(New2, t)
}

func testCodec(codecBuilder func(rwc io.ReadWriteCloser) birpc.Codec, t *testing.T) {
	if !mute {
		log.Printf("Test birpc.Codec: %s\n",
			runtime.FuncForPC(reflect.ValueOf(codecBuilder).Pointer()).Name())
	}

	server := birpc.NewServer()
	serverErrChan := make(chan error)
	sync := make(chan struct{})

	// server.RegisterFunc("42", service42)
	// server.RegisterFunc("serviceError", serviceError)
	// server.RegisterFunc("servicePanic", servicePanic)

	n, err := server.Register("Service", &testService{})
	if err != nil {
		t.Errorf("Server.Register failed: %s", err)
	}
	if n != 4 {
		t.Errorf("Server.Register shound register 4, not %d", n)
	}

	go func() {
		serverErrChan <- server.ListenAndServe(network, address,
			birpc.ConnHandlerFunc(func(s *birpc.Server, conn net.Conn) error {
				client := birpc.NewClientWithCodec(conn, codecBuilder(conn))
				clientErrChan := make(chan error)
				expected := 3.14159

				var result float64

				go func() {
					clientErrChan <- s.ServeClient(client)
				}()
				wait(t, clientErrChan)

				sync <- struct{}{} // Syncs with Client

				for idx := 0; idx < nbRPC; idx++ {
					err := client.Call("Service.Pi", nil, &result)
					if err != nil {
						t.Errorf("Client.Call PI failed: %s", err)
					}

					if result != expected {
						t.Errorf("Expected result %f instead of %f", expected, result)
					}

					// Go
					result = 0
					done := make(chan *birpc.Call, 1)

					call, err := client.Go("PI", nil, &result, done)
					if err != nil {
						t.Fatalf("Client.Go PI failed: %s", err)
					}

					call = <-done // Waits for RPC to end

					result, ok := call.Result.(*float64)
					if !ok {
						t.Errorf("result should be type *float64, not %T", call.Result)
					}

					if *result != expected {
						t.Errorf("Expected result %d instead of %d", expected, *result)
					}

					// Notifiy
					err = client.Notify("PI", nil)
					if err != nil {
						t.Fatalf("Client.Notify PI failed: %s", err)
					}
				}

				sync <- struct{}{} // Syncs with Client

				client.Wait()
				<-clientErrChan

				return nil
			}))
	}()
	wait(t, serverErrChan)

	client, err := newClientTest(codecBuilder)
	if err != nil {
		t.Fatal(err)
	}

	clientErrChan := make(chan error)

	// client.RegisterFunc("PI", servicePI)
	// client.RegisterFunc("ERROR", serviceError)

	n, err = client.Register("Service", &testService{})
	if err != nil {
		t.Errorf("Server.Register failed: %s", err)
	}
	if n != 4 {
		t.Errorf("Server.Register shound register 4, not %d", n)
	}

	go func() {
		clientErrChan <- client.Serve()
	}()
	wait(t, clientErrChan)

	<-sync // Syncs with Client on Server side

	expected := 42
	var result int

	for idx := 0; idx < nbRPC; idx++ {
		// Call
		err = client.Call("Service.FortyTwo", 1234, &result)
		if err != nil {
			t.Fatalf("Client.Call FortyTwo failed: %s", err)
		}

		if result != expected {
			t.Errorf("Expected result %d instead of %d", expected, result)
		}

		// Go
		done := make(chan *birpc.Call, 1)

		call, err := client.Go("42", 1234, &result, done)
		if err != nil {
			t.Fatalf("Client.Go 42 failed: %s", err)
		}

		call = <-done // Waits for RPC to end

		res, ok := call.Result.(*int)
		if !ok {
			t.Errorf("result should be type *int, not %T", call.Result)
		}

		if *res != expected {
			t.Errorf("Expected result %d instead of %d", expected, *res)
		}

		// Notifiy
		err = client.Notify("42", 1234)
		if err != nil {
			t.Fatalf("Client.Notify 42 failed: %s", err)
		}
	}

	// Launch nbRPC Go calls
	done := make(chan *birpc.Call, nbRPC)

	for idx := 0; idx < nbRPC; idx++ {
		var result int

		_, err := client.Go("42", 1234, &result, done)
		if err != nil {
			t.Fatalf("Client.Go 42 failed: %s", err)
		}
	}

	// Wait for nbRPC calls
	for idx := 0; idx < nbRPC; idx++ {
		call := <-done

		res, ok := call.Result.(*int)
		if !ok {
			t.Errorf("result should be type *int, not %T", call.Result)
		}

		if *res != expected {
			t.Errorf("Expected result %d instead of %d", expected, *res)
		}
	}

	// Service returning an error
	err = client.Call("serviceError", 1234, &result)
	if err.Error() != expectedError.Error() {
		t.Fatalf("Client.Call should return %s not %s", expectedError, err)
	}

	// Service that panics
	err = client.Call("servicePanic", 1234, &result)
	if err == nil {
		t.Fatalf("Client.Call servicePanic should fail")
	}

	<-sync // Syncs with Client on Server side

	client.Wait()
	<-clientErrChan

	server.Close()
}

// TODO : TestBiRPC
