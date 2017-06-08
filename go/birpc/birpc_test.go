package birpc

import (
	"flag"
	"io"
	"io/ioutil"
	"log"
	"net"
	"syscall"
	"testing"
	"time"
)

// TODO : wait prend un duration en plus wait(t, errChan, dur int)

const (
	network = "tcp"
	host    = "127.0.0.1"
	port    = "5555"
	address = host + ":" + port

	CLOSE    = "CLOSE"
	SHUTDOWN = "SHUTDOWN"
	WAIT     = "WAIT"
)

var (
	nbConns = 1 // Number of conns established
)

func init() {
	var mute bool

	flag.BoolVar(&mute, "mute", false, "Mute tests")
	flag.IntVar(&nbConns, "nbConns", nbConns, "Number of connections")

	flag.Parse()

	// Mute log output
	if mute {
		logger = log.New(ioutil.Discard, "", 0)
	}

	noFiles := uint64(999999)
	err := syscall.Setrlimit(syscall.RLIMIT_NOFILE, &syscall.Rlimit{
		Cur: noFiles,
		Max: noFiles,
	})

	if err != nil {
		if err != syscall.EPERM {
			logger.Printf("Increase number open file descriptors to %d failed: %s",
				noFiles, err)
		}
	}
}

func newConn() (net.Conn, error) { return net.Dial(network, address) }
func sleep(s int)                { time.Sleep(time.Duration(s) * time.Millisecond) }

func newClientTest(codecBuilder func(rwc io.ReadWriteCloser) Codec) (*Client, error) {
	conn, err := newConn()
	if err != nil {
		return nil, err
	}

	return NewClientWithCodec(conn, codecBuilder(conn)), nil
}

// wait gives 100 milliseconds to ListenAndServe to start or fails now.
func wait(t *testing.T, errChan chan error) {
	select {
	case <-time.After(time.Duration(100) * time.Millisecond):
	case err := <-errChan:
		if err != nil && err != ErrShutdown {
			t.Fatalf("ListenAndServe failed: %s", err)
		}
	}
}

func TestServerServeCloseShutdownWait(t *testing.T) {
	var server *Server
	var err error

	errChan := make(chan error)

	// Serve + (with or without) connections + Close + Shutdown + Wait
	for _, withConns := range []bool{false, true} {
		server = NewServer()

		go func() {
			errChan <- server.ListenAndServe(network, address,
				ConnHandlerFunc(func(s *Server, conn net.Conn) error {
					sleep(20) // Fakes client processing

					return nil
				}))
		}()
		wait(t, errChan)

		if !server.Serving() {
			t.Errorf("Server should be serving")
		}

		if withConns {
			for i := 0; i < nbConns; i++ {
				_, err = newClientTest(NewGobCodec)
				if err != nil {
					logger.Printf("newClientTest failed: %s", err)
				}
			}
		}

		err = server.Close()
		if err != nil {
			logger.Printf("Server.Close failed: %s", err)
		}

		server.Shutdown()
		server.Wait()

		<-errChan
	}

	// Double Close should fail
	err = server.Close()
	if err != ErrAlreadyClosed {
		t.Errorf("Server.Close should return ErrAlreadyClosed")
	}
}

func TestServerDoubleServe(t *testing.T) {
	server := NewServer()
	errChan := make(chan error)

	go func() {
		errChan <- server.ListenAndServe(network, address,
			ConnHandlerFunc(func(s *Server, conn net.Conn) error { return nil }))

		server.Close()
	}()
	sleep(100)

	// Listens on another port
	err := server.ListenAndServe(network, host+":5556",
		ConnHandlerFunc(func(s *Server, conn net.Conn) error { return nil }))
	server.Close()

	err2 := <-errChan
	if err != ErrAlreadyServing && err2 != ErrAlreadyServing {
		t.Errorf(
			"Server.ListenAndServe shound return ErrAlreadyServing (%v, %v)",
			err, err2)
	}
}

// Serving multiple times client.Serve should fail.
func TestClientDoubleServe(t *testing.T) {
	server := NewServer()
	errChan := make(chan error)

	go server.ListenAndServe(network, address,
		ConnHandlerFunc(func(s *Server, conn net.Conn) error { return nil }))

	wait(t, nil)

	client, err := newClientTest(NewGobCodec)
	if err != nil {
		t.Fatalf("newClientTest failed: %s", err)
	}

	go func() {
		errChan <- client.Serve()

		// Close the conn, we don't close the client so the c.serving is not
		// set to false
		client.Conn.Close()
	}()
	sleep(100)

	err = client.Serve()

	if !client.serving {
		t.Errorf("Client.serving should be true")
	}

	// Close the conn, we don't close the client so the c.serving is not
	// set to false
	client.Conn.Close()

	err2 := <-errChan

	if err != ErrAlreadyServing && err2 != ErrAlreadyServing {
		t.Errorf("Client.Serve should return ErrAlreadyServing (%v, %v)", err, err2)
	}

	server.Close()
}

// Panic in ConnHandler
func TestConnHandlerPanic(t *testing.T) {
	server := NewServer()
	sync := make(chan struct{})

	server.AsyncError = func(s *Server, err error) {
		sync <- struct{}{}
	}

	go server.ListenAndServe(network, address,
		ConnHandlerFunc(func(s *Server, conn net.Conn) error {
			panic("PANIC")

			return nil
		}))

	wait(t, nil)

	client, err := newClientTest(NewGobCodec)
	if err != nil {
		t.Fatalf("newClientTest failed: %s", err)
	}

	select {
	case <-sync: // OK

	case <-time.After(100 * time.Millisecond): // KO
		t.Errorf("ConnHandler should trigger Server.AsyncError")
	}

	client.Close()
	server.Close()
}

// Closing multiple times client should fail.
func TestClientDoubleClose(t *testing.T) {
	server := NewServer()

	go server.ListenAndServe(network, address,
		ConnHandlerFunc(func(s *Server, conn net.Conn) error { return nil }))

	wait(t, nil)

	client, err := newClientTest(NewGobCodec)
	if err != nil {
		t.Fatalf("newClientTest failed: %s", err)
	}

	client.Close()

	err = client.Close()
	if err != ErrAlreadyClosed {
		t.Errorf("Double Cient.Close should return ErrAlreadyClosed")
	}

	server.Close()
}
