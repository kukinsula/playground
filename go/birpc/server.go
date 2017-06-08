package birpc

import (
	"errors"
	"fmt"
	"log"
	"net"
	"os"
	"sync"
)

var (
	ErrAlreadyServing           = errors.New("already serving")
	ErrAlreadyClosed            = errors.New("already closed")
	ErrShutdown                 = errors.New("connection shut down")
	ErrMethodNotSuitable        = errors.New("not a suitable method")
	ErrNoSuitableMethods        = errors.New("receiver has no suitable methods")
	ErrMethodNotFound           = errors.New("receiver's method not found")
	ErrServiceAlreadyRegistered = errors.New("service already exists")
	ErrServiceNotFound          = errors.New("service not found")
	ErrServiceInvalidReturn     = errors.New("service expects to return an error")
	ErrInsufficientChanCap      = errors.New("insufficient Done chan capacity")

	logger = log.New(os.Stdout, "", log.Ltime)
)

func SetLogger(l *log.Logger) {
	logger = l
}

// Server receives incoming connections and serves them.
type Server struct {
	AsyncError  func(s *Server, err error) // Async error callback
	serviceSet                             // Set of RPC Services
	listener    net.Listener               // Incoming listener
	lock        sync.RWMutex               // Protects serving
	serving     bool                       // Is the server serving
	clientsLock sync.RWMutex               // Protects clients
	clients     map[string]*Client         // Pending clients
	group       sync.WaitGroup             // Group of running gorutines
	close       chan struct{}              // Close synchronisation
}

// ConnHandler handles incoming connections.
type ConnHandler interface {
	HandleConn(s *Server, conn net.Conn) error
}

type ConnHandlerFunc func(s *Server, conn net.Conn) error

func (c ConnHandlerFunc) HandleConn(s *Server, conn net.Conn) error {
	return c(s, conn)
}

// NewServer returns a Server without a net.Listener.
func NewServer() *Server {
	return &Server{
		serviceSet: serviceSet{
			services: make(map[string]*service),
		},
		AsyncError: func(s *Server, err error) {
			logger.Printf("Server %s [ASYNC ERROR] %s",
				s.listener.Addr(), err)
		},
		clients: make(map[string]*Client),
		close:   make(chan struct{}),
	}
}

// ListenAndServe sets the Server's net.Listener and starts serving
// incoming connection with the provided ConnHandler.
func (s *Server) ListenAndServe(network, address string, handler ConnHandler) error {
	listener, err := net.Listen(network, address)
	if err != nil {
		return err
	}

	return s.serve(listener, handler)
}

// Serve starts accepting incoming connections and handle each
// of them in a goroutine with the provided handler.
func (s *Server) Serve(listener net.Listener, handler ConnHandler) error {
	return s.serve(listener, handler)
}

func (s *Server) ServeFunc(listener net.Listener,
	handler func(s *Server, conn net.Conn) error) error {

	return s.serve(listener, ConnHandlerFunc(handler))
}

// serve waits for incoming connections and handles each of them in a
// new goroutine with the provided conn handler function.
func (s *Server) serve(listener net.Listener, handler ConnHandler) error {
	s.lock.Lock()

	if s.serving { // serve should be called only once
		s.lock.Unlock()
		return ErrAlreadyServing
	}

	s.listener = listener
	s.serving = true
	s.lock.Unlock()

	// TODO : protection contre les panics (s.AsyncError par exemple)

	var err error
	var conn net.Conn

	logger.Printf("Server %s: starts accepting connections", s.listener.Addr())

	// Waits for incoming connections until Close is called
	for serving := true; serving; serving = s.Serving() {
		conn, err = s.listener.Accept()
		if err != nil {
			if !s.Serving() { // If Close was called
				err = ErrShutdown
				break
			}

			if s.AsyncError != nil {
				s.AsyncError(s, err)
			}

			continue
		}

		// Each connection is handled in a goroutine
		s.group.Add(1)
		go s.handleConn(conn, handler)
	}

	if !s.Serving() { // Close was called
		s.close <- struct{}{} // Syncs with Close
	}

	logger.Printf("Server %s: Serve finished", s.listener.Addr())

	return err
}

// handleConn
func (s *Server) handleConn(conn net.Conn, handler ConnHandler) {
	defer conn.Close()
	defer s.group.Done()

	defer func() { // Panic protection
		if r := recover(); r != nil && s.AsyncError != nil {
			s.AsyncError(s, fmt.Errorf(
				"%s: panic while handling connection %s\n%v",
				s.listener.Addr(), conn.RemoteAddr(), r))
		}
	}()

	logger.Printf("Server %s: new connection from %s",
		s.listener.Addr(), conn.RemoteAddr())

	err := handler.HandleConn(s, conn)
	if err != nil && s.AsyncError != nil {
		s.AsyncError(s, fmt.Errorf(
			"%s: handle connection %s failed: %s",
			s.listener.Addr(), conn.RemoteAddr(), err))
	}

	logger.Printf("Server %s: closing connection %s",
		s.listener.Addr(), conn.RemoteAddr())
}

// ServeClient a Server side Clinet. The Client is registered in the
// Server and it starts serving RPC messages.
func (s *Server) ServeClient(client *Client) error {
	if client.Conn == nil {
		panic("Server.ServeClient: Client's Conn is nil")
	}

	// Client subscription
	s.clientsLock.Lock()
	s.clients[client.Conn.RemoteAddr().String()] = client
	s.clientsLock.Unlock()

	// Client RPC Serve
	client.server = true
	client.prefix = "Client* " + client.Conn.RemoteAddr().String()
	client.serviceSet = s.serviceSet
	err := client.Serve()

	// Client unsubscription
	s.clientsLock.Lock()
	delete(s.clients, client.Conn.RemoteAddr().String())
	s.clientsLock.Unlock()

	return err
}

// ServeConn is like ServeClient with a Client using a GobCodec.
func (s *Server) ServeConn(conn net.Conn) error {
	return s.ServeClient(NewClientWithCodec(conn, NewGobCodec(conn)))
}

// Close closes the Server's net.Listener and stops serving.
func (s *Server) Close() error {
	s.lock.Lock()

	if !s.serving {
		s.lock.Unlock()
		return ErrAlreadyClosed
	}

	s.serving = false         // s.Serving() returns false now
	s.lock.Unlock()           // No need to protect serving anymore
	err := s.listener.Close() // Unblocks Accept
	<-s.close                 // Syncs with serve end

	return err
}

// Shutdown closes all the clients.
func (s *Server) Shutdown() {
	s.clientsLock.RLock()
	for _, client := range s.clients {
		client.Close()
	}
	s.clientsLock.RUnlock()
}

// Waits for all the connections handled in a goroutine to be
// finished.
func (s *Server) Wait() {
	s.group.Wait()
}

// Serving returns wether the Server is serving or not at that
// moment.
func (s *Server) Serving() bool {
	s.lock.RLock()
	serving := s.serving
	s.lock.RUnlock()

	return serving
}
