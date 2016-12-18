package birpc

import (
	"errors"
	"io"
	"log"
	"net"
	"os"
	"sync"
)

var (
	logger  = log.New(os.Stdout, "", log.Ltime)
	verbose = true

	ErrServerStopped = errors.New("Server was stopped")
)

func SetVerbose(v bool) {
	verbose = v
}

func SetLogger(l *log.Logger) {
	logger = l
}

type Server struct {
	*ServiceSet

	listener net.Listener

	runningLock sync.RWMutex
	running     bool

	group sync.WaitGroup

	clientsLock sync.RWMutex
	clients     map[string]net.Conn

	stop chan struct{}

	Prefix string
}

func NewServerNetworkHostPort(network, host, port string) (*Server, error) {
	address := host + ":" + port

	listener, err := net.Listen(network, address)
	if err != nil {
		return nil, err
	}

	if verbose {
		logger.Printf("Server %s> starts listening", address)
	}

	return &Server{
		ServiceSet: NewServiceSet(),
		listener:   listener,
		running:    false,
		clients:    make(map[string]net.Conn),
		stop:       make(chan struct{}),
		Prefix:     address,
	}, nil
}

func NewTCPServer(host, port string) (*Server, error) {
	return NewServerNetworkHostPort("tcp", host, port)
}

func (s *Server) Start() {
	s.startHandler(s.ServeConn)
}

func (s *Server) StartHandler(handler func(conn net.Conn)) {
	s.startHandler(handler)
}

func (s *Server) startHandler(handler func(conn net.Conn)) {
	if s.isRunning() {
		panic("Server should not be started more than once")
	}

	if s.listener == nil {
		panic("Server was already stopped (see Reset)")
	}

	s.setRunning(true)

	for s.isRunning() {
		conn, err := s.listener.Accept()
		if err != nil {
			if !s.isRunning() { // Stop or Close was called
				err = ErrServerStopped
				break
			} else if err == io.EOF {
				s.setRunning(false)
				break
			}

			if verbose {
				logger.Printf("Server %s> Accept failed: %s", s.Prefix, err)
			}

			continue
		}

		if verbose {
			logger.Printf("Server %s> new incoming connection from %s",
				s.Prefix, conn.RemoteAddr())
		}

		s.group.Add(1)
		go func(conn net.Conn) {
			handler(conn)

			if verbose {
				logger.Printf("Server %s> connection from %s ended!",
					s.Prefix, conn.RemoteAddr())
			}

			conn.Close()
			s.group.Done()
		}(conn)
	}

	s.stop <- struct{}{}
}

func (s *Server) ServeConn(conn net.Conn) {
	s.ServeCodec(conn, NewGobBufferedCodec(conn, bufSize))
}

func (s *Server) ServeCodec(conn net.Conn, codec *BufferedCodec) {
	s.Register(conn)

	client := NewClientWithCodec(conn, codec, s.ServiceSet)
	client.Start()

	s.Unregister(conn)
}

func (s *Server) Close() error {
	s.setRunning(false)

	return s.listener.Close()
}

func (s *Server) Stop() {
	if verbose {
		logger.Printf("Server %s> stopping...", s.Prefix)
	}

	s.Close()
	<-s.stop
	s.listener = nil

	if verbose {
		logger.Printf("Server %s> stopped!", s.Prefix)
	}
}

// func (s *Server) Reset(network, host, port string) error {
// 	server, err := NewServerNetworkHostPort(network, host, port)
// 	if err != nil {
// 		return err
// 	}

// 	logger.Printf("XXXXX %#v\n", s)
// 	s = server
// 	logger.Printf("YYYYY %#v\n", s)

// 	return nil
// }

func (s *Server) Wait() {
	s.Stop()
	s.group.Wait()
}

func (s *Server) Interrupt() {
	done := make(chan struct{})

	go func() {
		s.Stop()

		done <- struct{}{}
	}()

	s.CloseAllClients()

	<-done
}

func (s *Server) CloseAllClients() {
	s.clientsLock.RLock()

	for _, conn := range s.clients {
		conn.Close()
	}

	s.clientsLock.RUnlock()
}

func (s *Server) isRunning() bool {
	s.runningLock.RLock()
	running := s.running
	s.runningLock.RUnlock()

	return running
}

func (s *Server) setRunning(running bool) {
	s.runningLock.Lock()
	s.running = running
	s.runningLock.Unlock()
}

func (s *Server) Register(conn net.Conn) {
	s.clientsLock.Lock()
	s.clients[conn.RemoteAddr().String()] = conn
	s.clientsLock.Unlock()
}

func (s *Server) Unregister(conn net.Conn) {
	s.clientsLock.Lock()
	delete(s.clients, conn.RemoteAddr().String())
	s.clientsLock.Unlock()
}
