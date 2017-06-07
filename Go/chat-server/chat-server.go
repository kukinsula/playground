package main

import (
	"fmt"
	"io"
	"net"
)

var bufSize = 1024

type Server struct {
	listener   net.Listener
	operations chan func(map[net.Addr]net.Conn)
}

func main() {
	server, err := NewServer("127.0.0.1:6666")
	if err != nil {
		fmt.Println(err)
		return
	}

	server.Run()
}

func NewServer(address string) (*Server, error) {
	listener, err := net.Listen("tcp", address)
	if err != nil {
		return nil, err
	}

	server := &Server{
		listener:   listener,
		operations: make(chan func(map[net.Addr]net.Conn)),
	}

	go server.monitoring()

	return server, nil
}

func (s *Server) monitoring() {
	conns := make(map[net.Addr]net.Conn)

	for op := range s.operations {
		op(conns)
	}
}

func (s *Server) Run() {
	defer s.listener.Close()

	for {
		conn, err := s.listener.Accept()
		if err != nil {
			fmt.Println(err)
			continue
		}

		go s.handleConn(conn)
	}
}

func (s *Server) handleConn(conn net.Conn) {
	defer func() {
		s.Remove(conn)
		conn.Close()
	}()

	fmt.Println("New connection from %s", conn.RemoteAddr())

	s.Add(conn)

	b := make([]byte, bufSize)

	for {
		_, err := conn.Read(b)
		if err != nil {
			fmt.Println(err)
			return
		}

		s.SendAll(string(b))
	}
}

func (s *Server) Add(conn net.Conn) {
	s.operations <- func(conns map[net.Addr]net.Conn) {
		conns[conn.RemoteAddr()] = conn
	}
}

func (s *Server) Remove(conn net.Conn) {
	s.operations <- func(conns map[net.Addr]net.Conn) {
		delete(conns, conn.RemoteAddr())
	}
}

func (s *Server) SendAll(msg string) error {
	errorChan := make(chan error)

	s.operations <- func(conns map[net.Addr]net.Conn) {
		var err error

		for _, conn := range conns {
			_, err = io.WriteString(conn, msg)
			if err != nil {
				break
			}
		}

		errorChan <- err
	}

	return <-errorChan
}

func (s *Server) SendTo(addr net.Addr, msg string) error {
	errorChan := make(chan error)

	s.operations <- func(conns map[net.Addr]net.Conn) {
		conn, ok := conns[addr]
		if !ok {
			errorChan <- fmt.Errorf("%s not found", addr)
		}

		_, err := io.WriteString(conn, msg)
		errorChan <- err
	}

	return <-errorChan
}
