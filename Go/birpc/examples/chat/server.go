package main

import (
	"bufio"
	"context"
	"fmt"
	"log"
	"net"
	"os"
	"os/signal"
	"strings"
	"sync"

	"github.com/kukinsula/birpc"
	"github.com/kukinsula/birpc/examples/chat/protocol"
)

const (
	network = "tcp"
	ip      = "127.0.0.1"
	port    = "20000"
	address = ip + ":" + port

	separator = ";"
)

var (
	serializers = []string{"gob", "jsonrpc", "jsonrpc2"}
)

func main() {
	server := NewServer(ip, port)

	n, err := server.Register("chat", server)
	if err != nil {
		log.Printf("Register failed: %s", err)
		return
	}
	if n != 3 {
		log.Printf("Register server should have registered 3 services")
		return
	}

	go func() {
		err = server.ListenAndServe(network, address, server)
		if err != nil && err != birpc.ErrShutdown {
			log.Printf("ListenAndServe failed: %s", err)
			return
		}
	}()

	// Waits for CTRL-C
	interruptChan := make(chan os.Signal, 1)
	signal.Notify(interruptChan, os.Interrupt)
	<-interruptChan

	server.Shutdown()
}

type Server struct {
	*birpc.Server
	id       string
	lock     sync.RWMutex
	chatters map[string]*birpc.Client
}

func NewServer(ip, port string) *Server {
	return &Server{
		Server:   birpc.NewServer(),
		id:       ip + ":" + port,
		chatters: make(map[string]*birpc.Client),
	}
}

func (s *Server) HandleConn(_ *birpc.Server, conn net.Conn) error {
	buf := bufio.NewReaderSize(conn, 32)

	line, err := buf.ReadString('\n')
	if err != nil {
		return err
	}

	var codec birpc.Codec
	var serializer string

	serializers := strings.Split(line[:len(line)-1], separator)

	for _, s := range serializers {
		s = strings.TrimSpace(s)

		codec, err = protocol.Serializer(s).Codec(conn)
		if err != nil {
			log.Println(err)
			continue
		}

		serializer = s
		break
	}

	if codec == nil {
		return fmt.Errorf("invalid serializers %s", serializers)
	}

	log.Printf("Client* %s: using %s\n", conn.RemoteAddr(), serializer)

	_, err = conn.Write([]byte(serializer))
	if err != nil {
		return err
	}

	return s.ServeClient(birpc.NewClientWithCodec(conn, codec))
}

func (s *Server) Login(ctx context.Context,
	client *birpc.Client,
	params *protocol.Login,
	result *protocol.Message,
) error {

	s.lock.Lock()
	defer s.lock.Unlock()

	_, exists := s.chatters[params.Login]
	if exists {
		return fmt.Errorf("Login '%s' is already taken", params.Login)
	}

	s.chatters[params.Login] = client

	log.Printf("Chatter '%s' logged in!", params.Login)

	*result = protocol.MsgOK(s.id)

	return nil
}

func (s *Server) Logout(ctx context.Context,
	client *birpc.Client,
	params *protocol.Login,
	result *protocol.Message,
) error {

	s.lock.Lock()
	delete(s.chatters, params.Login)
	s.lock.Unlock()

	log.Printf("Chatter '%s' logged out!", params.Login)

	*result = protocol.MsgOK(s.id)

	return nil
}

func (s *Server) Message(ctx context.Context,
	client *birpc.Client,
	params *protocol.Message,
	result *protocol.Message,
) error {

	// TODO : cherchez dans ce serveur, si ça échoue on cherche dans le cluster

	s.lock.RLock()
	chatter, exists := s.chatters[params.To]
	s.lock.RUnlock()

	if !exists {
		return fmt.Errorf("Service Message can't find chatter '%s'", params.To)
	}

	return chatter.Call("chat.Message", params, result)
}
