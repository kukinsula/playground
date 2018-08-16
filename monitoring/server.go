package main

import (
	"encoding/json"
	"fmt"
	"net/http"

	"github.com/kukinsula/playground/monitoring/metric"
)

// TODO route SSE overview

// TODO un type Handler par path : RootHandler { *Server }

var s *Server

type Server struct {
	Mux               *http.ServeMux
	incoming, leaving chan *subscription
	notify            chan []*message
	stop              chan struct{}
}

type subscription struct {
	subject string
	channel chan []byte
}

type message struct {
	subject string
	data    interface{}
}

func NewServer(config *metric.Config) (*Server, error) {
	server := &Server{
		Mux:      http.NewServeMux(),
		incoming: make(chan *subscription),
		leaving:  make(chan *subscription),
		notify:   make(chan []*message),
		stop:     make(chan struct{}),
	}

	server.Mux.HandleFunc("/", handleRoot)
	server.Mux.HandleFunc("/cpu", handleCPU)
	server.Mux.HandleFunc("/mem", handleMem)
	server.Mux.HandleFunc("/net", handleNet)

	s = server

	return server, nil
}

func (s *Server) Start() {
	go s.listen()
	go http.ListenAndServe(":8042", s.Mux)
}

func (s *Server) listen() {
	clients := make(map[string]map[*subscription]struct{})

	for {
		select {
		case sub := <-s.incoming:
			_, ok := clients[sub.subject]
			if !ok {
				clients[sub.subject] = make(map[*subscription]struct{})
			}

			clients[sub.subject][sub] = struct{}{}

		case sub := <-s.leaving:
			_, ok := clients[sub.subject]
			if !ok {
				break
			}

			delete(clients[sub.subject], sub)

		case messages := <-s.notify:
			for _, msg := range messages {
				b, err := json.Marshal(msg.data)
				if err != nil {
					// TODO
					continue
				}

				for sub := range clients[msg.subject] {
					sub.channel <- b
				}
			}

		case <-s.stop:
			goto done
		}
	}

done:
	for _, subs := range clients {
		for sub := range subs {
			close(sub.channel)
		}
	}
}

func (s *Server) Subscribe(subject string) *subscription {
	sub := &subscription{
		subject: subject,
		channel: make(chan []byte),
	}

	s.incoming <- sub

	return sub
}

func (s *Server) Unsubscribe(sub *subscription) {
	s.leaving <- sub
}

func (s *Server) Notify(messages []*message) {
	s.notify <- messages
}

func (s *Server) Stop() {
	s.stop <- struct{}{}
}

func handleRoot(w http.ResponseWriter, req *http.Request) {
	sseHandler(w, req, "overview")
}

func handleCPU(w http.ResponseWriter, req *http.Request) {
	sseHandler(w, req, "cpu")
}

func handleMem(w http.ResponseWriter, req *http.Request) {
	sseHandler(w, req, "mem")
}

func handleNet(w http.ResponseWriter, req *http.Request) {
	sseHandler(w, req, "net")
}

func sseHandler(w http.ResponseWriter, req *http.Request, name string) {
	// Make sure that the writer supports flushing
	flusher, ok := w.(http.Flusher)
	if !ok {
		http.Error(w, "Streaming unsupported!",
			http.StatusInternalServerError)

		return
	}

	sub := s.Subscribe(name)
	defer s.Unsubscribe(sub)

	w.Header().Set("content-type", "text/event-stream")
	w.Header().Set("Access-Control-Allow-Origin", "*")
	w.Header().Set("Cache-Control", "no-cache")
	w.Header().Set("Connection", "keep-alive")

	for id := int64(0); ; id++ {
		data, open := <-sub.channel
		if !open {
			return
		}

		_, err := fmt.Fprintf(w, `id: %d
data: %s

`, id, data)
		if err != nil {
			fmt.Printf("SSE Write failed: %s\n", err)
			http.Error(w, err.Error(), http.StatusInternalServerError)

			return
		}

		flusher.Flush()
	}
}

func serveJSON(v interface{}, w http.ResponseWriter, req *http.Request) {
	b, err := json.Marshal(v)
	if err != nil {
		// TODO

		return
	}

	w.Header().Set("content-type", "application/json")
	w.Header().Set("Access-Control-Allow-Origin", "*")

	_, err = w.Write(b)
	if err != nil {
		// TODO

		return
	}
}
