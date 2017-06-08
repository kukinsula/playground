package event

import (
	"sync"

	"github.com/veandco/go-sdl2/sdl"
)

type QuitListener interface {
	OnQuit(*sdl.QuitEvent)
}

type QuitHandler struct {
	lock          sync.Mutex // protects quitListeners
	quitListeners []QuitListener
}

func NewQuitHandler() *QuitHandler {
	return &QuitHandler{}
}

func (q *QuitHandler) AddQuitListener(listener QuitListener) {
	q.lock.Lock()
	q.quitListeners = append(q.quitListeners, listener)
	q.lock.Unlock()
}

func (q *QuitHandler) NotifyQuitListeners(event *sdl.QuitEvent) {
	q.lock.Lock()

	for _, listener := range q.quitListeners {
		listener.OnQuit(event)
	}

	q.lock.Unlock()
}
