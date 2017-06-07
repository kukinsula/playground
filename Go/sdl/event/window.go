package event

import (
	"sync"

	"github.com/veandco/go-sdl2/sdl"
)

type WindowListener interface {
	OnWindow(*sdl.WindowEvent)
}

type WindowHandler struct {
	lock            sync.Mutex // protects quitListeners
	windowListeners []WindowListener
}

func NewWindowHandler() *WindowHandler {
	return &WindowHandler{}
}

func (w *WindowHandler) AddWindowListener(listener WindowListener) {
	w.lock.Lock()
	w.windowListeners = append(w.windowListeners, listener)
	w.lock.Unlock()
}

func (w *WindowHandler) NotifyWindowListeners(event *sdl.WindowEvent) {
	w.lock.Lock()

	for _, listener := range w.windowListeners {
		listener.OnWindow(event)
	}

	w.lock.Unlock()
}
