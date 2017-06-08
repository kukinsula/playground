package event

import (
	"sync"

	"github.com/veandco/go-sdl2/sdl"
)

type MouseListener interface {
	OnMouseMove(*sdl.MouseMotionEvent)
	OnMouseClick(*sdl.MouseButtonEvent)
	OnMouseWheel(*sdl.MouseWheelEvent)
}

type MouseHandler struct {
	lockMotion           sync.Mutex // protects mouseMotionListeners
	mouseMotionListeners []MouseListener

	lockButton           sync.Mutex // protects mouseButtonListeners
	mouseButtonListeners []MouseListener

	lockWheel           sync.Mutex // protects mouseWheelListeners
	mouseWheelListeners []MouseListener
}

func NewMouseHandler() *MouseHandler {
	return &MouseHandler{}
}

func (m *MouseHandler) AddMouseMotionListener(l MouseListener) {
	m.lockMotion.Lock()
	m.mouseMotionListeners = append(m.mouseMotionListeners, l)
	m.lockMotion.Unlock()
}

func (m *MouseHandler) AddMouseButtonListener(listener MouseListener) {
	m.lockButton.Lock()
	m.mouseButtonListeners = append(m.mouseButtonListeners, listener)
	m.lockButton.Unlock()
}

func (m *MouseHandler) AddMouseWheelListener(listener MouseListener) {
	m.lockWheel.Lock()
	m.mouseWheelListeners = append(m.mouseWheelListeners, listener)
	m.lockWheel.Unlock()
}

func (m *MouseHandler) NotifyMouseMotionListeners(mme *sdl.MouseMotionEvent) {
	m.lockMotion.Lock()

	for _, listener := range m.mouseMotionListeners {
		listener.OnMouseMove(mme)
	}

	m.lockMotion.Unlock()
}

func (m *MouseHandler) NotifyMouseButtonListeners(event *sdl.MouseButtonEvent) {
	m.lockButton.Lock()

	for _, listener := range m.mouseButtonListeners {
		listener.OnMouseClick(event)
	}

	m.lockButton.Unlock()
}

func (m *MouseHandler) NotifyMouseWheelListeners(event *sdl.MouseWheelEvent) {
	m.lockWheel.Lock()

	for _, listener := range m.mouseWheelListeners {
		listener.OnMouseWheel(event)
	}

	m.lockWheel.Unlock()
}
