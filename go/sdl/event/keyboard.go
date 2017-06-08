package event

import (
	"sync"

	"github.com/veandco/go-sdl2/sdl"
)

type KeyboardListener interface {
	OnKeyPress(*sdl.KeyDownEvent)
	OnKeyRelease(*sdl.KeyUpEvent)
}

type KeyboardHandler struct {
	lockPress         sync.Mutex // protects keyPressListeners
	keyPressListeners []KeyboardListener

	lockRelease         sync.Mutex // protects keyReleaseListeners
	keyReleaseListeners []KeyboardListener
}

func NewKeyboardHandler() *KeyboardHandler {
	return &KeyboardHandler{}
}

func (k *KeyboardHandler) AddKeyPressListener(listener KeyboardListener) {
	k.lockPress.Lock()
	k.keyPressListeners = append(k.keyPressListeners, listener)
	k.lockPress.Unlock()
}

func (k *KeyboardHandler) AddKeyReleaseListener(listener KeyboardListener) {
	k.lockRelease.Lock()
	k.keyReleaseListeners = append(k.keyReleaseListeners, listener)
	k.lockRelease.Unlock()
}

func (k *KeyboardHandler) NotifyKeyPressListeners(event *sdl.KeyDownEvent) {
	k.lockPress.Lock()

	for _, listener := range k.keyPressListeners {
		listener.OnKeyPress(event)
	}

	k.lockPress.Unlock()
}

func (k *KeyboardHandler) NotifyKeyReleaseListeners(event *sdl.KeyUpEvent) {
	k.lockRelease.Lock()

	for _, listener := range k.keyReleaseListeners {
		listener.OnKeyRelease(event)
	}

	k.lockRelease.Unlock()
}
