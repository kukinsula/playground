package event

import (
	"fmt"

	"github.com/veandco/go-sdl2/sdl"
)

type EventHandler struct {
	*MouseHandler
	*KeyboardHandler
	*WindowHandler
	*JoystickHandler
	*QuitHandler

	running bool
}

func NewEventHandler() *EventHandler {
	return &EventHandler{
		running: false,

		MouseHandler:    NewMouseHandler(),
		KeyboardHandler: NewKeyboardHandler(),
		WindowHandler:   NewWindowHandler(),
		JoystickHandler: NewJoystickHandler(),
		QuitHandler:     NewQuitHandler(),
	}
}

func (e *EventHandler) Run() error {
	e.running = true

	var event sdl.Event

	for ; e.IsRunning(); event = sdl.WaitEvent() {
		switch t := event.(type) {
		case *sdl.QuitEvent:
			e.running = false
			go e.NotifyQuitListeners(t)
			break

		case *sdl.MouseMotionEvent:
			go e.NotifyMouseMotionListeners(t)

		case *sdl.MouseButtonEvent:
			go e.NotifyMouseButtonListeners(t)

		case *sdl.MouseWheelEvent:
			go e.NotifyMouseWheelListeners(t)

		case *sdl.KeyDownEvent:
			go e.NotifyKeyPressListeners(t)

		case *sdl.KeyUpEvent:
			go e.NotifyKeyReleaseListeners(t)

		case *sdl.JoyAxisEvent:
			go e.NotifyJoyAxisMotionListeners(t)

		case *sdl.JoyBallEvent:
			go e.NotifyJoyBallMotionListeners(t)

		case *sdl.JoyButtonEvent:
			go e.NotifyJoyButtonListeners(t)

		case *sdl.JoyHatEvent:
			go e.NotifyJoyHatListeners(t)

		case *sdl.JoyDeviceEvent:
			go e.NotifyJoyDeviceListeners(t)

		case *sdl.WindowEvent:
			go e.NotifyWindowListeners(t)

		case *sdl.TextInputEvent:
			fmt.Printf("TextInputEvent: %#v\n", t)

		default:
			fmt.Printf("Some event %T\n", t)
		}
	}

	return nil
}

func (e *EventHandler) setRunning(running bool) {
	e.running = running
}

func (e *EventHandler) IsRunning() bool {
	return e.running
}
