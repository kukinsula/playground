package event

import (
	"github.com/veandco/go-sdl2/sdl"
)

// TOUT DOUX !

type Controller interface {
	OnControllerAxisMove(*sdl.ControllerAxisEvent)
	OnControllerButton(*sdl.ControllerButtonEvent)
	OnControllerDevice(*sdl.ControllerDeviceEvent)
}

type ControllerHandler struct {
}
