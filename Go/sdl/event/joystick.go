package event

import (
	"sync"

	"github.com/veandco/go-sdl2/sdl"
)

type JoystickListener interface {
	OnJoyAxisMove(*sdl.JoyAxisEvent)
	OnJoyBallMove(*sdl.JoyBallEvent)
	OnJoyButton(*sdl.JoyButtonEvent)
	OnJoyHat(*sdl.JoyHatEvent)
	OnJoyDevice(*sdl.JoyDeviceEvent)
}

type JoystickHandler struct {
	lockAxis               sync.Mutex // protects joyAxisMotionListeners
	joyAxisMotionListeners []JoystickListener

	lockBall               sync.Mutex // protects joyBallMotionListeners
	joyBallMotionListeners []JoystickListener

	lockButton         sync.Mutex // protects joyButtonListeners
	joyButtonListeners []JoystickListener

	lockHat         sync.Mutex // protects joyHatListeners
	joyHatListeners []JoystickListener

	lockDevice         sync.Mutex // protects joyDeviceListeners
	joyDeviceListeners []JoystickListener
}

func NewJoystickHandler() *JoystickHandler {
	return &JoystickHandler{}
}

func (j *JoystickHandler) AddJoyAxisMotionListener(listener JoystickListener) {
	j.lockAxis.Lock()
	j.joyAxisMotionListeners = append(j.joyAxisMotionListeners, listener)
	j.lockAxis.Unlock()
}

func (j *JoystickHandler) AddJoyBallMotionListener(listener JoystickListener) {
	j.lockBall.Lock()
	j.joyAxisMotionListeners = append(j.joyAxisMotionListeners, listener)
	j.lockBall.Unlock()
}

func (j *JoystickHandler) AddJoyButtonListener(listener JoystickListener) {
	j.lockButton.Lock()
	j.joyButtonListeners = append(j.joyButtonListeners, listener)
	j.lockButton.Unlock()
}

func (j *JoystickHandler) AddJoyHatListener(listener JoystickListener) {
	j.lockHat.Lock()
	j.joyHatListeners = append(j.joyHatListeners, listener)
	j.lockHat.Unlock()
}

func (j *JoystickHandler) AddJoyDeviceListener(listener JoystickListener) {
	j.lockDevice.Lock()
	j.joyDeviceListeners = append(j.joyDeviceListeners, listener)
	j.lockDevice.Unlock()
}

func (j *JoystickHandler) NotifyJoyAxisMotionListeners(event *sdl.JoyAxisEvent) {
	j.lockAxis.Lock()

	for _, listener := range j.joyAxisMotionListeners {
		listener.OnJoyAxisMove(event)
	}

	j.lockAxis.Unlock()
}

func (j *JoystickHandler) NotifyJoyBallMotionListeners(event *sdl.JoyBallEvent) {
	j.lockBall.Lock()

	for _, listener := range j.joyBallMotionListeners {
		listener.OnJoyBallMove(event)
	}

	j.lockBall.Unlock()
}

func (j *JoystickHandler) NotifyJoyButtonListeners(event *sdl.JoyButtonEvent) {
	j.lockButton.Lock()

	for _, listener := range j.joyButtonListeners {
		listener.OnJoyButton(event)
	}

	j.lockButton.Unlock()
}

func (j *JoystickHandler) NotifyJoyHatListeners(event *sdl.JoyHatEvent) {
	j.lockHat.Lock()

	for _, listener := range j.joyHatListeners {
		listener.OnJoyHat(event)
	}

	j.lockHat.Unlock()
}

func (j *JoystickHandler) NotifyJoyDeviceListeners(event *sdl.JoyDeviceEvent) {
	j.lockDevice.Lock()

	for _, listener := range j.joyDeviceListeners {
		listener.OnJoyDevice(event)
	}

	j.lockDevice.Unlock()
}
