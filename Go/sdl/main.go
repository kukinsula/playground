package main

import (
	"fmt"

	"github.com/kukinsula/sdl/event"

	"github.com/veandco/go-sdl2/sdl"
)

func main() {
	sdl.Init(sdl.INIT_EVERYTHING)

	window, err := sdl.CreateWindow(
		"test",
		sdl.WINDOWPOS_UNDEFINED, sdl.WINDOWPOS_UNDEFINED,
		800, 600,
		sdl.WINDOW_SHOWN,
	)

	if err != nil {
		panic(err)
	}
	defer window.Destroy()

	surface, err := window.GetSurface()
	if err != nil {
		panic(err)
	}

	rect := sdl.Rect{0, 0, 200, 200}
	surface.FillRect(&rect, 0xffff0000)
	window.UpdateSurface()

	eventHandler := event.NewEventHandler()

	t := &Test{}
	eventHandler.AddMouseMotionListener(t)
	eventHandler.AddMouseButtonListener(t)
	eventHandler.AddMouseWheelListener(t)
	eventHandler.AddKeyPressListener(t)
	eventHandler.AddKeyReleaseListener(t)
	eventHandler.AddWindowListener(t)
	eventHandler.AddJoyAxisMotionListener(t)
	eventHandler.AddJoyBallMotionListener(t)
	eventHandler.AddJoyButtonListener(t)
	eventHandler.AddQuitListener(t)

	eventHandler.Run()

	sdl.Quit()
}

type Test struct{}

func (t *Test) OnMouseMove(event *sdl.MouseMotionEvent) {
	fmt.Printf("Test::OnMouseMove: %#v\n", event)
}

func (t *Test) OnMouseClick(event *sdl.MouseButtonEvent) {
	fmt.Printf("Test::OnMouseClick: %#v\n", event)
}

func (t *Test) OnMouseWheel(event *sdl.MouseWheelEvent) {
	fmt.Printf("Test::OnMouseWheel: %#v\n", event)
}

func (t *Test) OnQuit(event *sdl.QuitEvent) {
	fmt.Printf("Test::OnQuit: %#v\n", event)
}

func (t *Test) OnKeyPress(event *sdl.KeyDownEvent) {
	fmt.Printf("Test::OnKeyPress: %#v\n", event)
}

func (t *Test) OnKeyRelease(event *sdl.KeyUpEvent) {
	fmt.Printf("Test::OnKeyRelease: %#v\n", event)
}

func (t *Test) OnWindow(event *sdl.WindowEvent) {
	fmt.Printf("Test::OnWindow: %#v\n", event)
}

func (t *Test) OnJoyAxisMove(event *sdl.JoyAxisEvent) {
	fmt.Printf("Test::OnJoyAxisMove: %#v\n", event)
}

func (t *Test) OnJoyBallMove(event *sdl.JoyBallEvent) {
	fmt.Printf("Test::OnJoyBallMove: %#v\n", event)
}

func (t *Test) OnJoyButton(event *sdl.JoyButtonEvent) {
	fmt.Printf("Test::OnJoyButton: %#v\n", event)
}

func (t *Test) OnJoyHat(event *sdl.JoyHatEvent) {
	fmt.Printf("Test::OnJoyHat: %#v\n", event)
}

func (t *Test) OnJoyDevice(event *sdl.JoyDeviceEvent) {
	fmt.Printf("Test::OnJoyDevice: %#v\n", event)
}
