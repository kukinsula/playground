package window

import (
	"github.com/veandco/go-sdl2/sdl"
)

type Window struct {
	*sdl.Window
	// *sdl.Renderer

	Scene *Scene
}

func NewWindow(title string, x, y, w, h int, flags uint32) (*Window, err) {
	window, err := sdl.CreateWindow(title, x, y, w, h, flags)
	if err != nil {
		return nil, err
	}

	return &Window{
		Window: window,
	}, nil
}
