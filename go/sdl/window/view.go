package main

type Drawer interface {
	Draw()
}

type Scene struct {
	widgets []*Widget
}

func NewScene() *Scene {
	return &Scene{}
}

func (s *Scene) Draw() {
	for _, widget := range widgets {
		widget.Draw()
	}
}
