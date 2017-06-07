package main

type Widget struct {
	*Position
	*Dimension
}

func NewWidget(position *Position, dimension *Dimension) *Widget {
	return &Widget{
		Position:  position,
		Dimension: dimension,
	}
}
