package window

type Position struct {
	x, y int
}

func NewPosition(x, y int) *Position {
	return &Position{
		x: x,
		y: y,
	}
}
