package tictactoe

type Position struct {
	X, Y int
}

type Mark int

const (
	EMPTY Mark = iota
	CROSS
	CIRCLE
)

func (s Mark) String() string {
	switch s {
	case CROSS:
		return "X"
	case CIRCLE:
		return "O"
	}

	return "-"
}

func (s Mark) Opposite() Mark {
	switch s {
	case CROSS:
		return CIRCLE
	case CIRCLE:
		return CROSS
	}

	return EMPTY
}
