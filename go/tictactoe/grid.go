package tictactoe

import (
	"sync"
)

type Cell struct {
	Position
	Mark
	lock sync.RWMutex
}

func NewCell(x, y int, mark Mark) *Cell {
	return &Cell{
		Position: Position{x, y},
		Mark:     mark,
	}
}

func alignedCells(c1, c2, c3 *Cell) bool {
	return c1.Mark == c2.Mark &&
		c1.Mark == c3.Mark &&
		c1.Mark != EMPTY
}

type grid struct {
	cells [][]*Cell
	lock  sync.RWMutex
}

func new2DimensionalCellGrid(width, height int) [][]*Cell {
	cells := make([][]*Cell, height)

	for y := 0; y < height; y++ {
		cells[y] = make([]*Cell, width)

		for x := 0; x < width; x++ {
			cells[y][x] = NewCell(x, y, EMPTY)
		}
	}

	return cells
}

func (g *grid) Finished() (finished bool, win bool) {
	g.lock.RLock()
	defer g.lock.RUnlock()

	if g.win() {
		return true, true
	}

	if g.full() {
		return true, false
	}

	return false, false
}

func (g *grid) full() bool {
	for _, line := range g.cells {
		for _, cell := range line {
			if cell.Mark == EMPTY {
				return false // grid is not full
			}
		}
	}

	return true
}

// TODO : scale pour plus grand que 3*3
func (g *grid) win() (bool, *Result) {
	// Horizontal
	for y := 0; y < height; y++ {
		if alignedCells(g.cells[y][0], g.cells[y][1], g.cells[y][2]) {
			return true, &Result{
				cells: []*Cell{
					g.cells[y][0],
					g.cells[y][1],
					g.cells[y][2],
				},
			}
		}
	}

	// Vertical
	for x := 0; x < width; x++ {
		if alignedCells(g.cells[0][x], g.cells[1][x], g.cells[2][x]) {
			return true
		}
	}

	// Diagonal \
	if alignedCells(g.cells[0][0], g.cells[1][1], g.cells[2][2]) {
		return true
	}

	// Diagonal /
	if alignedCells(g.cells[0][2], g.cells[1][1], g.cells[2][0]) {
		return true
	}

	return false
}

func (g *grid) getCellMark(x, y int) Mark {
	g.lock.RLock()
	mark := g.cells[y][x].Mark
	g.lock.RUnlock()

	return mark
}

func (g *grid) setCellMark(x, y int, mark Mark) {
	g.lock.Lock()
	g.cells[y][x].Mark = mark
	g.lock.Unlock()
}

func (g *grid) String() string {
	var str string

	for _, line := range g.cells {
		for _, cell := range line {
			str += cell.Mark.String() + " "
		}

		str += "\n"
	}

	return str
}
