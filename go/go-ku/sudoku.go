package main

import (
	"fmt"
	"sort"
	"strings"
)

const (
	size = 9
)

type Sudoku struct {
	cells [size][size]*Cell
}

func NewSudoku(cells [size][size]*Cell) *Sudoku {
	return &Sudoku{
		cells: cells,
	}
}

func (s *Sudoku) String() string {
	var str []string

	for line, v := range s.cells {
		for _, w := range v {
			str = append(str, w.String())
		}

		if line < size-1 {
			str = append(str, "\n")
		}
	}

	return strings.Join(str, " ")
}

func (s *Sudoku) IsCompletlyFilled() bool {
	for _, line := range s.cells {
		for _, cell := range line {
			if !cell.IsFilled() {
				return false
			}
		}
	}

	return true
}

func (s *Sudoku) IsFinished() bool {
	if !s.IsCompletlyFilled() {
		return false
	}

	for _, line := range s.cells {
		set := CellSet(line)

		if !set.IsFinished() {
			return false
		}
	}

	return true
}

type Cell struct {
	pos Position
	val uint8 // from 1 to 9
}

func NewCell(pos Position, val uint8) (*Cell, error) {
	if !IsValid(pos) {
		return nil, InvalidPositionErr(pos)
	}

	if val < 0 || val > 9 {
		return nil, InvalidValueErr(val)
	}

	return &Cell{
		pos: pos,
		val: val,
	}, nil
}

func (c *Cell) IsFilled() bool {
	return c.val != 0 // 0 means "empty cell"
}

func (c *Cell) String() string {
	return fmt.Sprintf("[%v, %d]", c.pos, c.val)
}

type CellSet [size]*Cell

func (c CellSet) Len() int           { return len(c) }
func (c CellSet) Less(i, j int) bool { return c[i].val < c[j].val }
func (c CellSet) Swap(i, j int)      { c[i], c[j] = c[j], c[i] }

func (c *CellSet) IsFinished() bool {
	sort.Sort(c)

	for i, cell := range c {
		if uint8(i) != cell.val {
			return false
		}
	}

	return true
}

type Position struct {
	x, y uint8 // from 0 to 8
}

func (p *Position) String() string {
	return fmt.Sprintf("(%d, %d)", p.x, p.y)
}

func NewPosition(x, y uint8) *Position {
	return &Position{
		x: x,
		y: y,
	}
}

func IsValid(pos Position) bool {
	if pos.x >= size || pos.y >= size {
		return false
	}

	return true
}

func InvalidPositionErr(pos Position) error {
	return fmt.Errorf("Invalid position %v", pos)
}

func InvalidValueErr(value uint8) error {
	return fmt.Errorf("Invalid value %v", value)
}
