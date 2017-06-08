package main

import (
	"bufio"
	"encoding/json"
	"fmt"
	"io"
)

type JSONRepresentation struct {
	Name  string             `json:"name"`
	Level string             `json:"level"`
	Grid  [size][size]string `json:"grid"`
}

type JSONBuilder struct {
	*json.Decoder
}

func NewJSONBuilder(r io.Reader) *JSONBuilder {
	br := bufio.NewReader(r)

	return &JSONBuilder{
		Decoder: json.NewDecoder(br),
	}
}

func (j *JSONBuilder) Build() (*Sudoku, error) {
	var v JSONRepresentation
	var value int

	err := j.Decode(&v)
	if err != nil {
		return nil, err
	}

	cells := [size][size]*Cell{{}}

	for i, line := range v.Grid {
		for j, val := range line {
			pos := NewPosition(uint8(j), uint8(i))
			n, err := fmt.Sscanf(val, "%d", &value)
			if err != nil || n != 1 {
				return nil, err
			}

			cell, err := NewCell(*pos, uint8(value))
			if err != nil {
				return nil, err
			}

			cells[i][j] = cell
		}
	}

	return NewSudoku(cells), nil
}
