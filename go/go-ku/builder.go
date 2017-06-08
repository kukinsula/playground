package main

import (
	"errors"
)

type builderKind string

var (
	invalidSudokuBuilderKindErr = errors.New("invalid sudoku builder")
)

const (
	JSON = builderKind("JSON")
)

type Builder interface {
	Build() (*Sudoku, error)
}

func NewBuilder(kind builderKind) (Builder, error) {
	switch kind {
	case JSON:
		return nil, nil
	default:
		return nil, invalidSudokuBuilderKindErr
	}

	return nil, invalidSudokuBuilderKindErr
}
