package tictactoe

import (
	"math/rand"
	"time"
)

func init() {
	rand.Seed(time.Now().Unix())
}

type Player interface {
	Play(ttt *TicTacToe) Position
	Mark() Mark
}

type RandomPlayer struct {
	mark Mark
}

func NewRandomPlayer(mark Mark) *RandomPlayer {

	return &RandomPlayer{
		mark: mark,
	}
}

func (r *RandomPlayer) Play(ttt *TicTacToe) (pos Position) {
	for {
		pos.X = rand.Intn(width)
		pos.Y = rand.Intn(height)

		if ttt.GetMark(&pos) == EMPTY {
			break
		}
	}

	return pos
}

func (r *RandomPlayer) Mark() Mark {
	return r.mark
}
