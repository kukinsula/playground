package tictactoe

import (
	"fmt"
	"testing"
)

func TestTicTacToe(t *testing.T) {
	player1 := NewRandomPlayer(CROSS)
	player2 := NewRandomPlayer(CIRCLE)
	ttt := NewTicTacToe(player1, player2)

	res := ttt.Start()
	fmt.Println(res)
}
