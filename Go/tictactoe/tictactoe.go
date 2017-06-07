package tictactoe

import (
	"fmt"
)

const (
	width  = 3
	height = 3
)

type TicTacToe struct {
	grid
	Player1, Player2 Player
}

func NewTicTacToe(player1, player2 Player) *TicTacToe {
	return &TicTacToe{
		grid: grid{
			cells: new2DimensionalCellGrid(width, height),
		},
		Player1: player1,
		Player2: player2,
	}
}

func (ttt *TicTacToe) Start() *Result {
	res := Result{}

	for {
		for {
			pos := ttt.Player1.Play(ttt)
			err := ttt.setMark(&pos, ttt.Player1.Mark())
			if err == nil {
				break
			}

			fmt.Println(err)
		}

		fmt.Printf("%s\n\n", ttt)

		if finished, win := ttt.Finished(); finished {
			if win {
				res.Winner = ttt.Player1
				res.Loser = ttt.Player2
			}

			break
		}

		for {
			pos := ttt.Player2.Play(ttt)
			err := ttt.setMark(&pos, ttt.Player2.Mark())
			if err == nil {
				break
			}

			fmt.Println(err)
		}

		fmt.Printf("%s\n\n", ttt)

		if finished, win := ttt.Finished(); finished {
			if win {
				res.Winner = ttt.Player2
				res.Loser = ttt.Player1
			}

			break
		}
	}

	return &res
}

func (ttt *TicTacToe) setMark(pos *Position, mark Mark) error {
	if mark := ttt.getCellMark(pos.X, pos.Y); mark != EMPTY {
		return fmt.Errorf(
			"Cell at [%d;%d] is already accupied by %s",
			pos.Y, pos.X, mark)
	}

	ttt.setCellMark(pos.X, pos.Y, mark)

	return nil
}

func (ttt *TicTacToe) GetMark(pos *Position) Mark {
	return ttt.getCellMark(pos.X, pos.Y)
}

type Result struct {
	Winner, Loser Player
	finished, win bool
	cells         []*Cell
}

func (r *Result) String() string {
	if r.Winner == nil {
		return "No winner"
	}

	return fmt.Sprintf("Winner is '%s'\n", r.Winner.Mark())
}
