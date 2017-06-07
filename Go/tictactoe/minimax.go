package tictactoe

// type MinimaxPlayer struct {
// 	mark Mark
// 	root *State
// }

// func NewMinimaxPlayer(mark Mark) *MinimaxPlayer {
// 	return &MinimaxPlayer{
// 		mark: mark,
// 	}
// }

// func (m *MinimaxPlayer) Play(ttt *TicTacToe) {
// 	if m.root == nil {
// 		m.root = NewState(ttt.cells)

// 		m.root.Eval()
// 	}
// }

// func (m *MinimaxPlayer) Mark() Mark { return m.mark }

// // TODO : renommer en node
// type State struct {
// 	cells        [][]*Cell
// 	root         *State
// 	sons         []*State
// 	value, level int
// }

// func NewState(cells [][]*Cell) *State {
// 	return &State{
// 		cells: cells,
// 		level: 0,
// 	}
// }

// func (s *State) Eval() {
// 	emptyCells := make([]*Cell, 0, height*width)

// 	for y := 0; y < height; y++ {
// 		for x := 0; x < height; x++ {
// 			if s.cells[y][x].Mark == EMPTY {
// 				emptyCells = append(emptyCells, s.cells[y][x])
// 			}
// 		}
// 	}

// 	s.sons = make([]*State, len(emptyCells))

// 	// for _, cell := range emptyCells {
// 	// 	son := &State{
// 	// 		root:  s,
// 	// 		level: s.level + 1,
// 	// 	}
// 	// }

// 	// for _, cell := range s.sons {

// 	// }
// }
