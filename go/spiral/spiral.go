package main

import (
	"fmt"
	"sync"
)

type Spiral struct {
	width, height int
	array         []int
	state         *State
	lock          sync.RWMutex
}

type State struct {
	x, y          int
	width, height int
	value         int
}

func NewSpiral(width, height int) *Spiral {
	if width <= 0 || height <= 0 {
		panic("NewSpiral's width or height can't be negative")
	}

	return &Spiral{
		width:  width,
		height: height,
		array:  make([]int, width*height),
	}
}

func (s *Spiral) GetNextState() *State {
	var state State

	s.lock.Lock()
	defer s.lock.Unlock()

	// Initial State
	if s.state == nil {
		s.state = &State{
			x:      0,
			y:      0,
			width:  s.width,
			height: s.height,
			value:  0,
		}

		state = *s.state
		return &state
	}

	// Recursive State
	s.state.value += s.state.Perimeter()

	s.state.x++
	s.state.y++

	s.state.width -= 2
	s.state.height -= 2

	// Terminal State
	if s.state.width <= 0 || s.state.height <= 0 {
		return nil
	}

	state = *s.state
	return &state
}

func (s *Spiral) Fill(state *State) {
	x := state.x
	y := state.y

	value := state.value
	max := value + state.Perimeter()

	// Horizontal right
	for ; x < state.x+state.width; x++ {
		s.array[y*s.width+x] = value
		value++
	}

	if value >= max {
		return
	}

	x--
	y++

	// Vertical down
	for ; y < state.y+state.height; y++ {
		s.array[y*s.width+x] = value
		value++
	}

	if value >= max {
		return
	}

	y--
	x--

	// Horizontal left
	for ; x > state.x-1; x-- {
		s.array[y*s.width+x] = value
		value++
	}

	if value >= max {
		return
	}

	x++
	y--

	// Vertical up
	for ; y > state.y; y-- {
		s.array[y*s.width+x] = value
		value++
	}
}

func (s *Spiral) GoRecursivelyDeeper(size int) {
	var group sync.WaitGroup

	for done := false; !done; {
		states := make([]*State, 0, size)

		for i := 0; i < size; i++ {
			state := s.GetNextState()
			if state == nil {
				done = true
				break
			}

			states = append(states, state)
		}

		group.Add(1)
		go func(states []*State) {
			for i := 0; i < len(states); i++ {
				s.Fill(states[i])
			}

			group.Done()
		}(states)
	}

	group.Wait()
}

// func (s *Spiral) LimitGoroutines(max, size int) {
// 	tokens := make(chan struct{}, max)

// 	// Fill workers tokens
// 	for i := 0; i < max; i++ {
// 		tokens <- struct{}{}
// 	}

// 	for {
// 		//		states := make([]*State, 0, size)

// 		//	for i := 0; i < size; i++ {
// 		state := s.GetNextState()
// 		if state == nil {
// 			break
// 		}
// 		// }

// 		<-tokens

// 		go func(state *State) {
// 			s.Fill(state)
// 			tokens <- struct{}{}
// 		}(state)
// 	}

// 	// Wait all running goroutines
// 	for i := 0; i < len(tokens); i++ {
// 		<-tokens
// 	}
// }

func (s *Spiral) String() string {
	var str string

	for i := 0; i < s.height*s.width; i++ {
		if i != 0 && i%s.width == 0 {
			str += "\n"
		}

		str += fmt.Sprintf("%d ", s.array[i])
	}

	return str + "\n"
}

func (s *State) Perimeter() int {
	right := s.width
	down := s.height - 1
	left := s.width - 1
	up := s.height - 2

	if right-1 <= 0 {
		return s.height
	}

	if down <= 0 {
		return right
	}

	return right + down + left + up
}
