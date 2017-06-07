package main

import (
	"flag"
	"fmt"
)

// 0   1   2   3
// 11  12  13  4
// 10  15  14  5
// 9   8   7   6

// 1 2 3 4 5 6 7 8 9 10

// 1
// 2
// 3
// 4
// 5
// 6
// 7
// 8
// 9
// 10

func main() {
	var width, height, size int
	var mute bool

	flag.IntVar(&width, "w", 4, "Number of columns")
	flag.IntVar(&height, "h", 4, "Number of rows")
	flag.IntVar(&size, "s", 4, "Number of states per goroutine")
	flag.BoolVar(&mute, "m", false, "of verbosity")

	flag.Parse()

	spiral := NewSpiral(width, height)

	spiral.GoRecursivelyDeeper(size)
	// spiral.LimitGoroutines(10, 10)

	if !mute {
		fmt.Print(spiral)
	}
}
