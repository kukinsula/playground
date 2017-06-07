package main

import (
	"testing"
)

var (
	validTests = []struct {
		spiral   *Spiral
		expected []int
	}{
		// Squares

		{
			spiral:   NewSpiral(1, 1),
			expected: []int{0},
		},

		{
			spiral: NewSpiral(2, 2),
			expected: []int{
				0, 1,
				3, 2,
			},
		},

		{
			spiral: NewSpiral(3, 3),
			expected: []int{
				0, 1, 2,
				7, 8, 3,
				6, 5, 4,
			},
		},

		// Lines

		{
			spiral: NewSpiral(3, 1),
			expected: []int{
				0, 1, 2,
			},
		},

		{
			spiral: NewSpiral(1, 3),
			expected: []int{
				0, 1, 2,
			},
		},

		// Rectangles

		{
			spiral: NewSpiral(4, 5),
			expected: []int{
				0, 1, 2, 3,
				13, 14, 15, 4,
				12, 19, 16, 5,
				11, 18, 17, 6,
				10, 9, 8, 7,
			},
		},

		{
			spiral: NewSpiral(5, 4),
			expected: []int{
				0, 1, 2, 3, 4,
				13, 14, 15, 16, 5,
				12, 19, 18, 17, 6,
				11, 10, 9, 8, 7,
			},
		},
	}
)

func TestSpiral(t *testing.T) {
	for i, test := range validTests {
		test.spiral.GoRecursivelyDeeper()

		if !equalArrays(test.spiral.array, test.expected) {
			t.Errorf(
				"Test #%d failed\nResult:  %v\nExpected:%v\n",
				i, test.spiral.array, test.expected)
		}
	}
}

func equalArrays(a, b []int) bool {
	if len(a) != len(b) {
		return false
	}

	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}

	return true
}
