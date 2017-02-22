package future

import (
	"io"
)

type ioResult struct {
	n   int
	err error
}

// FutureRead starts a goroutine reading b from the provided Reader and returns
// a function to call to retrieve the Read result when needed. FutureRead
// doesn't block waiting for Read to finish, it  only starts the reading.
func FutureRead(r io.Reader, b []byte) func() (int, error) {
	done := make(chan ioResult)

	go func() {
		n, err := r.Read(b)

		done <- ioResult{n, err}
	}()

	return func() (int, error) {
		res := <-done

		return res.n, res.err
	}
}

// FutureWrite starts a goroutine writing b in the provided Writer and returns a
// function to call to retrieve the Write result when needed. FutureWrite
// doesn't block waiting for the Write to finish, it only starts the writing.
func FutureWrite(w io.Writer, b []byte) func() (int, error) {
	done := make(chan ioResult)

	go func() {
		n, err := w.Write(b)

		done <- ioResult{n, err}
	}()

	return func() (int, error) {
		res := <-done

		return res.n, res.err
	}
}
