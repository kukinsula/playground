package future

import (
	"io"
)

type ioChan struct {
	n   int
	err error
}

func FutureRead(r io.Reader, p []byte) func() (int, error) {
	done := make(chan ioChan)

	go func() {
		n, err := r.Read(p)

		done <- ioChan{n, err}
	}()

	return func() (int, error) {
		io := <-done

		return io.n, io.err
	}
}

func FutureWrite(w io.Writer, p []byte) func() (int, error) {
	done := make(chan ioChan)

	go func() {
		n, err := w.Write(p)

		done <- ioChan{n, err}
	}()

	return func() (int, error) {
		io := <-done

		return io.n, io.err
	}
}
