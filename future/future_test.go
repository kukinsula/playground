package future

import (
	"bytes"
	"testing"
)

func TestFutureRead(t *testing.T) {
	b := make([]byte, 128)
	buf := bytes.NewBuffer(b)

	futureRead := FutureRead(buf, b)
	futureWrite := FutureWrite(buf, b)

	n, err := futureWrite()
	t.Log(n, err)

	n, err = futureRead()
	t.Log(n, err)
}
