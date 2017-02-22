package future

import (
	"bytes"
	"testing"
)

func TestFutureRead(t *testing.T) {
	str := "AZERTY"
	buf := bytes.NewBufferString(str)
	b := buf.Bytes()

	expextedNWrite := len(str)
	expextedNRead := len(str)

	futureRead := FutureRead(buf, b)
	futureWrite := FutureWrite(buf, b)

	n, err := futureWrite()
	if err != nil {
		t.Errorf("FutureWrite failed: %s", err)
	}

	if n != expextedNWrite {
		t.Errorf("FutureWrite returned %d but expexted to %d", n, expextedNWrite)
	}

	n, err = futureRead()
	if err != nil {
		t.Errorf("FutureRead failed: %s", err)
	}

	if n != expextedNRead {
		t.Errorf("FutureRead returned %d but expexted to %d", n, expextedNRead)
	}
}
