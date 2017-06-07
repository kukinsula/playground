package birpc

// import (
// 	"bytes"
// 	"testing"
// )

// type rwcMock struct {
// 	*bytes.Buffer
// }

// func (r *rwcMock) Close() error {
// 	return nil
// }

// func TestGobCodec(t *testing.T) {
// 	rwc := &rwcMock{bytes.NewBuffer(make([]byte, 64))}
// 	codec := NewGobCodec(rwc)

// 	method := "foo"
// 	seq := uint64(18)
// 	param := 42
// 	result := 24

// 	req := &Request{Seq: seq, Method: method}
// 	resp := &Response{Seq: seq}

// 	err := codec.WriteRequest(req, param)
// 	if err != nil {
// 		t.Errorf("WriteRequest failed: %s", err)
// 	}

// 	logger.Printf("XXXXXXXXXXXX % x", rwc.Buffer.Bytes())

// 	err = codec.ReadHeader(req, resp)
// 	if err != nil {
// 		t.Errorf("ReadHeader failed: %s", err)
// 	}

// 	if req.Method != method {
// 		t.Errorf("Expected method '%s' not '%s'", method, req.Method)
// 	}

// 	if req.Seq != seq {
// 		t.Errorf("Expected seq %d not %d", seq, req.Seq)
// 	}

// 	logger.Println(result)
// }
