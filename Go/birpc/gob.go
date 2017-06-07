package birpc

import (
	"bufio"
	"encoding/gob"
	"io"
	"sync"
)

// Gob implements Codec using gob format.
type GobCodec struct {
	dec    *gob.Decoder
	enc    *gob.Encoder
	buffer *bufio.Writer
	closer io.Closer
	lock   sync.Mutex
}

// message holds either a Request or a Response. When a message is
// received we don't know yet if its a RPC Request or Response.
type message struct {
	Seq    uint64
	Method string
	Error  string
}

func NewGobCodec(rwc io.ReadWriteCloser) Codec {
	buffer := bufio.NewWriter(rwc)

	return &GobCodec{
		dec:    gob.NewDecoder(rwc),
		enc:    gob.NewEncoder(buffer),
		buffer: buffer,
		closer: rwc,
	}
}

func (g *GobCodec) ReadHeader(req *Request, resp *Response) error {
	var msg message

	err := g.dec.Decode(&msg)
	if err != nil {
		return err
	}

	if msg.Method != "" {
		req.Seq = msg.Seq
		req.Method = msg.Method
	} else {
		resp.Seq = msg.Seq
		resp.Error = msg.Error
	}

	return nil
}

func (g *GobCodec) ReadRequestBody(body interface{}) (err error) {
	if body == nil {
		tmp := 0
		g.dec.Decode(&tmp)

		return nil
	}

	return g.dec.Decode(body)
}

func (g *GobCodec) ReadResponseBody(body interface{}) (err error) {
	if body == nil {
		tmp := 0
		g.dec.Decode(&tmp)

		return nil
	}

	return g.dec.Decode(body)
}

func (g *GobCodec) WriteRequest(req *Request, body interface{}) error {
	return g.encode(req, body)
}

func (g *GobCodec) WriteResponse(resp *Response, body interface{}) error {
	return g.encode(resp, body)
}

func (g *GobCodec) encode(header, body interface{}) error {
	g.lock.Lock() // g.buffer protection
	defer g.lock.Unlock()

	err := g.enc.Encode(header)
	if err != nil {
		return err
	}

	if body != nil {
		err = g.enc.Encode(body)
		if err != nil {
			return err
		}
	} else {
		tmp := 0
		err = g.enc.Encode(tmp)
		if err != nil {
			return err
		}
	}

	return g.buffer.Flush()
}

func (g *GobCodec) Close() error {
	return g.closer.Close()
}
