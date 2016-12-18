package birpc

import (
	"bufio"
	"encoding/gob"
	"io"
	"sync"
)

type GobCodec struct {
	enc    *gob.Encoder
	dec    *gob.Decoder
	closer io.Closer
	lock   sync.Mutex
}

func NewGobBufferedCodec(rwc io.ReadWriteCloser, size int) *BufferedCodec {
	buf := bufio.NewWriterSize(rwc, size)
	codec := NewGobCodec(rwc)

	return NewBufferedCodec(codec, buf)
}

func NewGobCodec(rwc io.ReadWriteCloser) Codec {
	return &GobCodec{
		enc:    gob.NewEncoder(rwc),
		dec:    gob.NewDecoder(rwc),
		closer: rwc,
	}
}

func (g *GobCodec) Encode(v interface{}) error {
	return g.enc.Encode(v)
}

func (g *GobCodec) Decode(v interface{}) error {
	return g.dec.Decode(&v)
}

type gogMessage struct {
	Seq    uint32
	Method string
	Error  error
}

func (g *GobCodec) ReadHeader(req *Request, resp *Response) error {
	var msg gogMessage

	if err := g.dec.Decode(&msg); err != nil {
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

func (g *GobCodec) ReadRequestBody(body interface{}) error {
	return g.dec.Decode(body)
}

func (g *GobCodec) ReadResponseBody(body interface{}) error {
	return g.dec.Decode(body)
}

func (g *GobCodec) WriteRequest(r *Request, body interface{}) (err error) {
	g.lock.Lock()
	defer g.lock.Unlock()

	if err = g.enc.Encode(r); err != nil {
		return
	}

	return g.enc.Encode(body)
}

func (g *GobCodec) WriteResponse(r *Response, body interface{}) (err error) {
	g.lock.Lock()
	defer g.lock.Unlock()

	if err = g.enc.Encode(r); err != nil {
		return
	}

	return g.enc.Encode(body)
}

func (g *GobCodec) Close() error {
	return g.closer.Close()
}
