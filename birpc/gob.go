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

func (g *GobCodec) Close() error {
	return g.closer.Close()
}
