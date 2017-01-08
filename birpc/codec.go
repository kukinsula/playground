package birpc

import (
	"bufio"
	"fmt"
	"io"

	ugorji "github.com/ugorji/go/codec"
)

type SerializerType string

const (
	JSON    = SerializerType("JSON")
	MSGPACK = SerializerType("MSGPACK")
	BINC    = SerializerType("BINC")
	CBOR    = SerializerType("CBOR")
	GOB     = SerializerType("GOB")
)

type Codec interface {
	Encode(v interface{}) error
	Decode(v interface{}) error
	Close() error
}

type BufferedCodec struct {
	Codec
	buf *bufio.Writer
}

type ugorgiCodec struct {
	dec    *ugorji.Decoder
	enc    *ugorji.Encoder
	closer io.Closer
}

func NewBufferedCodec(codec Codec, buf *bufio.Writer) *BufferedCodec {
	return &BufferedCodec{
		Codec: codec,
		buf:   buf,
	}
}

func CreateBufferedCodec(name SerializerType, rwc io.ReadWriteCloser, size int) (codec *BufferedCodec, err error) {
	switch name {
	case GOB:
		gobCodec = NewGobBufferedCodec(rwc, size)

	case JSON, MSGPACK, BINC, CBOR:
		handle, err := createUgorjiHandle(name)
		if err != nil {
			break
		}

		buf := bufio.NewWriterSize(rwc, size)
		codec = NewBufferedCodec(&ugorgiCodec{
			enc:    ugorji.NewEncoder(buf, handle),
			dec:    ugorji.NewDecoder(rwc, handle),
			closer: rwc,
		}, buf)

	default:
		err = fmt.Errorf("codec named %s not handled", name)
	}

	return
}

func (b *BufferedCodec) Encode(v interface{}) error {
	err := b.Codec.Encode(v)
	if err != nil {
		return err
	}

	return b.Flush()
}

func (b *BufferedCodec) Decode(v interface{}) error {
	return b.Codec.Decode(v)
}

func (b *BufferedCodec) Flush() error {
	return b.buf.Flush()
}

// UGORJI

func (u *ugorgiCodec) Encode(v interface{}) error {
	return u.enc.Encode(v)
}

func (u *ugorgiCodec) Decode(v interface{}) error {
	return u.dec.Decode(v)
}

func (u *ugorgiCodec) Close() error {
	return u.closer.Close()
}

func createUgorjiHandle(name SerializerType) (handle ugorji.Handle, err error) {
	switch name {
	case JSON:
		jsHandle := new(ugorji.JsonHandle)
		jsHandle.StructToArray = true

		handle = jsHandle
	case MSGPACK:
		handle = new(ugorji.MsgpackHandle)
	case BINC:
		handle = new(ugorji.BincHandle)
	case CBOR:
		handle = new(ugorji.CborHandle)

	default:
		err = fmt.Errorf("codec named %s not handled", name)
	}

	return
}
