package birpc

import (
	"bufio"
	"fmt"
	"io"

	ugorji "github.com/ugorji/go/codec"
)

const (
	JSON    = "Js"
	MSGPACK = "Mp"
	BINC    = "Bc"
	CBOR    = "Cb"
	GOB     = "Gb"
)

type Codec interface {
	ReadHeader(req *Request, resp *Response) error
	ReadRequestBody(params interface{}) error
	ReadResponseBody(result interface{}) error
	WriteRequest(req *Request, params interface{}) error
	WriteResponse(resp *Response, result interface{}) error
	Close() error
}

// type Codec interface {
// 	Encode(v interface{}) error
// 	Decode(v interface{}) error
// 	Close() error
// }

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

func (u *ugorgiCodec) Encode(v interface{}) error {
	return u.enc.Encode(v)
}

func (u *ugorgiCodec) Decode(v interface{}) error {
	return u.dec.Decode(v)
}

func (u *ugorgiCodec) ReadHeader(req *Request, resp *Response) error {
	var msg message

	err := u.dec.Decode(&msg)
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

func (u *ugorgiCodec) ReadRequestBody(params interface{}) error {
	return u.dec.Decode(params)
}

func (u *ugorgiCodec) ReadResponseBody(result interface{}) error {
	return u.dec.Decode(result)
}

func (u *ugorgiCodec) WriteRequest(req *Request, params interface{}) error {
	err := u.enc.Encode(req)
	if err != nil {
		return err
	}

	return u.dec.Decode(params)
}

func (u *ugorgiCodec) WriteResponse(resp *Response, result interface{}) error {
	err := u.enc.Encode(resp)
	if err != nil {
		return err
	}

	return u.dec.Decode(result)
}

func (u *ugorgiCodec) Close() error {
	return u.closer.Close()
}

func (b *BufferedCodec) Flush() error {
	return b.buf.Flush()
}

// func (b *BufferedCodec) EncodeAndFlush(v interface{}) error {
// 	err := b.Codec.Encode(v)
// 	if err != nil {
// 		return err
// 	}

// 	return b.Flush()
// }

func CreateBufferedCode(name string, rwc io.ReadWriteCloser, size int) (codec *BufferedCodec, err error) {
	switch name {
	case GOB:
		codec = NewGobBufferedCodec(rwc, size)

	case JSON, MSGPACK, BINC, CBOR:
		var handle ugorji.Handle

		handle, err = createUgorjiHandle(name)
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

func createUgorjiHandle(name string) (handle ugorji.Handle, err error) {
	switch name {
	case JSON:
		handle = new(ugorji.JsonHandle)
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
