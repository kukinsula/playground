package birpc

const (
	JSON    = "Js"
	MSGPACK = "Mp"
	BINC    = "Bc"
	CBOR    = "Cb"
)

type Codec interface {
	Encode(v interface{}) error
	Decde(v interface{}) error
	Close() error
}

type BufferedCodec struct {
	Codec
	buf *bufio.Writer
}

func (b *BufferedCodec) Flush() error {
	return b.buf.Flush()
}

func CreateBufferedCode(rwc io.ReadWriteCloser, size int) (BufferedCodec, error) {}
