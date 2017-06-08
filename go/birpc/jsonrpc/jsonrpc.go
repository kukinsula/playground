package jsonrpc

import (
	"bufio"
	"encoding/json"
	"io"
	"sync"

	"github.com/kukinsula/birpc"
)

var Version1, Version2 version

func init() {
	Version1 = version("1.0")
	Version2 = version("2.0")
}

type Codec struct {
	enc     *json.Encoder // Encodes to JSON
	dec     *json.Decoder // Decodes from JSON
	buffer  *bufio.Writer // Encoder buffer
	closer  io.Closer     // Codec's Closer
	lock    sync.Mutex    // Protects buffer and msg
	msg     *message      // Current message handled
	version version       // Version of the Codec
}

type version string

type message struct {
	Version version `json:"jsonrpc"`
	Seq     uint64  `json:"id"`

	// Request
	Method string          `json:"method,omitempty"`
	Params json.RawMessage `json:"params,omitempty"`

	// Response
	Error  string          `json:"error,omitempty"`
	Result json.RawMessage `json:"result,omitempty"`
}

func New(rwc io.ReadWriteCloser) birpc.Codec {
	return NewWithVersion(rwc, Version1)
}

func NewWithVersion(rwc io.ReadWriteCloser, v version) birpc.Codec {
	buffer := bufio.NewWriter(rwc)

	return &Codec{
		dec:     json.NewDecoder(rwc),
		enc:     json.NewEncoder(buffer),
		buffer:  buffer,
		closer:  rwc,
		version: v,
	}
}

func (c *Codec) ReadHeader(req *birpc.Request, resp *birpc.Response) error {
	var msg message

	err := c.dec.Decode(&msg)
	if err != nil {
		return err
	}

	// TODO : check JSON RPC validity (en fonction de la version)

	if msg.Method != "" {
		req.Seq = msg.Seq
		req.Method = msg.Method
	} else {
		resp.Seq = msg.Seq
		resp.Error = msg.Error
	}

	// Lock is held until ReadRequestBody or ReadResponseBody
	c.lock.Lock()
	c.msg = &msg

	return nil
}

func (c *Codec) ReadRequestBody(body interface{}) error {
	// Lock is released so ReadHeader can handle new messages
	defer c.lock.Unlock()

	if body == nil {
		return nil
	}

	return json.Unmarshal(c.msg.Params, body)
}

func (c *Codec) ReadResponseBody(body interface{}) error {
	// Lock is released so ReadHeader can handle new messages
	defer c.lock.Unlock()

	if body == nil {
		return nil
	}

	return json.Unmarshal(c.msg.Result, body)
}

func (c *Codec) WriteRequest(req *birpc.Request, body interface{}) error {
	return c.encode(map[string]interface{}{
		"id":     req.Seq,
		"method": req.Method,
		"params": body,
	})
}

func (c *Codec) WriteResponse(resp *birpc.Response, body interface{}) error {
	return c.encode(map[string]interface{}{
		"id":     resp.Seq,
		"error":  resp.Error,
		"result": body,
	})
}

// TODO : prendre un message en paramètre plutôt qu'un map
func (c *Codec) encode(msg map[string]interface{}) error {
	c.lock.Lock() // TODO : utiliser un autre lock ???
	defer c.lock.Unlock()

	// JSON-RPC Version 2: http://www.jsonrpc.org/specification
	if c.version == Version2 {
		msg["jsonrpc"] = c.version
	}

	err := c.enc.Encode(msg)
	if err != nil {
		return err
	}

	return c.buffer.Flush()
}

func (c *Codec) Close() error {
	return c.closer.Close()
}
