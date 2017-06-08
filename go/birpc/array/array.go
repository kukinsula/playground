package array

// import (
// 	"bufio"
// 	"encoding/json"
// 	"errors"
// 	"io"
// 	"sync"

// 	"github.com/kukinsula/birpc"
// )

// type Codec struct {
// 	enc    *json.Encoder // Encodes to JSON
// 	dec    *json.Decoder // Decodes from JSON
// 	buffer *bufio.Writer
// 	closer io.ReadWriteCloser
// 	msg    []interface{}
// 	lock   sync.Mutex
// }

// func New(rwc io.ReadWriteCloser) birpc.Codec {
// 	buffer := bufio.NewWriter(rwc)

// 	return &Codec{
// 		dec:    json.NewDecoder(rwc),
// 		enc:    json.NewEncoder(buffer),
// 		buffer: buffer,
// 		closer: rwc,
// 	}
// }

// var (
// 	ErrInvalidMessage = errors.New("message invalid")
// 	ErrInvalidSeq     = errors.New("sequence number invalid")
// )

// func (c *Codec) ReadHeader(req *birpc.Request, resp *birpc.Response) error {
// 	var msg []interface{}

// 	err := c.dec.Decode(&msg)
// 	if err != nil {
// 		return err
// 	}

// 	switch len(msg) {
// 	case 2:

// 	case 3:
// 		seq, ok := msg[0].(uint64)
// 		if !ok {
// 			return ErrInvalidSeq
// 		}

// 		if seq&1 == 0 { // Even: message comes from the Client
// 			method, ok := msg[1].(string)
// 			if !ok {
// 				return ErrInvalidMessage
// 			}

// 			req.Seq = seq
// 			req.Method = method
// 		} else { // Odd: message comes from the Server
// 			error, ok := msg[1].(string)
// 			if !ok {
// 				return ErrInvalidMessage
// 			}

// 			resp.Seq = seq
// 			resp.Method = method
// 		}

// 	default:
// 		return ErrInvalidMessage
// 	}

// 	c.lock.Lock()
// 	c.msg = msg

// 	return nil
// }

// func (c *Codec) ReadRequestBody(body interface{}) error {
// 	defer c.lock.Unlock()

// 	if body == nil {
// 		return nil
// 	}

// 	return json.Unmarshal(c.msg[2], body)
// }

// func (c *Codec) ReadResponseBody(body interface{}) error {
// 	defer c.lock.Unlock()

// 	if body == nil {
// 		return nil
// 	}

// 	return json.Unmarshal(c.msg[2], body)
// }

// func (c *Codec) WriteRequest(req *birpc.Request, body interface{}) error {
// 	return c.encode([]interface{}{req.Seq, req.Method, body})
// }

// func (c *Codec) WriteResponse(resp *birpc.Response, body interface{}) error {
// 	return c.encode([]interface{}{resp.Seq, resp.Error, body})
// }

// func (c *Codec) encode(msg []interface{}) error {
// 	c.lock.Lock()
// 	defer c.lock.Unlock()

// 	err := c.enc.Encode(msg)
// 	if err != nil {
// 		return err
// 	}

// 	return c.buffer.Flush()
// }

// func (c *Codec) Close() error {
// 	return c.closer.Close()
// }
