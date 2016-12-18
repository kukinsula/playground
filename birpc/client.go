package birpc

import (
	"fmt"
	"net"
)

const (
	bufSize = 1024
)

type Client struct {
	*BiRPC

	conn     net.Conn
	services *ServiceSet
}

type message struct {
	Seq uint32 `json:"id"`

	Method string      `json:"method,omitempty"`
	Params interface{} `json:"params,omitempty"`

	Result interface{} `json:"result,omitempty"`
	Error  error       `json:"error,omitempty"`
}

func NewClient(conn net.Conn, services *ServiceSet) *Client {
	codec := NewGobBufferedCodec(conn, bufSize)

	return NewClientWithCodec(conn, codec, services)
}

func NewClientWithCodec(conn net.Conn, codec *BufferedCodec, services *ServiceSet) *Client {
	client := &Client{
		conn:     conn,
		services: services,
	}

	birpc := NewBiRPC(codec, client)
	birpc.Prefix = conn.LocalAddr().String()

	client.BiRPC = birpc

	return client
}

func (c *Client) EmptyMessage() interface{} {
	return &message{}
}

func (c *Client) OnRequest(req *Request) (interface{}, error) {
	return c.services.Exec(req, c)
}

func (c *Client) BuildRequest(seq uint32, method string, params interface{}) interface{} {
	return &message{
		Seq:    seq,
		Method: method,
		Params: params,
	}
}

func (c *Client) BuildResponse(seq uint32, result interface{}, err error) interface{} {
	return &message{
		Seq:    seq,
		Result: result,
		Error:  err,
	}
}

func (c *Client) Parse(v interface{}) (req *Request, resp *Response, err error) {
	var msg message

	switch m := v.(type) {
	case *message:
		msg = *m
	case message:
		msg = m

	default:
		err = fmt.Errorf(
			"Client.Parse expected message type *message, not %T", v, v)
		return
	}

	if msg.Method != "" {
		req = &Request{}

		req.Seq = msg.Seq
		req.Method = msg.Method
		req.Params = msg.Params
	} else if msg.Result != nil || msg.Error != nil {
		resp = &Response{}

		resp.Seq = msg.Seq
		resp.Result = msg.Result
		resp.Error = msg.Error
	} else {
		err = ErrInvalidMessage
	}

	return
}

func (m *message) Parse() (req *Request, resp *Response, err error) {
	if m.Method != "" {
		req.Seq = m.Seq
		req.Method = m.Method
		req.Params = m.Params
	} else if m.Result != nil || m.Error != nil {
		resp.Seq = m.Seq
		resp.Result = m.Result
		resp.Error = m.Error
	} else {
		err = ErrInvalidMessage
	}

	return
}

func (m *message) String() string {
	if m.Method != "" {
		return fmt.Sprintf("Request: %d, %s, %v",
			m.Seq, m.Method, m.Params)
	} else if m.Result != nil || m.Error != nil {

		return fmt.Sprintf("Response: %d, %v, %s",
			m.Seq, m.Result, m.Error)
	}

	return "invalid message"
}
