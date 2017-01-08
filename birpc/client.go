package birpc

import (
	"fmt"
	"net"
)

const (
	bufSize = 1024
)

type Client struct {
	BiRPC
	MessageBuilder
	conn     net.Conn
	services *ServiceSet
}

// Default Client's MessageBuilder.
// See NewClientWithCodecWithMessageBuilder.
type messageBuilder struct{}

type Message struct {
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

func NewClientWithCodec(conn net.Conn, codec Codec, services *ServiceSet) *Client {
	return newClient(conn, codec, services, &messageBuilder{})
}

func NewClientWithCodecWithMessageBuilder(conn net.Conn, codec Codec, services *ServiceSet, m MessageBuilder) *Client {
	return newClient(conn, codec, services, m)
}

func newClient(conn net.Conn, codec Codec, services *ServiceSet, m MessageBuilder) *Client {
	client := &Client{
		MessageBuilder: m,
		conn:           conn,
		services:       services,
	}

	InitBiRPC(&client.BiRPC, codec, client)
	client.Prefix = conn.LocalAddr().String()

	return client
}

func (c *Client) OnRequest(req *Request) (interface{}, error) {
	return c.services.Exec(req, c)
}

func (m *messageBuilder) MessageContainer() interface{} {
	return &Message{}
}

func (m *messageBuilder) BuildRequest(seq uint32, method string, params interface{}) interface{} {
	return &Message{
		Seq:    seq,
		Method: method,
		Params: params,
	}
}

func (m *messageBuilder) BuildResponse(seq uint32, result interface{}, err error) interface{} {
	return &Message{
		Seq:    seq,
		Result: result,
		Error:  err,
	}
}

func (c *Client) Parse(v interface{}) (req *Request, resp *Response, err error) {
	var msg Message

	switch m := v.(type) {
	case *Message:
		msg = *m
	case Message:
		msg = m

	default:
		err = fmt.Errorf(
			"Client.Parse expected Message type *Message, not %T", v, v)
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

func (m *Message) String() string {
	if m.Method != "" {
		return fmt.Sprintf("Request: %d, %s, %v",
			m.Seq, m.Method, m.Params)
	} else if m.Result != nil || m.Error != nil {

		return fmt.Sprintf("Response: %d, %v, %s",
			m.Seq, m.Result, m.Error)
	}

	return "invalid message"
}
