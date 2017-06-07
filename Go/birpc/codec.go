package birpc

import (
	"fmt"
)

// Codec can Encode Requests and Responses and Decode into a Request or a
// Response in a specific format.
//
// Two peers communicating bidirectionally a Codec need be able to
// send a RPC Request or Response (WriteRequest and WriteResponse), to
// read a message's header and find whether it's a RPC Request or
// Response (ReadHeader), read a message's body (ReadRequestBody and
// ReadResponseBody).
type Codec interface {
	// WriteRequest writes a birpc Request: header and body.
	WriteRequest(req *Request, body interface{}) error

	// WriteResponse writes a birpc Response: header and body.
	WriteResponse(resp *Response, body interface{}) error

	// ReadHeader reads the birpc message header (Request or Response).
	ReadHeader(req *Request, resp *Response) error

	// ReadRequestBody reads a birpc Request body.
	ReadRequestBody(body interface{}) error

	// ReadResponseBody reads a birpc Response body.
	ReadResponseBody(body interface{}) error

	// Close closes the Codec.
	Close() error
}

// Request is a birpc request header.
type Request struct {
	Seq    uint64 // Sequence number chosen by client
	Method string // RPC method to call
}

// Response is a birpc response header.
type Response struct {
	Seq   uint64 // Sequence number chosen by client
	Error string // Error if any
}

// String return a string representation of Request.
func (r *Request) String() string {
	if r.Seq == notificationSeq {
		return fmt.Sprintf("Method:%s", r.Method)
	}

	return fmt.Sprintf("Seq:%d, Method:%s", r.Seq, r.Method)
}

// String return a string representation of Response.
func (r *Response) String() string {
	return fmt.Sprintf("Seq:%d, Error:%s", r.Seq, r.Error)
}
