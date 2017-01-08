package birpc

import (
	"fmt"
)

type Messager interface {
	MessageBuilder
	Parse(v interface{}) (req *Request, resp *Response, err error)
	OnRequest(req *Request) (interface{}, error)
}

type MessageBuilder interface {
	MessageContainer() interface{}
	BuildRequest(seq uint32, method string, params interface{}) interface{}
	BuildResponse(seq uint32, result interface{}, err error) interface{}
	// BuildNotification(method string, params interface{}) interface{}
}

type Request struct {
	Seq    uint32
	Method string
	Params interface{}
}

type Response struct {
	Seq    uint32
	Result interface{}
	Error  error
}

func (r *Request) IsValid() bool {
	return r != nil
}

func (r *Request) String() string {
	if r.Seq == notificationSeq {
		return fmt.Sprintf("Notification{method=%s;params=%v}",
			r.Method, r.Params)
	}

	return fmt.Sprintf("Request{seq=%d;method=%s;params=%v}",
		r.Seq, r.Method, r.Params)
}

func (r *Response) IsValid() bool {
	return r != nil
}

func (r *Response) String() string {
	return fmt.Sprintf("Response{seq=%d;result=%v;error=%v}",
		r.Seq, r.Result, r.Error)
}
