package birpc

type Request struct {
	seq    int
	Method string
	Params interface{}
}

type Response struct {
	seq    int
	Result interface{}
	Error  error
}

type Messager interface {
	EmptyMessage() Message
	OnRequest(req *Resquest) error
	BuildRequest(seq uint32, method string, params interface{}) *Request
}

type Message interface {
	Parse() error
	IsRequest() bool
	IsResponse() bool
	AsRequest() *Request
	AsResponse() *Response
}
