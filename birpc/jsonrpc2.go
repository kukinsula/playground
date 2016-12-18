package birpc

// type JSONRPC2BufferedCodec struct {
// 	*ugorgiCodec
// }

// type JSONRPC2Message struct {
// 	Seq uint32 `json:"id"`

// 	Method string      `json:"method,omitempty"`
// 	Params interface{} `json:"params,omitempty"`

// 	Result interface{} `json:"result,omitempty"`
// 	Error  error       `json:"error,omitempty"`

// 	Version string `json:"version,omitempy"`
// }

// func (j *JSONRPC2Message) Parse() (req *Request, resp *Response, err error) {
// 	req, resp, err = j.message.Parse()
// 	if err != nil {
// 		return
// 	}

// 	j.Version = "2.0"

// 	return
// }
