package protocol

import (
	"encoding/gob"
	"fmt"
	"io"
	"time"

	"github.com/kukinsula/birpc"
	"github.com/kukinsula/birpc/jsonrpc"
)

func init() {
	gob.Register(Login{})
	gob.Register(Message{})
}

type Login struct {
	Login string `json:"login"`
	// TODO : Password string `json:"password"`
}

func (l *Login) String() string {
	return fmt.Sprintf("*Login{Login:%s}", l.Login)
}

type Message struct {
	From string      `json:"from"`
	To   string      `json:"to"`
	Code ReturnCode  `json:"code"`
	Data interface{} `json:"data"`
	Ts   time.Time   `json:"ts"`
}

func Msg(from, to string, code ReturnCode, data interface{}) Message {
	return Message{
		From: from,
		To:   to,
		Code: code,
		Data: data,
		Ts:   time.Now(),
	}
}

func MsgOK(from string) Message {
	return Message{
		From: from,
		Code: RC_OK,
		Ts:   time.Now(),
	}
}

func MsgBadRequest(from string, data interface{}) Message {
	return Message{
		From: from,
		Code: RC_BAD_REQUEST,
		Data: data,
		Ts:   time.Now(),
	}
}

func (m *Message) String() string {
	return fmt.Sprintf("*Message{From:%s, To:%s, Data:%v, Code:%s, Ts:%v}",
		m.From, m.To, m.Data, m.Code, m.Ts)
}

type ReturnCode int

var (
	RC_OK              = ReturnCode(200)
	RC_BAD_REQUEST     = ReturnCode(400)
	RC_UNAUTHORIZED    = ReturnCode(401)
	RC_NOT_FOUND       = ReturnCode(404)
	RC_INTERNAL_SERVER = ReturnCode(500)
)

func (r ReturnCode) String() string {
	switch r {
	case 0:
		return ""

	case RC_OK:
		return "OK"

	case RC_BAD_REQUEST:
		return "Bad Request"

	case RC_UNAUTHORIZED:
		return "Unauthorized"

	case RC_NOT_FOUND:
		return "Not Found"

	case RC_INTERNAL_SERVER:
		return "Internal Server Error"
	}

	return fmt.Sprintf("Unknown ReturnCode %d", r)
}

type Serializer string

var (
	GOB      = Serializer("gob")
	JSONRPC  = Serializer("jsonrpc")
	JSONRPC2 = Serializer("jsonrpc2")
)

func (s Serializer) Codec(rwc io.ReadWriteCloser) (codec birpc.Codec, err error) {
	switch string(s) {
	case string(GOB):
		codec = birpc.NewGobCodec(rwc)
	case string(JSONRPC):
		codec = jsonrpc.New(rwc)
	case string(JSONRPC2):
		codec = jsonrpc.NewWithVersion(rwc, jsonrpc.Version2)
	default:
		err = fmt.Errorf("invalid serializer '%s'", s)
	}

	return
}
