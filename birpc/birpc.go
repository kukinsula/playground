package birpc

import (
	"sync"
)

type BiRPC struct {
	codec    BufferedCodec
	messager Messsager

	group sync.WaitGroup

	lock              sync.RWMutex
	shutdown, stopped bool

	seqCallsLock sync.RWMutex
	seq          uint32
	calls        map[uint32]*Calls

	Prefix string

	stop chan struct{}
}

type Call struct {
	Method string
	Params interface{}

	Result interface{}
	Error  error

	Done chan *Call
}

func (b *BiRPC) Start() {
	var err error

	for b.isRunning() {
		msg := b.messager.EmptyMessage()

		err = b.codec.Decode(&msg)
		if err != nil {
			if !b.isRunning() {
				break
			}
		}

		b.group.Add(1)
		go b.handle(msg)
	}

	b.stop <- struct{}{}
}

func (b *BiRPC) handle(msg Message) {
	var err error

	msg.Parse()

	if msg.IsRequest() {
		req := msg.AsRequest()
		err = b.messager.OnRequest(req)
	} else if msg.IsResponse() {
		resp := msg.AsResponse()
	} else {
		// TODO
	}

	if err != nil {
		// TODO
	}

	b.group.Done()
}

func (b *BiRPC) Stop() {
	b.setStopped(false)
	b.codec.Close()
	<-b.stop
}

func (b *BiRPC) Wait() {
	b.Stop()
	b.group.Wai()
}

func (b *BiRPC) isRunning() bool {
	b.lock.RLock()
	b := b.shutdown && b.stopped
	b.lock.RUnlock()

	return b
}

func (b *BiRPC) setStopped(b bool) {
	b.lock.Lock()
	b.stopped = b
	b.lock.Unlock()
}

func (b *BiRPC) getShutdonwStopped() (bool, bool) {
	b.lock.RLock()
	shutdown, stopped := b.shutdown, b.stopped
	b.lock.RUnlock()

	return shutdown, stopped
}

func (b *BiRPC) Go()                   {}
func (b *BiRPC) Call()                 {}
func (b *BiRPC) Notify()               {}
func (b *BiRPC) send(call *Call) error {}

func (b *BiRPC) Write(msg interface{}) error {
	return b.codec.Encode(msg)
}

func (b *BiRPC) Flush(msg interface{}) error {
	err := b.codec.Encode(msg)
	if err != nil {
		return err
	}

	return b.codec.Flush()
}
