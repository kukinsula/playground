package birpc

import (
	"errors"
	"math"
	"sync"
)

var (
	ErrBiRPCStopped   = errors.New("birpc was stopped")
	ErrCanceledCall   = errors.New("call was canceled")
	ErrInvalidMessage = errors.New("invalid message")
)

const (
	notificationSeq uint32 = 0
	startSeq        uint32 = 1
	maxSeq          uint32 = math.MaxUint32
)

type Config struct {
	// TODO
}

type BiRPC struct {
	codec    *BufferedCodec
	messager Messager

	group sync.WaitGroup

	lock              sync.RWMutex
	shutdown, stopped bool

	callsLock sync.RWMutex
	seq       uint32
	calls     map[uint32]*Call

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

func NewBiRPC(codec *BufferedCodec, m Messager) *BiRPC {
	return &BiRPC{
		codec:    codec,
		messager: m,
		seq:      startSeq,
		calls:    make(map[uint32]*Call),
		stop:     make(chan struct{}, 1),
	}
}

func (b *BiRPC) Start() {
	var err error

	b.setRunning()

	if verbose {
		logger.Printf("%s> starts birpc", b.Prefix)
	}

	for b.isRunning() {
		// msg := b.messager.EmptyMessage()
		// var msg interface{}

		req := Request{}
		resp := Response{}

		err = b.codec.ReadHeader(&req, &resp)
		if err != nil {
			_, stopped := b.getShutdonwStopped()
			if stopped {
				err = ErrBiRPCStopped
			} else if verbose {
				logger.Printf("%s> decode message failed: %s", b.Prefix, err)
				b.setShutdown(true)
			}

			break
		}

		b.group.Add(1)
		go b.handle(&req, &resp)
	}

	b.cancelPendingCalls()

	if verbose {
		logger.Printf("%s> birpc finished (err: %s)", b.Prefix, err)
	}

	b.stop <- struct{}{}
}

func (b *BiRPC) handle(req *Request, resp *Response) {
	var err error

	if req.Method != "" { // Message received is a request
		if verbose {
			logger.Printf("%s> <- %s\n", b.Prefix, req)
		}

		result, err := b.messager.OnRequest(req)

		if req.Seq != notificationSeq { // If not a notification
			resp.Seq = req.Seq
			resp.Result = result
			resp.Error = err

			err = b.codec.WriteResponse(resp, result)
			if err != nil {
				logger.Printf("%s> WriteResponse failed: %s", b.Prefix, err)
			} else if verbose {
				logger.Printf("%s> -> %s\n", b.Prefix, resp)
			}

			err = b.codec.Flush()
			if err != nil {
				logger.Printf("%s> Flush failed: %s", b.Prefix, err)
			}
		}
	} else { // Message received is a response
		if verbose {
			logger.Printf("%s> <- %s\n", b.Prefix, resp)
		}

		call := b.unregisterCall(resp.Seq)

		switch {
		case call == nil:
			if verbose {
				logger.Println("%s> Call #%d not found", b.Prefix, resp.Seq)
			}

			err = b.codec.ReadResponseBody(nil)
			if err != nil {
				logger.Println("%s> ReadResponseBody failed: %s", b.Prefix, err)
			}

		case resp.Error != nil:
			call.Error = resp.Error

			err = b.codec.ReadResponseBody(nil)
			if err != nil {
				logger.Println("%s> ReadResponseBody failed: %s", b.Prefix, err)
			}

			call.done()

		default:
			err = b.codec.ReadResponseBody(call.Result)
			if err != nil {
				logger.Println("%s> ReadResponseBody failed: %s", err)
			}

			call.done()
		}
	}
}

func (b *BiRPC) Stop() {
	b.setStopped(false)
	b.codec.Close()
	<-b.stop
}

func (b *BiRPC) Wait() {
	b.Stop()
	b.group.Wait()
}

func (b *BiRPC) isRunning() bool {
	b.lock.RLock()
	running := !b.shutdown && !b.stopped
	b.lock.RUnlock()

	return running
}

func (b *BiRPC) setStopped(stopped bool) {
	b.lock.Lock()
	b.stopped = stopped
	b.lock.Unlock()
}

func (b *BiRPC) setShutdown(shutdown bool) {
	b.lock.Lock()
	b.shutdown = shutdown
	b.lock.Unlock()
}

func (b *BiRPC) setRunning() {
	b.lock.Lock()
	b.shutdown = false
	b.stopped = false
	b.lock.Unlock()
}

func (b *BiRPC) getShutdonwStopped() (bool, bool) {
	b.lock.RLock()
	shutdown, stopped := b.shutdown, b.stopped
	b.lock.RUnlock()

	return shutdown, stopped
}

func (b *BiRPC) Go(method string, params interface{}, result interface{}, done chan *Call) *Call {
	call := &Call{
		Method: method,
		Params: params,
		Result: result,
	}

	if done == nil {
		done = make(chan *Call, 10) // buffered.
	} else {
		// If caller passes done != nil, it must arrange that
		// done has enough buffer for the number of simultaneous
		// RPCs that will be using that channel.  If the channel
		// is totally unbuffered, it's best not to run at all.
		if cap(done) == 0 {
			panic("birpc: done channel is unbuffered")
		}
	}

	call.Done = done
	call.Error = b.send(call, false)

	return call
}

func (b *BiRPC) GoFuture(method string, params interface{}, result interface{}, done chan *Call) func() (call *Call) {
	finished := make(chan *Call)

	go func() {
		finished <- b.Go(method, params, result, done)
	}()

	return func() *Call {
		return <-finished
	}
}

func (b *BiRPC) Call(method string, params interface{}, result interface{}) error {
	call := <-b.Go(method, params, result, make(chan *Call, 1)).Done
	return call.Error
}

func (b *BiRPC) Notify(method string, params interface{}) error {
	call := &Call{
		Method: method,
		Params: params,
	}

	return b.send(call, true)
}

func (b *BiRPC) send(call *Call, notification bool) (err error) {
	var seq uint32

	if notification {
		seq = notificationSeq
	} else {
		b.callsLock.Lock()
		seq = b.seq
		b.seq++

		if b.seq == maxSeq { // If max reached
			b.seq = startSeq
		}

		b.calls[seq] = call
		b.callsLock.Unlock()
	}

	req := &Request{
		Seq:    seq,
		Method: call.Method,
	}

	err = b.codec.WriteRequest(req, call.Params)
	if err != nil {
		if !notification {
			call = b.unregisterCall(seq)

			call.Error = err
			call.done()
		}
	}

	return b.codec.Flush()
}

func (b *BiRPC) cancelPendingCalls() {
	b.callsLock.Lock()

	for _, call := range b.calls {
		call.Error = ErrCanceledCall
		call.done()
	}

	b.callsLock.Unlock()
}

func (b *BiRPC) unregisterCall(seq uint32) *Call {
	b.callsLock.Lock()
	call := b.calls[seq]
	delete(b.calls, seq)
	b.callsLock.Unlock()

	return call
}

func (call *Call) done() {
	select {
	case call.Done <- call:
		// ok

	default:
		// We don't want to block here.  It is the caller's responsibility to make
		// sure the channel has enough buffer space. See comment in Go().

		logger.Println(
			"birpc: discarding Call reply due to insufficient Done chan capacity")
	}
}
