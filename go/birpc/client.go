package birpc

import (
	"context"
	"errors"
	"fmt"
	"math"
	"net"
	"reflect"
	"sync"
)

const (
	notificationSeq = uint64(0)
	firstCallSeq    = uint64(1)
)

// Client is able to make RPC calls (synchronous and asynchronous) and
// to receive RPC Responses or incoming RPC Requests.
type Client struct {
	Conn           net.Conn                                        // Peer connection
	ContextBuilder func() context.Context                          // New Context on connection
	AsyncError     func(ctx context.Context, c *Client, err error) // Async error callback
	serviceSet                                                     // Set of available Services
	codec          Codec                                           // Codec to  peer
	lock           sync.RWMutex                                    // Protects calls and seq
	calls          map[uint64]*Call                                // Pending Calls
	seq            uint64                                          // Current messages id
	server         bool                                            // Client or Server side ?
	servingLock    sync.RWMutex                                    // Protects serving
	serving        bool                                            // Is serving RPC calls
	group          sync.WaitGroup                                  // Group of running gorutines
	close          chan struct{}                                   // Close Synchronisation
	prefix         string
}

// Call represents an active RPC.
type Call struct {
	Method string      // The name of the service and method to call.
	Params interface{} // The argument to the function (*struct).
	Result interface{} // The reply from the function (*struct).
	Error  error       // After completion, the error status.
	Done   chan *Call  // Strobes when call is complete.
}

// NewClient returns a new Client with a GobCodec build above conn.
func NewClient(conn net.Conn) *Client {
	return NewClientWithCodec(conn, NewGobCodec(conn))
}

// NewClientWithCodec returns a new Client with a specific Codec.
func NewClientWithCodec(conn net.Conn, codec Codec) *Client {
	return &Client{
		serviceSet: serviceSet{
			services: make(map[string]*service),
		},
		Conn: conn,
		ContextBuilder: func() context.Context {
			return context.Background()
		},
		AsyncError: func(ctx context.Context, client *Client, err error) {
			logger.Printf("%s [ASYNC ERROR] %s", client.prefix, err)
		},
		codec:  codec,
		seq:    firstCallSeq,
		calls:  make(map[uint64]*Call),
		close:  make(chan struct{}),
		prefix: "Client " + conn.LocalAddr().String(),
	}
}

// Serve loops waiting for incoming messages (Requests and Responses)
// and then handles each of them.
func (c *Client) Serve() (err error) {
	c.servingLock.Lock()

	if c.serving {
		c.servingLock.Unlock()
		return ErrAlreadyServing
	}

	c.serving = true
	c.servingLock.Unlock()

	logger.Printf("%s: starts accepting RPC messages", c.prefix)

	for serving := true; serving; serving = c.Serving() {
		req := Request{}
		resp := Response{}

		err = c.codec.ReadHeader(&req, &resp)
		if err != nil {
			if !c.Serving() { // Close or Wait were called
				err = ErrShutdown
			}
			break
		}

		if req.Method != "" {
			err = c.handleRequest(&req)
			if err != nil {
				err = fmt.Errorf("handle request [%s] failed: %s", &req, err)
			}
		} else {
			err = c.handleResponse(&resp)
			if err != nil {
				err = fmt.Errorf("handle response [%s] failed: %s", &resp, err)
			}
		}

		if err != nil {
			break
		}
	}

	c.terminatePendingCalls(err)

	if !c.Serving() { // If Close or Wait were called
		c.close <- struct{}{} // Syncs with Close
	}

	logger.Printf("%s: Serve finished!", c.prefix)

	return err
}

// Close closes the Client's connection.
func (c *Client) Close() error {
	c.servingLock.Lock()

	if !c.serving {
		c.servingLock.Unlock()
		return ErrAlreadyClosed
	}

	c.serving = false // c.Serving()returns false now
	c.servingLock.Unlock()

	err := c.codec.Close() // Unblocks c.codec.ReadHeader()
	<-c.close              // Syncs with Serve end

	return err
}

// Wait is like Close but also waits for all the running goroutines
// handling each received messages.
func (c *Client) Wait() error {
	err := c.Close()
	if err == ErrAlreadyClosed {
		return err
	}

	c.group.Wait()

	return nil
}

// Serving returns wether the Client is currently serving RPC messages.
func (c *Client) Serving() bool {
	c.servingLock.RLock()
	serving := c.serving
	c.servingLock.RUnlock()

	return serving
}

// handleRequest executes the requested Service and sends back the
// response to the peer.
func (c *Client) handleRequest(req *Request) error {
	s, err := c.Get(req.Method)
	if err != nil {
		return err
	}

	var param reflect.Value

	argIsValue := false // if true, need to indirect before calling.

	if s.param.Kind() == reflect.Ptr {
		param = reflect.New(s.param.Elem())
	} else {
		argIsValue = true
		param = reflect.New(s.param)
	}

	err = c.codec.ReadRequestBody(param.Interface())
	if err != nil {
		return err
	}

	if argIsValue {
		param = param.Elem()
	}

	result := reflect.New(s.result.Elem())

	var method string
	if req.Seq == notificationSeq {
		method = "Notification"
	} else {
		method = "Request"
	}
	logger.Printf("%s <- %s [%s, Params:%v]", c.prefix, method, req, param)

	c.group.Add(1)
	go func() {
		defer c.group.Done()

		ctx := c.ContextBuilder() // TODO : protection contre les panic ???

		defer func() { // Panic protection
			if r := recover(); r != nil {
				err := fmt.Errorf("panic while handling request: [%s, Params:%v]: %v",
					req, param, r)

				resp := &Response{Seq: req.Seq, Error: err.Error()}
				err = c.codec.WriteResponse(resp, 0)
				if err != nil && c.AsyncError != nil {
					c.AsyncError(ctx, c, fmt.Errorf("Codec.WriteResponse failed: %s", err))
					return
				}

				logger.Printf("%s -> Response [%s]", c.prefix, resp)
			}
		}()

		res := s.method.Func.Call([]reflect.Value{
			s.rcvr,
			reflect.ValueOf(ctx),
			reflect.ValueOf(c),
			param,
			result,
		})

		if !res[0].IsNil() {
			err = res[0].Interface().(error)
		}

		if req.Seq == notificationSeq { // No response
			if err != nil && c.AsyncError != nil {
				c.AsyncError(ctx, c, fmt.Errorf("Service '%s' failed: %s",
					req.Method, err))
			}
			return
		}

		resp := &Response{Seq: req.Seq}

		if err != nil {
			resp.Error = err.Error()
		}

		err = c.codec.WriteResponse(resp, result.Interface())
		if err != nil && c.AsyncError != nil {
			c.AsyncError(ctx, c, fmt.Errorf("Codec.WriteResponse(%s, %v) failed: %s",
				resp, res, err))

			return
		}

		logger.Printf("%s -> Response [%s, Result:%v]",
			c.prefix, resp, result.Interface())
	}()

	return nil
}

// handleResponse extracts the response body into the registered call
// made previously.
func (c *Client) handleResponse(resp *Response) (err error) {
	seq := resp.Seq

	c.lock.Lock()
	call := c.calls[seq]
	delete(c.calls, seq)
	c.lock.Unlock()

	if call == nil {
		// We've got no pending call. lThat usually means that
		// WriteRequest partially failed, and call was already
		// removed; response is a server telling us about an
		// error reading request body.

		return c.codec.ReadResponseBody(nil)
	}

	if resp.Error != "" {
		// We've got an error response. Give this to the request;
		// any subsequent requests will get the ReadResponseBody
		// error if there is one.

		call.Error = errors.New(resp.Error)
		err = c.codec.ReadResponseBody(nil)
		if err != nil {
			return err
		}
	} else {
		err = c.codec.ReadResponseBody(call.Result)
		if err != nil {
			return err
		}

		logger.Printf("%s <- Response [%s, Result:%v]",
			c.prefix, resp, call.Result)
	}

	return call.done()
}

// terminatePendingCalls terminates all pending calls. All the calls
// are answered with the provided error
func (c *Client) terminatePendingCalls(err error) {
	c.lock.RLock()
	for _, call := range c.calls {
		call.Error = err

		if err := call.done(); err != nil {
			logger.Printf("Terminate pending call failed: %s\n", err)
		}
	}
	c.lock.RUnlock()
}

func (c *Client) asyncCall(
	method string,
	params, result interface{},
	done chan *Call,
	mode string,
) (*Call, error) {

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
	err := c.send(call, mode)
	if err != nil {
		return nil, err
	}

	return call, nil
}

// Go invokes the function asynchronously. It returns the Call
// structure representing the invocation. The done channel will signal
// when the call is complete by returning the same Call object. If
// done is nil, Go will allocate a new channel. If non-nil, done must
// be buffered or Go will deliberately crash.
func (c *Client) Go(
	method string,
	params, result interface{},
	done chan *Call,
) (*Call, error) {

	return c.asyncCall(method, params, result, done, "Go")
}

// Call invokes the named function, waits for it to complete, and
// returns its error status.
func (c *Client) Call(
	method string,
	args, result interface{},
) error {

	call, err := c.asyncCall(method, args, result, make(chan *Call, 1), "Call")
	if err != nil {
		return err
	}

	return (<-call.Done).Error
}

func (c *Client) Notify(method string, params interface{}) error {
	return c.send(&Call{Method: method, Params: params}, "Notify")
}

func (c *Client) send(call *Call, method string) error {
	if !c.Serving() {
		return ErrAlreadyClosed
	}

	var seq uint64
	if method == "Notify" { // Call not registered
		seq = notificationSeq
	} else { // Call registered
		c.lock.Lock()
		seq = c.seq

		if seq == math.MaxUint64 {
			c.seq = firstCallSeq
		} else {
			c.seq++
		}

		c.calls[seq] = call
		c.lock.Unlock()
	}

	req := &Request{
		Seq:    seq,
		Method: call.Method,
	}

	err := c.codec.WriteRequest(req, &call.Params)
	if err != nil {
		c.lock.Lock()
		call = c.calls[seq]
		delete(c.calls, seq)
		c.lock.Unlock()

		return err
	}

	logger.Printf("%s -> %s [%s, Param:%v]", c.prefix, method, req, call.Params)

	return nil
}

// setServing set the Client's serving.
func (c *Client) setServing(b bool) {
	c.servingLock.Lock()
	c.serving = b
	c.servingLock.Unlock()
}

func (call *Call) done() (err error) {
	select {
	case call.Done <- call:
		// ok

	default:
		// We don't want to block here.  It is the caller's responsibility to make
		// sure the channel has enough buffer space. See comment in Go().
		err = ErrInsufficientChanCap
	}

	return err
}
