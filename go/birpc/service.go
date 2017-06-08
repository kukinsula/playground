package birpc

import (
	"context"
	"reflect"
	"sync"
)

// TODO : cacher service
//
// type service struct {
// 	params reflect.Type
// 	method reflect.Type
// }

// TODO : pour un protocole dont les requêtes et les réponses sont des
// tableau, pa exemple, le Codec doit les encoder d'une très dans les
// WriteRequest et WriteResponse. Pour les décoder il décode tout dans
// ReadHeader d'un coup dans la struct concrète du Codec (i.e
// bhp.Codec) ET il prend un mutex pour réserver l'accès à ce
// message. Dans les ReadRequestBody et ReadResponseBody elle ne font
// rien d'autre que de libérer le mutex.

// TODO:
//
// * Codec:
//     ** Codecs: MessagePack, Cbor et Binc grâce à github.com/ugorji/go/
//
// * Divers: codec doc, README.md, examples

var (
	// reflect.Types used to check method suitability
	contextType = reflect.TypeOf((*context.Context)(nil)).Elem()
	clientType  = reflect.TypeOf(&Client{})
	errorType   = reflect.TypeOf((*error)(nil)).Elem()
)

type service struct {
	rcvr   reflect.Value
	method reflect.Method
	param  reflect.Type
	result reflect.Type
}

func (s *service) Execute(ctx context.Context,
	client *Client,
	param interface{},
	result interface{},
) (err error) {

	results := s.method.Func.Call([]reflect.Value{
		s.rcvr,
		reflect.ValueOf(ctx),
		reflect.ValueOf(client),
		reflect.ValueOf(param),
		reflect.ValueOf(result),
	})

	if !results[0].IsNil() {
		return results[0].Interface().(error)
	}

	return nil
}

type serviceSet struct {
	services map[string]*service
	lock     sync.RWMutex
}

func (s *serviceSet) Register(name string, rcvr interface{}) (int, error) {
	rcvrType := reflect.TypeOf(rcvr)
	n := 0

	for i := 0; i < rcvrType.NumMethod(); i++ {
		if s.registerMethod(name, rcvr, rcvrType.Method(i)) == nil {
			n++
		}
	}

	if n == 0 {
		return n, ErrNoSuitableMethods
	}

	return n, nil
}

func (s *serviceSet) Get(name string) (*service, error) {
	s.lock.Lock()
	service, exists := s.services[name]
	s.lock.Unlock()

	if !exists {
		return nil, ErrServiceNotFound
	}

	return service, nil
}

func (s *serviceSet) registerMethod(name string,
	rcvr interface{},
	method reflect.Method,
) error {

	serviceName := name + "." + method.Name

	param, result, suitable := s.parse(method)
	if !suitable {
		return ErrMethodNotSuitable
	}

	s.lock.Lock()
	s.services[serviceName] = &service{
		rcvr: reflect.ValueOf(rcvr),
		// method: reflect.ValueOf(method),
		method: method,
		param:  param,
		result: result,
	}
	s.lock.Unlock()

	return nil
}

// parse returns true if method is valid as a Service, false otherwise.
//
// Example:
//
// func (o *Object) Test(context.Context, *Client, XXX, *YYY) error
func (s *serviceSet) parse(method reflect.Method) (reflect.Type, reflect.Type, bool) {
	methodType := method.Type

	// Method must be exported
	if method.PkgPath != "" {
		return nil, nil, false
	}

	// Method needs three params: Receiver, Context, *Client, XXX, *YYY
	if methodType.NumIn() != 5 {
		return nil, nil, false
	}

	// 1st method argument must be type context.Context
	if !methodType.In(1).Implements(contextType) {
		return nil, nil, false
	}

	// 2nd method argument must be type *Client
	if methodType.In(2) != clientType {
		return nil, nil, false
	}

	// Method needs to return 1 values
	if methodType.NumOut() != 1 {
		return nil, nil, false
	}

	// The result must be a pointer
	resultType := methodType.In(4)
	if resultType.Kind() != reflect.Ptr {
		return nil, nil, false
	}

	// 2nd returned value needs to be an error
	if methodType.Out(0) != errorType {
		return nil, nil, false
	}

	return methodType.In(3), resultType, true
}
