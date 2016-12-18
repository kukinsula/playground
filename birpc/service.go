package birpc

import (
	"fmt"
	"sync"
)

type Service interface {
	Exec(req *Request, v interface{}) (interface{}, error)
}

type ServiceFunc func(req *Request, v interface{}) (interface{}, error)

type ServiceSet struct {
	services map[string]Service
	lock     sync.RWMutex
}

func NewServiceFunc(fn func(req *Request, v interface{}) (interface{}, error)) ServiceFunc {
	return ServiceFunc(fn)
}

func (f ServiceFunc) Exec(req *Request, v interface{}) (interface{}, error) {
	return f(req, v)
}

func NewServiceSet() *ServiceSet {
	return &ServiceSet{
		services: make(map[string]Service),
	}
}

func (s *ServiceSet) Register(name string, service Service) {
	s.lock.Lock()
	s.services[name] = service
	s.lock.Unlock()
}

func (s *ServiceSet) RegisterFunc(name string, fn func(req *Request, v interface{}) (interface{}, error)) {
	s.Register(name, NewServiceFunc(fn))
}

func (s *ServiceSet) Unregister(name string) {
	s.lock.Lock()
	delete(s.services, name)
	s.lock.Unlock()
}

func (s *ServiceSet) Clear() {
	s.lock.Lock()

	for name, _ := range s.services {
		delete(s.services, name)
	}

	s.lock.Unlock()
}

func (s *ServiceSet) Get(name string) (service Service, err error) {
	s.lock.RLock()
	service, exists := s.services[name]
	s.lock.RUnlock()

	if !exists {
		err = fmt.Errorf("ServiceSet: service name %s not found", name)
	}

	return
}

func (s *ServiceSet) Exists(name string) bool {
	_, err := s.Get(name)

	return err != nil
}

func (s *ServiceSet) Exec(req *Request, v interface{}) (interface{}, error) {
	service, err := s.Get(req.Method)
	if err != nil {
		return nil, err
	}

	return service.Exec(req, v)
}
