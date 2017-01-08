package birpc

import (
	"fmt"
	"testing"
)

var (
	expectedRes    = 42
	expectedParams = 24
	secondParam    = "bar"
	expectedErr    = fmt.Errorf("this is an error returned by a Service")
)

type ObjService struct{}
type ErrService struct{}

func (s *ObjService) Exec(req *Request, v interface{}) (interface{}, error) {
	return expectedRes, nil
}

func (s *ErrService) Exec(req *Request, v interface{}) (interface{}, error) {
	return nil, expectedErr
}

func TestServiceSet(t *testing.T) {
	services := NewServiceSet()

	name := "foo"

	fooService := func(req *Request, v interface{}) (interface{}, error) {
		if req.Method != name {
			t.Errorf("Request.Method should be %s, not %s", name, req.Method)
		}

		if req.Params != expectedParams {
			t.Errorf("Request.Params should be %v, not %v",
				expectedParams, req.Params)
		}

		if v != secondParam {
			t.Errorf("v should be %v, not %v", secondParam, v)
		}

		return expectedRes, nil
	}

	if services.Exists(name) {
		t.Errorf("Service %s should not exist yet", name)
	}

	service, err := services.Get(name)
	if service != nil || err == nil {
		t.Errorf("Get Service %s should return nil and error: service=%v, error=%s",
			name, err)
	}

	services.RegisterFunc(name, fooService)

	if !services.Exists(name) {
		t.Errorf("Service %s should exist now", name)
	}

	service, err = services.Get(name)
	if err != nil {
		t.Errorf("Get Service %s should not fail: %s", name, err)
	}

	req := &Request{
		Method: name,
		Params: expectedParams,
	}

	res, err := service.Exec(req, secondParam)
	if err != nil {
		t.Errorf("Exec service %s should not fail: %s", err)
	}

	if res != expectedRes {
		t.Errorf("Exec service %s should return %d", name, expectedRes)
	}

	res2, err := services.Exec(req, secondParam)
	if err != nil {
		t.Errorf("Exec service %s should not fail: %s", err)
	}

	if res2 != expectedRes {
		t.Errorf("Exec service %s should return %d", name, expectedRes)
	}

	if res != res2 {
		t.Errorf("Service %s: res and res2 should be equal: %v, %v",
			name, res, res2)
	}

	name = "bar"
	o := &ObjService{}

	if services.Exists(name) {
		t.Errorf("Service %s should not exist yet", name)
	}

	service, err = services.Get(name)
	if service != nil || err == nil {
		t.Errorf("Get Service %s should return nil and error: service=%v, error=%s",
			name, err)
	}

	services.Register(name, o)

	if !services.Exists(name) {
		t.Errorf("Service %s should exist now", name)
	}

	service, err = services.Get(name)
	if err != nil {
		t.Errorf("Get Service %s should not fail: %s", name, err)
	}

	res, err = service.Exec(req, expectedParams)
	if err != nil {
		t.Errorf("Service %s Exec should not return an error: %s", err)
	}

	if res != expectedRes {
		t.Errorf("Service %s should return %v", name, expectedRes)
	}

	name = "foobar"
	e := &ErrService{}

	services.Register(name, e)

	service, err = services.Get(name)
	if err != nil {
		t.Errorf("Get Service %s should not fail: %s", name, err)
	}

	res, err = service.Exec(req, expectedParams)
	if err == nil {
		t.Errorf("Service %s Exec should return an error, not nil")
	}

	if err != expectedErr {
		t.Errorf("Service %s should return error %s", name, expectedErr)
	}

	services.Unregister(name)
	if services.Exists(name) {
		t.Errorf("Service % shound not exist anymore", name)
	}

	services.Clear()

	if services.Exists("foo") || services.Exists("bar") || services.Exists("foobar") {
		t.Errorf("Services 'foo', 'bar' and 'foobar' shound not exist")
	}
}
