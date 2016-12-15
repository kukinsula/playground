package birpc

import (
	"sync"
)

type Service interface {
	Exec(req *Request, v interface{}) (interface{}, error)
}

type ServiceSet struct {
	services map[string]Service
	locj     sync.RWMutex
}
