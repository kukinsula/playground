package birpc

import (
	"context"
	"fmt"
	"testing"
)

type testService struct{}

func (t *testService) FortyTwo(ctx context.Context,
	client *Client,
	params int,
	result *int,
) error {

	*result = 42

	return nil
}

func (t *testService) Pi(ctx context.Context,
	client *Client,
	params int,
	result *float64,
) error {

	*result = float64(3.14159)

	return nil
}

func (t *testService) Error(ctx context.Context,
	client *Client,
	params int,
	result *float64,
) error {

	return fmt.Errorf("JUST AN ERROR")
}

func TestServiceSet(t *testing.T) {
	set := &serviceSet{services: make(map[string]*service)}

	n, err := set.Register("Service", &testService{})
	if err != nil {
		t.Errorf("serviceSet's Register should not return error")
	}

	if n != 3 {
		t.Errorf("serviceSet's Register should return 3")
	}

	validServices := []string{
		"Service.FortyTwo",
		"Service.Pi",
		"Service.Error",
	}

	for _, s := range validServices {
		_, err = set.Get(s)
		if err != nil {
			t.Errorf("serviceSet's Get Service.FortyTwo should not return error")
		}
	}
}
