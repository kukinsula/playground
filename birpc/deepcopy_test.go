package birpc

import (
	"testing"
)

func TestDeepCopy(t *testing.T) {
	in := 42
	out := 3.14159

	err := Copy(&in, &out)
	if err == nil {
		t.Errorf("Copy expects mismatch error")
	}

	err = Copy(in, &out)
	if err == nil {
		t.Errorf("Copy should receive a pointer to in")
	}

	var out2 int
	err = Copy(&in, &out2)
	if err != nil {
		t.Errorf("Copy should not fail: %s", err)
	}

	inMap := map[string]interface{}{
		"A": 1,
		"B": 3.14159,
		"C": true,
		"D": "AZERTY",
	}

	var out3 map[string]interface{}
	err = Copy(&inMap, &out3)
	if err != nil {
		t.Errorf("Copy should not fail: %s", err)
	}

	type foobar struct {
		S string
		F float64
		I int
		U uint32
		B bool
		M map[interface{}]interface{}
	}

	inStruct := foobar{
		S: "AZERTY",
		F: 3.14159,
		I: 42,
		U: uint32(1234),
		B: true,
		M: map[interface{}]interface{}{
			"A":   1,
			42:    24,
			true:  false,
			false: true,
		},
	}

	var out4 foobar
	err = Copy(&inStruct, &out4)
	if err != nil {
		t.Errorf("Copy should not fail: %s", err)
	}
}
