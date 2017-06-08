package protocol

import (
	"testing"
)

func TestSerializers(t *testing.T) {
	validSerializers := []string{
		"gob",
		"jsonrpc",
		"jsonrpc2",
	}

	for _, serializer := range validSerializers {
		_, err := Serializer(serializer).Codec(nil)
		if err != nil {
			t.Errorf("SerializerToCodec(%s) should not fail: %s", serializer, err)
		}
	}

	invalidSerializers := []string{
		"json",
		"json2",
	}

	for _, serializer := range invalidSerializers {
		_, err := Serializer(serializer).Codec(nil)
		if err == nil {
			t.Errorf("SerializerToCodec(%s) should fail", serializer)
		}
	}
}

func TestSerializers2(t *testing.T) {
	validSerializers := [][]string{
		[]string{"gob"},
		[]string{"jsonrpc"},
		[]string{"jsonrpc2"},

		[]string{"gob", "jsonrpc"},
		[]string{"gob", "jsonrpc2"},
		[]string{"jsonrpc", "jsonrpc2"},

		[]string{"gob", "jsonrpc", "jsonrpc2"},
	}

	for _, serializers := range validSerializers {
		for _, serializer := range serializers {
			_, err := Serializer(serializer).Codec(nil)
			if err != nil {
				t.Errorf("SerializerToCodec(%s) should not fail: %s", serializer, err)
			}
		}
	}

	// invalidSerializers := []string{
	// 	"jsonrpc",
	// 	"jsonrpc2",
	// }

	// for _, serializer := range invalidSerializers {
	// 	_, err := SerializerToCodec(serializer, nil)
	// 	if err == nil {
	// 		t.Errorf("SerializerToCodec(%s) should fail", serializer)
	// 	}
	// }
}
