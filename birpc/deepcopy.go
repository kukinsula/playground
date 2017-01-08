package birpc

import (
	"fmt"
	"reflect"
)

var (
	ErrNotAPointer = fmt.Errorf("copy expects out to be a pointer")
)

func Copy(in interface{}, out interface{}) (err error) {
	logger.Printf("Copying %v (%T) into %v (%T)", in, in, out, out)

	inValue := reflect.ValueOf(in)
	outValue := reflect.ValueOf(out)

	if outValue.Kind() != reflect.Ptr {
		return ErrNotAPointer
	}

	// if inValue.Kind() == reflect.Ptr {
	// 	inValue = inValue.Elem()
	// }

	// inType := reflect.TypeOf(in)
	// outType := reflect.TypeOf(out)

	// if inType != outType {
	// 	return fmt.Errorf("Copy mismatch type %T and %T", in, out)
	// }

	// inElem := inValue.Elem()
	// outElem := outValue.Elem()

	// if !outElem.CanSet() {
	// 	return fmt.Errorf("unhandled type %T", in)
	// }

	// outElem.Set(inElem)

	return switchInKind(inValue, outValue)
}

func switchInKind(inValue reflect.Value, outValue reflect.Value) error {
	switch inValue.Kind() {
	case reflect.Ptr:
		logger.Printf("inValue.Kind = Ptr")

		return switchInKind(inValue.Elem(), outValue)

	case reflect.Uint64:
		logger.Printf("inValue.Kind = Uint64")

		if outValue.Kind() == reflect.Ptr {
			outValue = outValue.Elem()
		}

		logger.Printf("outValue.Kind = %v", outValue.Kind())

		switch outValue.Kind() {
		case reflect.Int:
			logger.Printf("outValue.Kind = Int")

		case reflect.Uint64:
			logger.Printf("outValue.Kind = Uint64")

			logger.Printf("XXXX %b %b", outValue.CanSet(), outValue.CanSet())

			outValue.SetUint(inValue.Uint())

		default:
			return fmt.Errorf("unhandled reflect.Kind 2 %v", outValue)
		}

	default:
		return fmt.Errorf("unhandled reflect.Kind 1 %v", inValue)
	}

	return nil
}

func switchOutKind(inValue reflect.Value, outValue reflect.Value) error {

	return nil
}
