package metric

import (
	"fmt"
)

type Metric interface {
	Update() error
	Name() string
	// Save() error
	// Close() error
}

func checkSscanf(field string, err error, n, expected int) error {
	if err != nil {
		return fmt.Errorf("Sscanf '%s' failed: %s", field, err)
	}

	if n != expected {
		return fmt.Errorf("Sscanf '%s' parsed %d item(s) but expected %d",
			field, n, expected)
	}

	return nil
}
