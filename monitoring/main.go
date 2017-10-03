package main

import (
	"fmt"
	"os"
	"os/signal"

	"github.com/kukinsula/playground/monitoring/metric"
)

func main() {
	config, err := metric.NewConfig()
	if err != nil {
		fmt.Println("New config failed: ", err)
		os.Exit(1)
	}

	monitor, err := NewMonitoring(config)
	if err != nil {
		fmt.Println("New montoring failed: ", err)
		os.Exit(2)
	}

	monitor.Start()

	done := make(chan os.Signal, 1)
	signal.Notify(done, os.Interrupt)

	signal := <-done

	fmt.Printf("Caught signal %s!", signal)

	monitor.Stop()

	os.Exit(0)
}
