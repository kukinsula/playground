package main

import (
	"log"
	"os"
)

var (
	logger = log.New(os.Stdout, "", log.Ltime)
)

// TODO:
//
// * msgpack
// * mesurer request

func main() {
	config, err := GetConfig()
	if err != nil {
		logger.Printf("GetConfig failed: %s", err)
		return
	}

	pubsub, err := NewPubSub(config)
	if err != nil {
		logger.Printf("NewPubSub failed: %s", err)
		return
	}

	err = pubsub.Start(config)
	if err != nil {
		logger.Printf("Start failed: %s", err)
		return
	}

	pubsub.Stop()
}
