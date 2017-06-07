package main

import (
	"fmt"
	"os"
	"os/signal"
)

func HandleInerrupt(nb int) chan struct{} {
	interrupt := make(chan os.Signal)
	interrupted := make(chan struct{}, nb)

	signal.Notify(interrupt, os.Interrupt)
	go func() {
		<-interrupt

		fmt.Println("CTRL-C")

		for i := 0; i < nb; i++ {
			interrupted <- struct{}{}
		}

		fmt.Println("XXXXXXXXX")
	}()

	return interrupted
}

func main() {
	nb := 2
	ch := make(chan struct{})
	done := make(chan struct{}, nb)
	interrupted := HandleInerrupt(nb)

	go ping(ch, done, interrupted)
	go pong(ch, done, interrupted)

	for i := 0; i < nb; i++ {
		fmt.Println("DONING...")
		<-done

		fmt.Println("DONE", i)
	}
}

func ping(ch, done, interrupted chan struct{}) {
	running := true

	fmt.Println("ping")
	ch <- struct{}{}

	for running {
		select {
		case <-ch:
			fmt.Println("ping")
			ch <- struct{}{}
			break

		case <-interrupted:
			running = false
			fmt.Println("ping interrupted")
			break
		}
	}

	done <- struct{}{}
}

func pong(ch, done, interrupted chan struct{}) {
	running := true

	for running {
		select {
		case <-ch:
			fmt.Println("pong")
			ch <- struct{}{}
			break

		case <-interrupted:
			running = false
			fmt.Println("pong interrupted")
			break
		}
	}

	done <- struct{}{}
}
