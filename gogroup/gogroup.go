package gogroup

import (
	_ "fmt"
	"time"
)

type GoGroup struct {
	nb                                      int64
	ready                                   chan struct{}
	add, done, waiting, finished, interrupt chan struct{}
}

func NewGroup() *GoGroup {
	group := &GoGroup{
		nb:        0,
		add:       make(chan struct{}),
		done:      make(chan struct{}),
		ready:     make(chan struct{}),
		finished:  make(chan struct{}),
		interrupt: make(chan struct{}),
		waiting:   make(chan struct{}),
	}

	go group.monitoring()

	return group
}

func (g *GoGroup) Add() {
	g.add <- struct{}{}
	g.lock()
}

func (g *GoGroup) Done() {
	g.done <- struct{}{}
	g.lock()
}

func (g *GoGroup) Wait() {
	g.waiting <- struct{}{}
	<-g.finished
}

func (g *GoGroup) WaitWithTimeout(timeout time.Duration) bool {
	timedout := false

	g.waiting <- struct{}{}

	select {
	case <-time.After(timeout):
		timedout = true
		g.interrupt <- struct{}{}
		break

	case <-g.finished:
		break
	}

	return timedout
}

func (g *GoGroup) Interrupt() {
	g.interrupt <- struct{}{}
	<-g.finished
}

// monitoring loops "infenitely" handling signals coming from g.add,
// g.done and g.interrupt channels. The loops ends when the group is
// waiting andthere is no goroutine running or on interrupt.
func (g *GoGroup) monitoring() {
	running := true

	for running {
		select {
		case <-g.add:
			g.nb++
			g.unlock()
			break

		case <-g.done:
			g.nb--

			// If n = 0 we need to check if we're already waiting
			if g.nb == 0 {
				select {
				case <-g.waiting:
					running = false
					break

				default:
					break
				}
			}

			g.unlock()
			break

		case <-g.interrupt:
			running = false
			break
		}
	}

	g.finished <- struct{}{}
}

// locks and unlocks nb
func (g *GoGroup) lock()   { <-g.ready }
func (g *GoGroup) unlock() { g.ready <- struct{}{} }
