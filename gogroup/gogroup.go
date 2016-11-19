package gogroup

import (
	"time"
)

type GoGroup struct {
	nb                                             int64
	add, done, ready, waiting, finished, interrupt chan struct{}
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
	<-g.ready
}

func (g *GoGroup) Done() {
	g.done <- struct{}{}
	<-g.ready
}

func (g *GoGroup) Wait() {
	g.waiting <- struct{}{}
	<-g.finished
}

func (g *GoGroup) WaitWithTimeout(timeout time.Duration) {
	g.waiting <- struct{}{}

	select {
	case <-time.After(timeout):
	case <-g.finished:
	}
}

func (g *GoGroup) Interrupt() {
	g.interrupt <- struct{}{}
}

func (g *GoGroup) monitoring() {
	running := true

	for running {
		select {
		case <-g.add:
			g.nb++
			g.ready <- struct{}{}
			break

		case <-g.done:
			g.nb--

			if g.nb == 0 {
				select {
				case <-g.waiting:
					running = false
					break

				default:
					break
				}
			}

			g.ready <- struct{}{}
			break

		case <-g.interrupt:
			running = false
			break
		}
	}

	g.finished <- struct{}{}
}
