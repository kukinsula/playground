package gogroup

import (
	_ "fmt"
	"time"
)

// GoGroup aims to handle multiple goroutines from their begining to their end.
type GoGroup struct {
	nb                                 int
	add                                chan int
	ready                              chan struct{}
	done, waiting, finished, interrupt chan struct{}
}

// NewGroup returns a new GoGroup.
func NewGroup() *GoGroup {
	group := &GoGroup{
		nb:        0,
		add:       make(chan int),
		done:      make(chan struct{}),
		ready:     make(chan struct{}),
		finished:  make(chan struct{}),
		interrupt: make(chan struct{}),
		waiting:   make(chan struct{}),
	}

	// monitoring is started in a goroutine and will receive signals
	// through the group's channels.
	go group.monitoring()

	return group
}

// Add should be called before the begining of a Goroutine.
func (g *GoGroup) Add(delta int) {
	g.add <- delta
	<-g.ready
}

// Done should be called when a goroutine, previously added, is done.
func (g *GoGroup) Done() {
	g.done <- struct{}{}
	<-g.ready
}

// Wait waits for all goroutines to be Done.
func (g *GoGroup) Wait() {
	g.waiting <- struct{}{}
	<-g.finished
}

// WaitWithTimeout waits for all goroutines to be Done or timeout expiration.
// It returns true if the timeout expired, false otherwise.
func (g *GoGroup) WaitWithTimeout(timeout time.Duration) bool {
	timedout := false

	// Signals monitoring we start waiting
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

// Interrupt unblocks Wait or WaitWithTimeout even if some goroutines added
// are still alive
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
		case delta := <-g.add:
			g.nb += delta

			// unlocks channel held by Add
			g.ready <- struct{}{}
			break

		case <-g.done:
			g.nb--

			if g.nb == 0 {
				select {
				case <-g.waiting: // Are we already waiting ?
					running = false
					break

				default:
					break
				}
			}

			// Unlocks channel held by Done
			g.ready <- struct{}{}
			break

		case <-g.interrupt:
			running = false
			break
		}
	}

	// Unlocks channel held by Wait
	g.finished <- struct{}{}
}
