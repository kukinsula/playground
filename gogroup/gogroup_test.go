package gogroup

import (
	_ "fmt"
	"testing"
	"time"
)

func TestGoGroup(t *testing.T) {
	group := NewGroup()

	for i := 0; i < 1000; i++ {
		group.Add()
		go group.Done()
	}

	group.Wait()
}

func TestGoGroupWithTimeout(t *testing.T) {
	group := NewGroup()

	for i := 0; i < 1000; i++ {
		group.Add()
		go group.Done()
	}

	timedout := group.WaitWithTimeout(10 * time.Second)
	if timedout {
		t.Errorf("WaitWithTimeout should have finished before 10s")
	}

	// group = NewGroup()

	// for i := 0; i < 1; i++ {
	// 	group.Add()
	// 	go func() {
	// 		defer group.Done()
	// 		time.Sleep(10 * time.Second)
	// 	}()
	// }

	// timedout = group.WaitWithTimeout(1 * time.Millisecond)
	// if !timedout {
	// 	t.Errorf("WaitWithTimeout should not have finished before 1ms")
	// }
}

func TestGoGroupGroup(t *testing.T) {
	father := NewGroup()

	for i := 0; i < 10; i++ { // Father
		father.Add()
		go func(father *GoGroup) {
			defer father.Done()

			son := NewGroup()

			for j := 0; j < 100; j++ { // Son
				son.Add()
				go func() {
					defer son.Done()
					go time.Sleep(1 * time.Millisecond)
				}()
			}

			son.Wait()
		}(father)
	}

	father.Wait()
}

// Creates max 8192 simultaneous goroutines (go -race imposes that).
func TestGoGroupGroupGroup(t *testing.T) {
	grandfather := NewGroup()

	for i := 0; i < 2; i++ { // GrandFather
		grandfather.Add()
		go func(grandfather *GoGroup) {
			defer grandfather.Done()

			father := NewGroup()

			for j := 0; j < 4; j++ { // Father
				father.Add()
				go func(father *GoGroup) {
					defer father.Done()

					son := NewGroup()

					for k := 0; k < 1024; k++ { // Son
						son.Add()
						go func() {
							defer son.Done()

							time.Sleep(1 * time.Millisecond)
						}()
					}

					son.Wait()
				}(father)
			}

			father.Wait()
		}(grandfather)
	}

	grandfather.Wait()
}

func BenchmarckGoGroup(b *testing.B) {
	group := NewGroup()

	for n := 0; n < b.N; n++ {
		group.Add()
		go group.Done()
	}

	group.Wait()
}
