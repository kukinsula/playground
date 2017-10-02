package metric

import (
	"bufio"
	"encoding/json"
	"fmt"
	"os"
	"strings"
)

const (
	meminfo       = "/proc/meminfo"
	memOutputFile = "mem"
)

type Memory struct {
	currentMeasure, lastMeasure *memoryMeasure

	// TODO: ajouter DeltaMemFree, DeltaMemOccupied, DeltaSwapFree, ...
}

type memoryMeasure struct {
	MemTotal        kbyte `json:"total"`
	MemFree         kbyte `json:"free"`
	MemOccupied     kbyte `json:"occupied"`
	MemAvailable    kbyte `json:"available"`
	SwapTotal       kbyte `json:"swapTotal"`
	SwapFree        kbyte `json:"swapFree"`
	SwapOccupied    kbyte `json:"swapOccupied"`
	VmallocTotal    kbyte `json:"vmAllocTotal"`
	VmallocFree     kbyte `json:"vmAllocFree"`
	VmallocOccupied kbyte `json:"vmAllocOccupied"`
}

func NewMemory() (*Memory, error) {
	return &Memory{
		currentMeasure: &memoryMeasure{},
		lastMeasure:    &memoryMeasure{},
	}, nil
}

func (m *Memory) Update() error {
	*m.lastMeasure = *m.currentMeasure

	return m.currentMeasure.update()
}

// func (m *Memory) MarshalCSV() ([]byte, error) {
// 	return m.currentMeasure.MarshalCSV()
// }

func (m *Memory) MarshalJSON() ([]byte, error) {
	return json.Marshal(m.currentMeasure)
}

func (m *Memory) PercentMemFree() float64 {
	return 100.0 - m.PercentMemOccupied()
}

func (m *Memory) PercentMemOccupied() float64 {
	return float64(m.currentMeasure.MemOccupied) * 100.0 /
		float64(m.currentMeasure.MemTotal)
}

func (m *Memory) PercentMemAvailable() float64 {
	return float64(m.currentMeasure.MemAvailable) * 100.0 /
		float64(m.currentMeasure.MemTotal)
}

func (m *Memory) PercentSwapFree() float64 {
	return 100.0 - m.PercentSwapOccupied()
}

func (m *Memory) PercentSwapOccupied() float64 {
	return float64(m.currentMeasure.SwapOccupied) * 100.0 /
		float64(m.currentMeasure.SwapTotal)
}

func (m *Memory) PercentVmallocFree() float64 {
	return 100.0 - m.PercentVmallocOccupied()
}

func (m *Memory) PercentVmallocOccupied() float64 {
	return float64(m.currentMeasure.VmallocOccupied) * 100.0 /
		float64(m.currentMeasure.VmallocFree)
}

func (m *Memory) Name() string {
	return "mem"
}

const memFormat = `===== MEMORY =========================================

  Free:      %s [%.2f %%] (%s)
  Occupied:  %s [%.2f %%] (%s)
  Available: %s [%.2f %%] (%s)
  Total:     %s

  Swap:
    Free:     %s [%.2f %%] (%s)
    Occupied: %s [%.2f %%] (%s)
    Total:    %s

  Vm Alloc:
    Free:     %s [%.2f %%] (%s)
    Occupied: %s [%.2f %%] (%s)
    Total:    %s`

func (m *Memory) String() string {
	return fmt.Sprintf(memFormat,
		m.currentMeasure.MemFree, m.PercentMemFree(),
		m.currentMeasure.MemFree-m.lastMeasure.MemFree,

		m.currentMeasure.MemOccupied, m.PercentMemOccupied(),
		m.currentMeasure.MemOccupied-m.lastMeasure.MemOccupied,

		m.currentMeasure.MemAvailable, m.PercentMemAvailable(),
		m.currentMeasure.MemAvailable-m.lastMeasure.MemAvailable,

		m.currentMeasure.MemTotal,

		m.currentMeasure.SwapFree, m.PercentSwapFree(),
		m.currentMeasure.SwapFree-m.lastMeasure.SwapFree,

		m.currentMeasure.SwapOccupied, m.PercentSwapOccupied(),
		m.currentMeasure.SwapOccupied-m.lastMeasure.SwapOccupied,

		m.currentMeasure.SwapTotal,

		m.currentMeasure.VmallocFree, m.PercentVmallocFree(),
		m.currentMeasure.VmallocFree-m.lastMeasure.VmallocFree,

		m.currentMeasure.VmallocOccupied, m.PercentVmallocOccupied(),
		m.currentMeasure.VmallocOccupied-m.lastMeasure.VmallocOccupied,

		m.currentMeasure.VmallocTotal)
}

// update updates memoryMeasure parsing /proc/meminfo.
func (m *memoryMeasure) update() error {
	file, err := os.Open(meminfo)
	if err != nil {
		return err
	}
	defer file.Close()

	var n int
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()

		if strings.Contains(line, "MemTotal") {
			n, err = fmt.Sscanf(line, "MemTotal: %d kB", &m.MemTotal)
			checkSscanf("MemTotal", err, n, 1)
		} else if strings.Contains(line, "MemFree") {
			n, err = fmt.Sscanf(line, "MemFree: %d kB", &m.MemFree)
			checkSscanf("MemFree", err, n, 1)
		} else if strings.Contains(line, "MemAvailable") {
			n, err = fmt.Sscanf(line, "MemAvailable: %d kB", &m.MemAvailable)
			checkSscanf("MemAvailable", err, n, 1)
		} else if strings.Contains(line, "SwapTotal") {
			n, err = fmt.Sscanf(line, "SwapTotal: %d kB", &m.SwapTotal)
			checkSscanf("SwapTotal", err, n, 1)
		} else if strings.Contains(line, "SwapFree") {
			n, err = fmt.Sscanf(line, "SwapFree: %d kB", &m.SwapFree)
			checkSscanf("SwapFree", err, n, 1)
		} else if strings.Contains(line, "VmallocTotal") {
			n, err = fmt.Sscanf(line, "VmallocTotal: %d kB", &m.VmallocTotal)
			checkSscanf("VmallocTotal", err, n, 1)
		} else if strings.Contains(line, "VmallocUsed") {
			n, err = fmt.Sscanf(line, "VmallocUsed: %d kB", &m.VmallocOccupied)
			checkSscanf("VmallocUsed", err, n, 1)
		}
	}

	m.MemOccupied = m.MemTotal - m.MemFree
	m.SwapOccupied = m.SwapTotal - m.SwapFree
	m.VmallocFree = m.VmallocTotal - m.VmallocOccupied

	return nil
}

// func (m *memoryMeasure) MarshalCSV() ([]byte, error) {}

// func (m memoryMeasure) save() {
// 	file, err := os.OpenFile(memdat, os.O_WRONLY|os.O_APPEND, 0600)
// 	if err != nil {
// 		log.Println(err)
// 	}
// 	defer file.Close()

// 	str := fmt.Sprintf("%d,%d,%d,%d,%d,%d,%d\n",
// 		m.MemTotal, m.MemFree, m.MemOccupied, m.MemAvailable,
// 		m.SwapTotal, m.SwapFree, m.SwapOccupied)

// 	return []byte(str), nil
// 	w := bufio.NewWriter(file)
// 	w.WriteString(str)
// 	w.Flush()
// }

type kbyte int

const (
	mByte = kbyte(1024)
	gByte = 1024 * mByte
	tByte = 1024 * gByte
	pByte = 1024 * tByte
)

func (k kbyte) String() string {
	if k > pByte {
		return fmt.Sprintf("%.3f PB", float64(k)/float64(1000000000000))
	} else if k > tByte {
		return fmt.Sprintf("%.3f TB", float64(k)/float64(1000000000))
	} else if k > gByte {
		return fmt.Sprintf("%.3f GB", float64(k)/float64(1000000))
	} else if k > mByte {
		return fmt.Sprintf("%d MB", int(k)/1000)
	} else if k < mByte {
		return fmt.Sprintf("%d kB", int(k))
	}

	return "[ERROR]"
}
