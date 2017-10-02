package metric

import (
	"bufio"
	"encoding/json"
	"fmt"
	"math"
	"os"
	"runtime"
	"strings"
	"sync"
	"time"
)

const (
	stat         = "/proc/stat"
	nbCpuColumns = 10
)

type CPU struct {
	currentMeasure, lastMeasure *cpuMeasure
	LoadAverage                 float64
	LoadAverages                []float64
	NumCPU                      int
	lock                        sync.RWMutex
}

type cpuMeasure struct {
	SwitchCtxt   int   `json:"switchContext"`
	BootTime     int64 `json:"bootTime"`
	Processes    int   `json:"processes"`
	ProcsRunning int   `json:"procsRunning"`
	ProcsBlocked int   `json:"procsBlocked"`
	cores        [][nbCpuColumns]int
}

func NewCPU() (*CPU, error) {
	NumCPU := runtime.NumCPU()

	return &CPU{
		NumCPU:         NumCPU,
		currentMeasure: newCpuMeasure(),
		lastMeasure:    newCpuMeasure(),
		LoadAverages:   make([]float64, NumCPU),
	}, nil
}

func (c *CPU) Update() error {
	c.lock.Lock()
	defer c.lock.Unlock()

	*c.lastMeasure = *c.currentMeasure
	copy((*c.lastMeasure).cores, (*c.currentMeasure).cores)
	c.currentMeasure.cores = make(
		[][nbCpuColumns]int, runtime.NumCPU()+1)

	err := c.currentMeasure.update()
	if err != nil {
		return err
	}

	c.computeCpuAverages()

	return nil
}

// computeCpuAverages computes the global CPU and all CPU cores usage.
func (c *CPU) computeCpuAverages() {
	c.LoadAverage = c.computeCpuLoad(
		c.currentMeasure.cores[0], c.lastMeasure.cores[0])

	for i := 0; i < runtime.NumCPU(); i++ {
		c.LoadAverages[i] = c.computeCpuLoad(
			c.currentMeasure.cores[i+1], c.lastMeasure.cores[i+1])
	}
}

// computeCpuLoad computes the CPU's first and second raw CPU stats.
func (c *CPU) computeCpuLoad(first, second [nbCpuColumns]int) float64 {
	numerator := float64(
		(second[0] + second[1] + second[2]) -
			(first[0] + first[1] + first[2]))

	denominator := float64(
		(second[0] + second[1] + second[2] + second[3]) -
			(first[0] + first[1] + first[2] + first[3]))

	return math.Abs(numerator / denominator * 100.0)
}

func (c *CPU) MarshalJSON() ([]byte, error) {
	return json.Marshal(map[string]interface{}{
		"switchContext": c.currentMeasure.SwitchCtxt,
		"boottime":      c.currentMeasure.BootTime,
		"processes":     c.currentMeasure.Processes,
		"procsRunning":  c.currentMeasure.ProcsRunning,
		"procsBlocked":  c.currentMeasure.ProcsBlocked,
		"load":          c.LoadAverage,
		"cores":         c.LoadAverages,
	})
}

func (c *CPU) MarshalCSV() ([]byte, error) {
	str := fmt.Sprintf("%.2f,", c.LoadAverage)

	for i := 0; i < c.NumCPU; i++ {
		str += fmt.Sprintf("%.2f", c.LoadAverages[i])

		if i != c.NumCPU-1 {
			str += ","
		}
	}
	str += "\n"

	return []byte(str), nil
}

func (c *CPU) Name() string {
	return "CPU"
}

const cpuFormat = `===== CPU ============================================

  CPU: %.2f %%

%s
  SwitchCtxt:    %d (%d)
  Processes:     %d (%d)
  Procs Blocked: %d
  Pros Running:  %d
  Boot Time:     EPOCH: %d
                 UNIX:  %v`

func (c *CPU) String() string {
	var cores string
	for i := 0; i < c.NumCPU; i++ {
		cores += fmt.Sprintf("    Core #%d: %.2f %%\n",
			i, c.LoadAverages[i])
	}

	return fmt.Sprintf(cpuFormat,
		c.LoadAverage,
		cores,

		c.currentMeasure.SwitchCtxt,
		c.currentMeasure.SwitchCtxt-c.lastMeasure.SwitchCtxt,

		c.currentMeasure.Processes,
		c.currentMeasure.Processes-c.lastMeasure.Processes,

		c.currentMeasure.ProcsBlocked,
		c.currentMeasure.ProcsRunning,
		c.currentMeasure.BootTime,
		time.Unix(c.currentMeasure.BootTime, 0))
}

func newCpuMeasure() *cpuMeasure {
	return &cpuMeasure{
		cores: make([][nbCpuColumns]int, runtime.NumCPU()+1),
	}
}

// update uodates the cpuMeasure parsing /proc/stat file.
func (c *cpuMeasure) update() error {
	file, err := os.Open(stat)
	if err != nil {
		return err
	}
	defer file.Close()

	var lineName string
	var n, cpuCount int

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()

		if strings.Contains(line, "cpu") {
			n, err = fmt.Sscanf(line,
				"%s %d %d %d %d %d %d %d %d %d %d", &lineName,
				&c.cores[cpuCount][0], &c.cores[cpuCount][1],
				&c.cores[cpuCount][2], &c.cores[cpuCount][3],
				&c.cores[cpuCount][4], &c.cores[cpuCount][5],
				&c.cores[cpuCount][6], &c.cores[cpuCount][7],
				&c.cores[cpuCount][8], &c.cores[cpuCount][9],
			)
			checkSscanf(lineName, err, n, 11)
			cpuCount++
		} else if strings.Contains(line, "ctxt") {
			n, err = fmt.Sscanf(line, "ctxt %d", &c.SwitchCtxt)
			checkSscanf("ctxt", err, n, 1)
		} else if strings.Contains(line, "btime") {
			n, err = fmt.Sscanf(line, "btime %d", &c.BootTime)
			checkSscanf("ctxt", err, n, 1)
		} else if strings.Contains(line, "processes") {
			n, err = fmt.Sscanf(line, "processes %d", &c.Processes)
			checkSscanf("ctxt", err, n, 1)
		} else if strings.Contains(line, "procs_running") {
			n, err = fmt.Sscanf(line, "procs_running %d", &c.ProcsRunning)
			checkSscanf("ctxt", err, n, 1)
		} else if strings.Contains(line, "procs_blocked") {
			n, err = fmt.Sscanf(line, "procs_blocked %d", &c.ProcsBlocked)
			checkSscanf("ctxt", err, n, 1)
		}
	}

	return nil
}

func (c *cpuMeasure) String() string {
	var str string

	for i, core := range c.cores {
		str += fmt.Sprintf("CPU%d: %v\n", i, core)
	}

	return str + fmt.Sprintf("SwitchCtxt: %d\n\n", c.SwitchCtxt)
}
