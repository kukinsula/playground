package main

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"sort"
	"strings"
	"time"

	"github.com/kukinsula/playground/monitoring/metric"
)

var (
	supportedMetrics    = []string{"cpu", "mem", "net"}
	nbSupprortedMetrics = len(supportedMetrics)
)

type Monitoring struct {
	config  *metric.Config
	server  *Server
	Metrics map[string]metric.Metric
}

func NewMonitoring(config *metric.Config) (*Monitoring, error) {
	var fields []string

	if config.Metrics == "" {
		// TODO : trouver mieux
		fields = supportedMetrics
	} else {
		fields = strings.Split(config.Metrics, ",")

		if len(fields) > nbSupprortedMetrics {
			return nil,
				fmt.Errorf("too much metrics: max is %d",
					nbSupprortedMetrics)
		}
	}

	// Map used to overwrite potential Metric doublon
	metrics := make(map[string]metric.Metric)

	var m metric.Metric
	var err error

	for _, field := range fields {
		switch field {
		case "cpu":
			m, err = metric.NewCPU()

		case "mem":
			m, err = metric.NewMemory()

		case "net":
			m, err = metric.NewNetwork()

		default:
			err = fmt.Errorf("invalid metric '%s'", field)
		}

		if err != nil {
			return nil, err
		}

		metrics[field] = m
	}

	server, err := NewServer(config)
	if err != nil {
		return nil, err
	}

	return &Monitoring{
		config:  config,
		server:  server,
		Metrics: metrics,
	}, nil
}

func (m *Monitoring) Start() error {
	m.server.Start()

	clear()

	names := make([]string, 0, len(m.Metrics))
	for name := range m.Metrics {
		names = append(names, name)
	}
	sort.Strings(names)

	for {
		messages := make([]*message, 0, len(names)+1)

		for idx, name := range names {
			metric := m.Metrics[name]

			err := metric.Update()
			if err != nil {
				return fmt.Errorf("metric update failed: %s", err)
			}

			fmt.Println(metric)

			if idx < len(names)-1 {
				fmt.Println()
			}

			messages = append(messages, &message{
				subject: metric.Name(),
				data:    metric,
			})
		}

		messages = append(messages, &message{
			subject: "overview",
			data:    m,
		})

		m.server.Notify(messages)
		time.Sleep(1 * time.Second)
		clear()
	}

	return nil
}

func (m *Monitoring) Stop() {
	m.server.Stop()
	// TODO: stopper aussi la boucle Monitoring.Start
}

func (m *Monitoring) MarshalJSON() ([]byte, error) {
	return json.Marshal(m.Metrics)
}

func clear() {
	cmd := exec.Command("clear")
	cmd.Stdout = os.Stdout
	cmd.Run()
}
