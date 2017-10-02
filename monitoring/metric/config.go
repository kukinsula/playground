package metric

import (
	"flag"
)

var DefaultConfig = &Config{
	Duration:  0,
	Sleep:     1,
	Metrics:   "",
	WebServer: ":8080",
}

type Config struct {
	Duration  int
	Sleep     int
	Metrics   string
	WebServer string
}

func NewConfig() (*Config, error) {
	config := DefaultConfig

	flag.IntVar(&config.Duration, "duration", config.Duration,
		"Monitoring duration in seconds (0 is infinite)")
	flag.IntVar(&config.Sleep, "sleep", config.Sleep,
		"Update frequency in seconds")
	flag.StringVar(&config.Metrics, "metrics", config.Metrics,
		"Metrics to monitor: cpu,mem,proc,net (comma separated)")
	flag.StringVar(&config.WebServer, "address", config.WebServer,
		"Web server address")

	flag.Parse()

	return config, nil
}
