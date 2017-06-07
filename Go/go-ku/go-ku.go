package main

import (
	"flag"
	"fmt"
	"os"
)

type Config struct {
	Filename string
	Kind     string
}

func main() {
	config := &Config{}

	flag.StringVar(
		&config.Filename, "file", "", "load sudoku from file")
	flag.StringVar(
		&config.Kind, "kind", "JSON", "the kind of sudoku to load: JSON")

	flag.Parse()

	sudoku, err := build(config)
	if err != nil {
		fmt.Printf("Build failed: %s", err)
		os.Exit(1)
	}

	err = start(sudoku)
	if err != nil {
		fmt.Printf("Start failed: %s", err)
		os.Exit(1)
	}
}

func start(sudoku *Sudoku) error {
	fmt.Println("Starting:\n", sudoku)
	fmt.Println("IsFinished:", sudoku.IsFinished())
	fmt.Println("IsCompletlyFilled:", sudoku.IsCompletlyFilled())

	return nil
}

func build(config *Config) (sudoku *Sudoku, err error) {
	if config.Filename != "" {
		file, err := os.Open(config.Filename)
		if err != nil {
			return nil, err
		}

		var builder Builder
		switch config.Kind {
		case "JSON", "json", "JS", "js":
			builder = NewJSONBuilder(file)
			if err != nil {
				return nil, err
			}

		default:
			return nil, fmt.Errorf("invalid sudoku kind %s", string(config.Kind))
		}

		sudoku, err = builder.Build()
		if err != nil {
			return nil, err
		}
	}

	return sudoku, nil
}
