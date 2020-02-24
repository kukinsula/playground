package metric

import (
	"bufio"
	"encoding/json"
	"fmt"
	"log"
	"os"
	"sort"
	"strconv"
	"strings"
)

const (
	dev           = "/proc/net/dev"
	netOutputFile = "net"
	nbNetColumns  = 16
)

type Network struct {
	measures     map[string]*networkInterface
	lastMeasures map[string]*networkInterface
	names        []string
}

type networkInterface struct {
	Name     string              `json:"name"`
	Download kbyte               `json:"download"`
	Upload   kbyte               `json:"upload"`
	Measure  [nbNetColumns]int64 `json:"-"`
}

func NewNetwork() (*Network, error) {
	return &Network{
		measures:     make(map[string]*networkInterface),
		lastMeasures: make(map[string]*networkInterface),
	}, nil
}

func (n *Network) Update() error {
	n.lastMeasures = n.measures
	n.measures = make(map[string]*networkInterface)

	file, err := os.Open(dev)
	if err != nil {
		return err
	}
	defer file.Close()

	var data [nbNetColumns]int64
	var interfaceName string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()

		if strings.Contains(line, ":") {
			fields := strings.Fields(line)
			interfaceName = fields[0][:len(fields[0])-1]

			for i := 0; i < len(data); i++ {
				data[i], err = strconv.ParseInt(fields[i+1], 10, 0)
				if err != nil {
					log.Fatal(err)
				}
			}

			n.measures[interfaceName] = &networkInterface{
				interfaceName, 0.0, 0.0, data,
			}
		}
	}

	n.computeNetworkSpeed()

	if n.names == nil {
		n.names = make([]string, 0, len(n.measures))

		for name, _ := range n.measures {
			n.names = append(n.names, name)
		}

		sort.Strings(n.names)
	}

	return nil
}

func (n *Network) computeNetworkSpeed() {
	for k, _ := range n.measures {
		if n.lastMeasures[k] != nil {
			n.measures[k].Download = kbyte(
				n.measures[k].Measure[0]-n.lastMeasures[k].Measure[0]) / 1024

			n.measures[k].Upload = kbyte(
				n.measures[k].Measure[9]-n.lastMeasures[k].Measure[9]) / 1024
		}
	}
}

func (n *Network) MarshalCSV() ([]byte, error) {
	str := ""
	i, length := 0, len(n.measures)

	for _, v := range n.measures {
		str += fmt.Sprintf("%d,%d", v.Download, v.Upload)

		if i != length-1 {
			str += ","
		}

		i++
	}
	str += "\n"

	return []byte(str), nil
}

func (n *Network) MarshalJSON() ([]byte, error) {
	interfaces := make([]*networkInterface, 0, len(n.measures))

	for _, name := range n.names {
		interfaces = append(interfaces, n.measures[name])
	}

	return json.Marshal(interfaces)
}

func (n *Network) Name() string {
	return "net"
}

const (
	netFormat = `===== NETWORK ========================================

%s`
	interfaceFormat = `  %s:
    Download: %s/s
    Upload:   %s/s`
)

func (n *Network) String() string {
	interfaces := ""
	for idx, name := range n.names {
		interfaces += fmt.Sprintf(interfaceFormat,
			n.measures[name].Name,
			n.measures[name].Download,
			n.measures[name].Upload)

		if idx < len(n.names)-1 {
			interfaces += "\n\n"
		}
	}

	return fmt.Sprintf(netFormat, interfaces)
}

func isInterface(str string) bool {
	valid := false

	switch str {
	case "wlan0", "l0":
		valid = true
	}

	return valid
}
