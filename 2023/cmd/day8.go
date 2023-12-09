package cmd

import (
	"fmt"
	"regexp"
	"slices"
	"sync"
	"time"

	"github.com/kendaganio/aoc/2023/magic"
	"github.com/spf13/cobra"
)

func SolveD8P1(moves string, network map[string][]string) {
	start := time.Now()
	steps := 0
	currentLocation := "AAA"

	for currentLocation != "ZZZ" {
		for _, m := range moves {
			if m == 'L' {
				currentLocation = network[currentLocation][0]
			} else {
				currentLocation = network[currentLocation][1]
			}
			steps++

			if currentLocation == "ZZZ" {
				break
			}
		}
	}

	fmt.Println("Part 1:", steps, time.Since(start))
}

func gcd(a, b int) int {
	for b != 0 {
		t := b
		b = a % b
		a = t
	}

	return a
}

func lcm(integers ...int) int {
	a := integers[0]
	b := integers[1]
	result := a * b / gcd(a, b)

	for i := 2; i < len(integers); i++ {
		result = lcm(result, integers[i])
	}

	return result
}

func SolveD8P2Loop(moves string, network map[string][]string, ogLocations []string) {
	locations := slices.Clone(ogLocations)
	start := time.Now()
	stepCounts := []int{0, 0, 0, 0, 0, 0}

	for i := range locations {
		for locations[i][2] != 'Z' {
			for _, m := range moves {
				if m == 'L' {
					locations[i] = network[locations[i]][0]
				} else {
					locations[i] = network[locations[i]][1]
				}
				stepCounts[i]++
			}
		}
	}

	fmt.Println("Part 2 (loop):", lcm(stepCounts...), time.Since(start))
}

func SolveD8P2GoRoutine(moves string, network map[string][]string, ogLocations []string) {
	locations := slices.Clone(ogLocations)
	start := time.Now()
	stepCounts := []int{0, 0, 0, 0, 0, 0}

	var wg sync.WaitGroup
	for i := range locations {
		wg.Add(1)

		go func(location string, count *int) {
			defer wg.Done()

			for location[2] != 'Z' {
				for _, m := range moves {
					if m == 'L' {
						location = network[location][0]
					} else {
						location = network[location][1]
					}
					(*count) += 1
				}
			}
		}(locations[i], &stepCounts[i])
	}

	wg.Wait()
	fmt.Println("Part 2 (goroutine):", lcm(stepCounts...), time.Since(start))
}

var day8Cmd = &cobra.Command{
	Use:   "day8 [path/to/input]",
	Short: "Solver for day 8",
	Args:  cobra.MinimumNArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		lines := magic.Lines(args[0])

		network := make(map[string][]string)
		locations := []string{}

		for _, line := range lines[2:] {
			m := regexp.MustCompile(`(\w+) = \((\w+), (\w+)\)`)
			found := m.FindStringSubmatch(line)

			network[found[1]] = []string{found[2], found[3]}

			if found[1][2] == 'A' {
				locations = append(locations, found[1])
			}
		}

		SolveD8P1(lines[0], network)
		SolveD8P2Loop(lines[0], network, locations)
		SolveD8P2GoRoutine(lines[0], network, locations)
	},
}

func init() {
	rootCmd.AddCommand(day8Cmd)
}
