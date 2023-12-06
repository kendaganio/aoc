package cmd

import (
	"fmt"
	"strings"
	"time"

	"github.com/kendaganio/aoc/2023/magic"
	"github.com/spf13/cobra"
)

func SolveD6P1(lines []string) {
	start := time.Now()

	times := parseLine(lines[0])
	distances := parseLine(lines[1])

	total := 1

	for i, t := range times {
		targetDistance := distances[i]
		wins := 0

		for e := 1; e < t; e++ {
			distanceTraveled := e * (t - e)
			if distanceTraveled > targetDistance {
				wins++
			}
		}

		total *= wins

	}

	fmt.Println("Part 1:", total, time.Since(start))
}

func SolveD6P2(lines []string) {
	start := time.Now()
	numStrings := strings.ReplaceAll(strings.Split(lines[0], ":")[1], " ", "")
	distanceStrings := strings.ReplaceAll(strings.Split(lines[1], ":")[1], " ", "")

	t := magic.ParseInt(numStrings)
	targetDistance := magic.ParseInt(distanceStrings)

	wins := 0

	for e := 1; e < t; e++ {
		distanceTraveled := e * (t - e)
		if distanceTraveled > targetDistance {
			wins++
		}
	}

	fmt.Println("Part 1:", wins, time.Since(start))
}

func parseLine(s string) []int {
	numStrings := strings.Fields(strings.Split(s, ":")[1])
	nums := []int{}

	for _, numS := range numStrings {
		nums = append(nums, magic.ParseInt(numS))
	}

	return nums
}

// Cobra stuff
var day6Cmd = &cobra.Command{
	Use:   "day6 [path/to/input]",
	Short: "Solver for day 6",
	Args:  cobra.MinimumNArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		lines := magic.Lines(args[0])

		SolveD6P1(lines)
		SolveD6P2(lines)
	},
}

func init() {
	rootCmd.AddCommand(day6Cmd)
}
