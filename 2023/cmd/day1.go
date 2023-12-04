package cmd

import (
	"fmt"

	"github.com/kendaganio/aoc/2023/magic"
	"github.com/spf13/cobra"
	"regexp"
	"slices"
	"strconv"
	"strings"
)

func SolveD1P1(lines []string) int {
	result := 0

	numInts := []string{"1", "2", "3", "4", "5", "6", "7", "8", "9"}
	firstMatcher := regexp.MustCompile(`(` + strings.Join(numInts, "|") + `)`)
	lastMatcher := regexp.MustCompile(`.*(` + firstMatcher.String() + `)`)

	for _, line := range lines {
		first := firstMatcher.FindStringSubmatch(line)[1]
		last := lastMatcher.FindStringSubmatch(line)[1]

		calibrationValue, err := strconv.Atoi(first + last)
		if err != nil {
			panic(err)
		}

		result += calibrationValue
	}

	return result
}

func SolveD1P2(lines []string) int {
	result := 0

	numInts := []string{"1", "2", "3", "4", "5", "6", "7", "8", "9"}
	numStrings := []string{"one", "two", "three", "four", "five", "six", "seven", "eight", "nine"}
	numAll := append(numInts, numStrings...)

	firstMatcher := regexp.MustCompile(`(` + strings.Join(numAll, "|") + `)`)
	lastMatcher := regexp.MustCompile(`.*(` + firstMatcher.String() + `)`)

	for _, line := range lines {
		first := firstMatcher.FindStringSubmatch(line)[1]
		last := lastMatcher.FindStringSubmatch(line)[1]

		firstValue := slices.Index(numAll, first)%9 + 1
		secondValue := slices.Index(numAll, last)%9 + 1

		calibrationValue := firstValue*10 + secondValue
		result += calibrationValue
	}

	return result
}

// Cobra stuff
var day1Cmd = &cobra.Command{
	Use:   "day1 [path/to/input]",
	Short: "Solver for day 1",
	Args:  cobra.MinimumNArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		lines := magic.Lines(args[0])

		fmt.Println("Part 1:", SolveD1P1(lines))
		fmt.Println("Part 2:", SolveD1P2(lines))
	},
}

func init() {
	rootCmd.AddCommand(day1Cmd)
}
