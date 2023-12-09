package cmd

import (
	"fmt"
	"time"

	"github.com/kendaganio/aoc/2023/magic"
	"github.com/spf13/cobra"
)

func getNextValue(nums []int) int {
	isAllZeroes := true
	out := []int{}

	for i := 0; i < len(nums)-1; i++ {
		nextNum := nums[i+1] - nums[i]
		out = append(out, nextNum)

		isAllZeroes = isAllZeroes && nextNum == 0
	}

	if isAllZeroes {
		return nums[len(nums)-1]
	} else {
		return nums[len(nums)-1] + getNextValue(out)
	}
}

func getPrevValue(nums []int) int {
	isAllZeroes := true
	out := []int{}

	for i := 0; i < len(nums)-1; i++ {
		prevNum := nums[i+1] - nums[i]
		out = append(out, prevNum)

		isAllZeroes = isAllZeroes && prevNum == 0
	}

	if isAllZeroes {
		return nums[0]
	} else {
		return nums[0] - getPrevValue(out)
	}
}

func SolveD9P1(lines []string) int {
	total := 0

	for _, line := range lines {
		nums := magic.SplitAndParseInt(line, " ")
		total += getNextValue(nums)
	}

	return total
}

func SolveD9P2(lines []string) int {
	total := 0

	for _, line := range lines {
		nums := magic.SplitAndParseInt(line, " ")
		total += getPrevValue(nums)
	}

	return total
}

var day9Cmd = &cobra.Command{
	Use:   "day9 [path/to/input]",
	Short: "Solver for day 9",
	Args:  cobra.MinimumNArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		lines := magic.Lines(args[0])

		start := time.Now()
		fmt.Println("Part 1:", SolveD9P1(lines), time.Since(start))

		start = time.Now()
		fmt.Println("Part 2:", SolveD9P2(lines), time.Since(start))
	},
}

func init() {
	rootCmd.AddCommand(day9Cmd)
}
