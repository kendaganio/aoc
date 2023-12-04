package cmd

import (
	"fmt"
	"math"
	"slices"
	"strings"

	"github.com/kendaganio/aoc/2023/magic"
	"github.com/spf13/cobra"
)

func SolveD4P1(lines []string) {
	points := 0
	cards := []int{}
	wins := make(map[int]int)

	// Part 1
	for i, line := range lines {
		input := strings.Split(line, ":")
		raw := strings.Split(input[1], "|")

		cardWinners := strings.Fields(raw[0])
		cardNumbers := strings.Fields(raw[1])
		matches := 0

		for _, n := range cardNumbers {
			if slices.Contains(cardWinners, n) {
				matches++
			}
		}

		points += int(math.Pow(2, float64(matches-1)))
		wins[i] = matches
		cards = append(cards, i)
	}

	// Part 2
	i := 0
	for {
		if i >= len(cards) {
			break
		}

		card := cards[i]

		if copies, ok := wins[card]; ok {
			newCards := make([]int, copies)
			for j := range newCards {
				newCards[j] = card + j + 1
			}

			cards = append(cards, newCards...)
		}

		i++
	}

	fmt.Println("Part 1:", points)
	fmt.Println("Part 2:", len(cards))
}

var day4Cmd = &cobra.Command{
	Use:   "day4 [path/to/input]",
	Short: "Solver for day 4",
	Args:  cobra.MinimumNArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		lines := magic.Lines(args[0])

		SolveD4P1(lines)
	},
}

func init() {
	rootCmd.AddCommand(day4Cmd)
}
