package cmd

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/kendaganio/aoc/2023/magic"
	"github.com/spf13/cobra"
)

type Game struct {
	id int
	r  int
	g  int
	b  int
}

func (g Game) Power() int {
	return g.r * g.g * g.b
}

func NewGamePart1(line string) (Game, error) {
	newGame := Game{}

	tokens := strings.Split(line, ":")
	gameId, err := strconv.Atoi(strings.Split(tokens[0], " ")[1])
	if err != nil {
		panic("whoops")
	}
	newGame.id = gameId

	rounds := strings.Split(tokens[1], ";")

	for _, round := range rounds {
		samples := strings.Split(round, ",")
		for _, sample := range samples {
			s := strings.Split(strings.Trim(sample, " "), " ")
			count, err := strconv.Atoi(s[0])
			if err != nil {
				panic("whoops")
			}

			if s[1] == "red" {
				if count > 12 {
					return newGame, fmt.Errorf("more than 12 red shown")
				}

				newGame.r += count
			}

			if s[1] == "green" {
				if count > 13 {
					return newGame, fmt.Errorf("more than 13 blue shown")
				}

				newGame.g += count
			}

			if s[1] == "blue" {
				if count > 14 {
					return newGame, fmt.Errorf("more than 14 blue shown")
				}

				newGame.b += count
			}

		}
	}

	return newGame, nil
}

func NewGamePart2(line string) Game {
	newGame := Game{}

	tokens := strings.Split(line, ":")
	gameId, err := strconv.Atoi(strings.Split(tokens[0], " ")[1])
	if err != nil {
		panic("whoops")
	}
	newGame.id = gameId

	rounds := strings.Split(tokens[1], ";")

	for _, round := range rounds {
		samples := strings.Split(round, ",")
		for _, sample := range samples {
			s := strings.Split(strings.Trim(sample, " "), " ")
			count, err := strconv.Atoi(s[0])
			if err != nil {
				panic("whoops")
			}

			if s[1] == "red" {
				if count > newGame.r {
					newGame.r = count
				}
			}

			if s[1] == "green" {
				if count > newGame.g {
					newGame.g = count
				}

			}

			if s[1] == "blue" {
				if count > newGame.b {
					newGame.b = count
				}
			}

		}
	}

	return newGame
}

func SolveD2P1(lines []string) int {
	total := 0
	for _, line := range lines {
		g, err := NewGamePart1(line)
		if err != nil {
			continue
		}

		total += g.id
	}

	return total
}

func SolveD2P2(lines []string) int {
	total := 0

	for _, line := range lines {
		g := NewGamePart2(line)
		total += g.Power()
	}

	return total
}

// Cobra stuff
var day2Cmd = &cobra.Command{
	Use:   "day2 [path/to/input]",
	Short: "Solver for day 2",
	Args:  cobra.MinimumNArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		lines := magic.Lines(args[0])

		fmt.Println("Part 1:", SolveD2P1(lines))
		fmt.Println("Part 2:", SolveD2P2(lines))
	},
}

func init() {
	rootCmd.AddCommand(day2Cmd)
}
