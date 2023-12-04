package cmd

import (
	"fmt"
	"regexp"
	"strconv"
	"unicode"

	"github.com/kendaganio/aoc/2023/magic"
	"github.com/spf13/cobra"
)

func hasValidNeighbors(lines []string, x int, y int) (valid bool, gears []string) {
	neighbors := ""

	for yOffset := -1; yOffset < 2; yOffset++ {
		for xOffset := -1; xOffset < 2; xOffset++ {
			if yOffset == 0 && xOffset == 0 {
				continue
			}

			neighbors = neighbors + string(lines[y+yOffset][x+xOffset])

			if string(lines[y+yOffset][x+xOffset]) == "*" {
				gears = append(gears, fmt.Sprintf("%v:%v", y+yOffset, x+xOffset))
			}
		}
	}

	m := regexp.MustCompile(`\.|\d`)
	neighbors = m.ReplaceAllString(neighbors, "")
	valid = len(neighbors) > 0

	return
}

func SolveD3P1(lines []string) (total int) {
	y := len(lines)
	x := len(lines[0])

	potentialPartNumber := ""
	isPartNumber := false

	for i := 1; i < y-1; i++ {
		for j := 1; j < x-1; j++ {
			char := rune(lines[i][j])

			if unicode.IsDigit(char) {
				potentialPartNumber = potentialPartNumber + string(char)
				valid, _ := hasValidNeighbors(lines, j, i)
				isPartNumber = isPartNumber || valid
			} else {
				if len(potentialPartNumber) > 0 && isPartNumber {
					convertedPartNumber, err := strconv.Atoi(potentialPartNumber)
					if err != nil {
						panic(err.Error())
					}

					total += convertedPartNumber
				}
				potentialPartNumber = ""
				isPartNumber = false
			}
		}
	}

	return
}

func SolveD3P2(lines []string) (total int) {
	y := len(lines)
	x := len(lines[0])

	gearMap := make(map[string][]int)
	potentialPartNumber := ""
	isPartNumber := false
	gear := ""

	for i := 1; i < y-1; i++ {
		for j := 1; j < x-1; j++ {
			char := rune(lines[i][j])

			if unicode.IsDigit(char) {
				potentialPartNumber = potentialPartNumber + string(char)
				valid, neighboringGears := hasValidNeighbors(lines, j, i)
				isPartNumber = isPartNumber || valid

				if len(neighboringGears) == 1 {
					gear = neighboringGears[0]
				}
			} else {
				if len(potentialPartNumber) > 0 && isPartNumber {
					convertedPartNumber, err := strconv.Atoi(potentialPartNumber)
					if err != nil {
						panic(err.Error())
					}

					if len(gear) > 0 {
						if _, ok := gearMap[gear]; !ok {
							gearMap[gear] = []int{}
						}

						gearMap[gear] = append(gearMap[gear], convertedPartNumber)
					}
				}

				gear = ""
				potentialPartNumber = ""
				isPartNumber = false
			}
		}
	}

	for _, partNumbers := range gearMap {
		if len(partNumbers) == 2 {
			total += partNumbers[0] * partNumbers[1]
		}
	}

	return total
}

// Cobra stuff
var day3Cmd = &cobra.Command{
	Use:   "day3 [path/to/input]",
	Short: "Solver for day 3",
	Args:  cobra.MinimumNArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		lines := magic.Border(magic.Lines(args[0]), ".")

		fmt.Println("Part 1:", SolveD3P1(lines))
		fmt.Println("Part 2:", SolveD3P2(lines))
	},
}

func init() {
	rootCmd.AddCommand(day3Cmd)
}
