package cmd

import (
	"fmt"
	"regexp"
	"slices"
	"time"

	"github.com/kendaganio/aoc/2023/magic"
	"github.com/spf13/cobra"
	"golang.org/x/exp/maps"
)

type Point struct {
	X int
	Y int
}

func traverse(grid []string, scores *map[Point]int, start Point, step int) {
	node := rune(grid[start.Y][start.X])

	if ((*scores)[start] > 0 && step > 1) || node == '.' {
		return
	}

	(*scores)[start] = step

	u := Point{start.X, start.Y - 1}
	d := Point{start.X, start.Y + 1}
	l := Point{start.X - 1, start.Y}
	r := Point{start.X + 1, start.Y}

	switch node {
	case 'S':
		traverse(grid, scores, l, step+1)
		traverse(grid, scores, r, step+1)
		traverse(grid, scores, u, step+1)
		traverse(grid, scores, d, step+1)
	case '|':
		traverse(grid, scores, u, step+1)
		traverse(grid, scores, d, step+1)
	case '-':
		traverse(grid, scores, l, step+1)
		traverse(grid, scores, r, step+1)
	case 'L':
		traverse(grid, scores, u, step+1)
		traverse(grid, scores, r, step+1)
	case 'J':
		traverse(grid, scores, u, step+1)
		traverse(grid, scores, l, step+1)
	case 'F':
		traverse(grid, scores, d, step+1)
		traverse(grid, scores, r, step+1)
	case '7':
		traverse(grid, scores, d, step+1)
		traverse(grid, scores, l, step+1)
	}
}

func SolveD10P1(grid []string, start Point) (int, map[Point]int) {
	scores := make(map[Point]int)
	scores[start] = 1

	traverse(grid, &scores, start, 1)

	return slices.Max(maps.Values(scores)) / 2, scores
}

func SolveD10P2(grid []string, scores map[Point]int) (total int) {
	for y, line := range grid {
		bars := 0

		// replace L--7 and F--J with just a single pipe
		m := regexp.MustCompile(`(L-*7)|(F-*J)`)
		matches := m.FindAllStringSubmatchIndex(line, -1)
		for _, hit := range matches {
			newLine := []rune(line)
			newLine[hit[0]] = '|'
			line = string(newLine)
		}

		for x, ch := range line {
			if _, visited := scores[Point{x, y}]; visited {
				if ch == '|' {
					bars++
				}
				continue
			} else if bars%2 == 1 {
				total++
			}
		}

	}

	return total
}

var day10Cmd = &cobra.Command{
	Use:   "day10 [path/to/input]",
	Short: "Solver for day 10",
	Args:  cobra.MinimumNArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		var startingPoint Point
		lines := magic.Border(magic.Lines(args[0]), ".")

		for y, line := range lines {
			for x, char := range line {
				if char == 'S' {
					startingPoint = Point{x, y}
					break
				}
			}
		}

		start := time.Now()
		a, scores := SolveD10P1(lines, startingPoint)
		fmt.Println("Part 1:", a, time.Since(start))

		start = time.Now()
		fmt.Println("Part 2:", SolveD10P2(lines, scores), time.Since(start))
	},
}

func init() {
	rootCmd.AddCommand(day10Cmd)
}
