package cmd

import (
	"fmt"
	"time"

	"github.com/kendaganio/aoc/2023/magic"
	"github.com/spf13/cobra"
	"golang.org/x/exp/maps"
)

func visit(grid [][]rune, p Point, visited map[Point]bool) map[Point]bool {
	dirs := []Point{
		{0, -1},
		{0, 1},
		{-1, 0},
		{1, 0},
	}

	nextPoints := make(map[Point]bool)

	for _, d := range dirs {
		np := Point{p.X + d.X, p.Y + d.Y}

		if np.X < 0 || np.Y < 0 || np.X >= len(grid[0])-1 || np.Y >= len(grid)-1 {
			continue
		}

		if grid[np.Y][np.X] == '.' || grid[np.Y][np.X] == 'S' {
			visited[np] = true
			nextPoints[np] = true
		}

	}

	return nextPoints
}

func visit2(grid [][]rune, p Point, visited map[Point]bool) map[Point]bool {
	dirs := []Point{
		{0, -1},
		{0, 1},
		{-1, 0},
		{1, 0},
	}

	nextPoints := make(map[Point]bool)

	mod := func(d int, m int) int {
		d %= m
		if d < 0 {
			d += m
		}
		return d
	}

	for _, d := range dirs {
		np := Point{p.X + d.X, p.Y + d.Y}
		vp := np

		vp.X = mod(np.X, len(grid[0]))
		vp.Y = mod(np.Y, len(grid))

		if grid[vp.Y][vp.X] == '.' || grid[vp.Y][vp.X] == 'S' {
			visited[np] = true
			nextPoints[np] = true
		}
	}

	return nextPoints
}

func SolveD21P1(grid [][]rune, start Point) (total int) {
	visited := make(map[Point]bool)

	nodes := map[Point]bool{start: true}

	for i := 0; i < 1000; i++ {
		next := map[Point]bool{}

		for _, n := range maps.Keys(nodes) {
			toVisit := visit2(grid, n, visited)

			for _, p := range maps.Keys(toVisit) {
				next[p] = true
			}
		}

		if i == 49 || i == 99 || i == 499 {
			fmt.Println("step", i, "next", len(next))
		}
		// fmt.Println(i, len(visited), len(next), next)
		nodes = next
	}

	return len(nodes)
}

var day21Cmd = &cobra.Command{
	Use:   "day21 [path/to/input]",
	Short: "Solver for day 21",
	Args:  cobra.MinimumNArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		lines := magic.Lines(args[0])

		grid := make([][]rune, len(lines))
		var init Point

		for y, line := range lines {
			grid[y] = make([]rune, len(line))

			for x, r := range line {
				grid[y][x] = r

				if r == 'S' {
					init = Point{x, y}
				}
			}
		}

		start := time.Now()
		a := SolveD21P1(grid, init)
		fmt.Println(a, time.Since(start))
	},
}

func init() {
	rootCmd.AddCommand(day21Cmd)
}
