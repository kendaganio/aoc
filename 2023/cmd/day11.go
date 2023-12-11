package cmd

import (
	"fmt"
	"math"
	"time"

	"github.com/kendaganio/aoc/2023/magic"
	"github.com/spf13/cobra"
)

type Galaxy struct {
	X int
	Y int
}

func computeWeights(lines []string, inc int) [][]int {
	out := make([][]int, len(lines))

	for i, line := range lines {
		newLine := make([]int, len(line))
		out[i] = newLine

		isEmpty := true
		for j, r := range line {
			out[i][j] = 1
			isEmpty = isEmpty && r == '.'
		}

		if isEmpty {
			for x := 0; x < len(out[i]); x++ {
				out[i][x] = inc
			}
		}
	}

	for x := 0; x < len(out[0]); x++ {
		isEmpty := true

		for y := 0; y < len(out); y++ {
			isEmpty = isEmpty && lines[y][x] == '.'
		}

		if isEmpty {
			for y := 0; y < len(out); y++ {
				out[y][x] = inc
			}
		}
	}

	return out
}

func djikstra(weights [][]int, src Galaxy) map[Point]int {
	visited := make(map[Point]bool)
	dist := make(map[Point]int)

	srcPoint := Point{src.X, src.Y}
	q := []Point{srcPoint}
	dist[srcPoint] = 0

	for len(q) != 0 {
		node := q[0]
		q = q[1:]

		if visited[node] || node.X < 0 || node.Y < 0 || node.X >= len(weights[0]) || node.Y >= len(weights) {
			continue
		}

		neighbors := []Point{
			{node.X + 1, node.Y},
			{node.X - 1, node.Y},
			{node.X, node.Y + 1},
			{node.X, node.Y - 1},
		}

		q = append(q, neighbors...)

		for _, next := range neighbors {
			if _, ok := dist[next]; !ok {
				dist[next] = math.MaxInt
			}

			alt := dist[node] + weights[node.Y][node.X]
			if dist[next] > alt {
				dist[next] = alt
			}
		}

		visited[node] = true
	}

	return dist
}

func SolveD11P1(weights [][]int, galaxies []Galaxy) (total int) {
	for i, f := range galaxies {
		dists := djikstra(weights, f)
		for _, s := range galaxies[i+1:] {
			total += dists[Point{s.X, s.Y}]
		}
	}

	return total
}

var day11Cmd = &cobra.Command{
	Use:   "day11 [path/to/input]",
	Short: "Solver for day 11",
	Args:  cobra.MinimumNArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		lines := magic.Lines(args[0])
		weights := computeWeights(lines, 2)
		weights2 := computeWeights(lines, 1_000_000)

		galaxies := []Galaxy{}

		for y, line := range lines {
			for x, r := range line {
				if r != '.' {
					galaxies = append(galaxies, Galaxy{x, y})
				}
			}
		}

		start := time.Now()
		a := SolveD11P1(weights, galaxies)
		fmt.Println("Part 1:", a, time.Since(start))

		start = time.Now()
		b := SolveD11P1(weights2, galaxies)
		fmt.Println("Part 2:", b, time.Since(start))
	},
}

func init() {
	rootCmd.AddCommand(day11Cmd)
}
