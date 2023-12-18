package cmd

import (
	"fmt"
	"time"

	"github.com/kendaganio/aoc/2023/magic"
	"github.com/spf13/cobra"
)

type Direction int

func (d Direction) String() string {
	return []string{"Up", "Down", "Left", "Right"}[d-1]
}

const (
	Up = iota + 1
	Down
	Left
	Right
	Unknown
)

func travel(d Direction, p Point, grid map[Point]rune, visited map[Point]map[Direction]bool) {
	hops := []Direction{}

	if object, ok := grid[p]; ok {
		switch object {
		case 'x':
			return
		case '.':
			hops = append(hops, d)
		case '|':
			switch d {
			case Up, Down:
				hops = append(hops, d)
			case Left, Right:
				hops = append(hops, Up, Down)
			}
		case '-':
			switch d {
			case Left, Right:
				hops = append(hops, d)
			case Up, Down:
				hops = append(hops, Left, Right)
			}
		case '/':
			switch d {
			case Up:
				hops = append(hops, Right)
			case Down:
				hops = append(hops, Left)
			case Left:
				hops = append(hops, Down)
			case Right:
				hops = append(hops, Up)
			}
		case '\\':
			switch d {
			case Up:
				hops = append(hops, Left)
			case Down:
				hops = append(hops, Right)
			case Left:
				hops = append(hops, Up)
			case Right:
				hops = append(hops, Down)
			}
		}
	}

	if _, ok := visited[p]; !ok {
		visited[p] = make(map[Direction]bool)
	}

	if _, ok := visited[p][d]; ok {
		return
	} else {
		visited[p][d] = true
	}

	for _, d := range hops {
		switch d {
		case Up:
			travel(Up, Point{p.X, p.Y - 1}, grid, visited)
		case Down:
			travel(Down, Point{p.X, p.Y + 1}, grid, visited)
		case Left:
			travel(Left, Point{p.X - 1, p.Y}, grid, visited)
		case Right:
			travel(Right, Point{p.X + 1, p.Y}, grid, visited)
		}
	}
}

func SolveD16P1(grid map[Point]rune) (total int) {
	start := Point{1, 1}
	visited := make(map[Point]map[Direction]bool)

	travel(Right, start, grid, visited)

	return len(visited)
}

func SolveD16P2(lines []string, grid map[Point]rune) (total int) {
	out := make(chan int)

	for y := range lines {
		go func(y int) {
			start := Point{1, y}
			visited := make(map[Point]map[Direction]bool)
			travel(Right, start, grid, visited)

			out <- len(visited)
		}(y)

		go func(y int) {
			start := Point{len(lines[0]) - 2, y}
			visited := make(map[Point]map[Direction]bool)
			travel(Left, start, grid, visited)

			out <- len(visited)
		}(y)
	}

	for x := range lines[0] {
		go func(x int) {
			start := Point{x, 1}
			visited := make(map[Point]map[Direction]bool)
			travel(Down, start, grid, visited)
			out <- len(visited)
		}(x)

		go func(x int) {
			start := Point{x, len(lines) - 2}
			visited := make(map[Point]map[Direction]bool)
			travel(Up, start, grid, visited)
			out <- len(visited)
		}(x)
	}

	for i := 0; i < (len(lines)+len(lines[0]))*2; i++ {
		total = max(total, <-out)
	}

	return total
}

var day16Cmd = &cobra.Command{
	Use:   "day16 [path/to/input]",
	Short: "Solver for day 16",
	Args:  cobra.MinimumNArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		grid := map[Point]rune{}
		lines := magic.Border(magic.Lines(args[0]), "x")

		for y := range lines {
			for x := range lines[0] {
				grid[Point{x, y}] = rune(lines[y][x])
			}
		}

		start := time.Now()
		a := SolveD16P1(grid)
		fmt.Println("Part 1:", a, time.Since(start))

		start = time.Now()
		b := SolveD16P2(lines, grid)
		fmt.Println("Part 2:", b, time.Since(start))
	},
}

func init() {
	rootCmd.AddCommand(day16Cmd)
}
