package cmd

import (
	"fmt"
	"time"

	"github.com/kendaganio/aoc/2023/magic"
	"github.com/spf13/cobra"
)

func d(platform [][]rune) {
	for _, l := range platform {
		fmt.Println(string(l))
	}
}

func tiltN(platform [][]rune) [][]rune {
	m, n := len(platform), len(platform[0])

	for x := 0; x < n; x++ {
		emptyIndices := []int{}

		for y := 0; y < m; y++ {
			switch platform[y][x] {
			case '.':
				emptyIndices = append(emptyIndices, y)
			case '#':
				emptyIndices = []int{}
			case 'O':
				if len(emptyIndices) > 0 {
					i := emptyIndices[0]
					emptyIndices = append(emptyIndices[1:], y)

					platform[y][x] = '.'
					platform[i][x] = 'O'
				}
			}
		}
	}

	return platform
}

func tiltS(platform [][]rune) [][]rune {
	m, n := len(platform), len(platform[0])

	for x := 0; x < n; x++ {
		emptyIndices := []int{}
		for y := m - 1; y >= 0; y-- {
			switch platform[y][x] {
			case '.':
				emptyIndices = append(emptyIndices, y)
			case '#':
				emptyIndices = []int{}
			case 'O':
				if len(emptyIndices) > 0 {
					i := emptyIndices[0]
					emptyIndices = append(emptyIndices[1:], y)

					platform[y][x] = '.'
					platform[i][x] = 'O'
				}
			}
		}
	}

	return platform
}

func tiltE(platform [][]rune) [][]rune {
	m, n := len(platform), len(platform[0])
	for y := 0; y < m; y++ {
		emptyIndices := []int{}
		for x := n - 1; x >= 0; x-- {
			switch platform[y][x] {
			case '.':
				emptyIndices = append(emptyIndices, x)
			case '#':
				emptyIndices = []int{}
			case 'O':
				if len(emptyIndices) > 0 {
					i := emptyIndices[0]
					emptyIndices = append(emptyIndices[1:], x)

					platform[y][x] = '.'
					platform[y][i] = 'O'
				}
			}
		}
	}

	return platform
}

func tiltW(platform [][]rune) [][]rune {
	m, n := len(platform), len(platform[0])
	for y := 0; y < m; y++ {
		emptyIndices := []int{}
		for x := 0; x < n; x++ {
			switch platform[y][x] {
			case '.':
				emptyIndices = append(emptyIndices, x)
			case '#':
				emptyIndices = []int{}
			case 'O':
				if len(emptyIndices) > 0 {
					i := emptyIndices[0]
					emptyIndices = append(emptyIndices[1:], x)

					platform[y][x] = '.'
					platform[y][i] = 'O'
				}
			}
		}
	}

	return platform
}

func SolveD14P1(platform [][]rune) (total int) {
	platform = tiltN(platform)
	m, n := len(platform), len(platform[0])

	for y := 0; y < m; y++ {
		for x := 0; x < n; x++ {
			if platform[y][x] == 'O' {
				total += m - y
			}
		}
	}

	return total
}

func cycle(platform [][]rune) [][]rune {
	return tiltE(tiltS(tiltW(tiltN(platform))))
}

func SolveD14P2(platform [][]rune) (total int) {
	cache := make(map[string]int)
	spins := 1_000_000_000

	for i := 0; i < spins; i++ {
		platform = cycle(platform)
		cacheKey := fmt.Sprintf("%v", platform)

		if repeated, ok := cache[cacheKey]; ok {
			cycleCount := i - repeated
			remainingSpins := spins - i
			i = spins - remainingSpins%cycleCount
			cache = make(map[string]int)
		} else {
			cache[cacheKey] = i
		}

	}

	m, n := len(platform), len(platform[0])

	for y := 0; y < m; y++ {
		for x := 0; x < n; x++ {
			if platform[y][x] == 'O' {
				total += m - y
			}
		}
	}

	return total
}

var day14Cmd = &cobra.Command{
	Use:   "day14 [path/to/input]",
	Short: "Solver for day 14",
	Args:  cobra.MinimumNArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		lines := magic.Lines(args[0])
		platform := [][]rune{}

		for _, line := range lines {
			platform = append(platform, []rune(line))
		}

		start := time.Now()
		a := SolveD14P1(platform)
		fmt.Println("Part 1:", a, time.Since(start))

		start = time.Now()
		b := SolveD14P2(platform)
		fmt.Println("Part 2:", b, time.Since(start))
	},
}

func init() {
	rootCmd.AddCommand(day14Cmd)
}
