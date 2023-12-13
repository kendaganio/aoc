package cmd

import (
	"fmt"
	"strings"
	"time"

	"github.com/kendaganio/aoc/2023/magic"
	"github.com/spf13/cobra"
)

func transpose(matrix [][]rune) [][]rune {
	m, n := len(matrix), len(matrix[0])
	res := make([][]rune, n)

	for j := 0; j < n; j++ {
		res[j] = make([]rune, m)
	}

	for i := 0; i < m; i++ {
		for j := 0; j < n; j++ {
			res[j][i] = matrix[i][j]
		}
	}

	return res
}

func findReflection(pattern [][]rune, tolerance int) (int, bool) {
	m, n := len(pattern), len(pattern[0])

	for y := 1; y < m; y++ {
		l, r, smears := y-1, y, 0

		for l >= 0 && r < m {
			for k := 0; k < n; k++ {
				if pattern[l][k] != pattern[r][k] {
					smears++
				}
			}

			l--
			r++
		}

		if smears == tolerance {
			return y, true
		}
	}

	return 0, false
}

func SolveD13P1(patterns [][][]rune, tolerance int) (total int) {
	for _, pattern := range patterns {
		if refLine, ok := findReflection(pattern, tolerance); ok {
			total += refLine * 100
		}

		if refLine, ok := findReflection(transpose(pattern), tolerance); ok {
			total += refLine
		}
	}

	return total
}

var day13Cmd = &cobra.Command{
	Use:   "day13 [path/to/input]",
	Short: "Solver for day 13",
	Args:  cobra.MinimumNArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		raw := strings.Split(magic.Read(args[0]), "\n\n")
		patterns := [][][]rune{}

		for _, r := range raw {
			lines := strings.Split(strings.TrimSpace(r), "\n")
			pattern := make([][]rune, len(lines))

			for i, l := range lines {
				pattern[i] = []rune(l)
			}

			patterns = append(patterns, pattern)
		}

		start := time.Now()
		a := SolveD13P1(patterns, 0)
		fmt.Println("Part 1:", a, time.Since(start))

		start = time.Now()
		b := SolveD13P1(patterns, 1)
		fmt.Println("Part 2:", b, time.Since(start))
	},
}

func init() {
	rootCmd.AddCommand(day13Cmd)
}
