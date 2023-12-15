package cmd

import (
	"fmt"
	"slices"
	"strings"
	"time"

	"github.com/kendaganio/aoc/2023/magic"
	"github.com/spf13/cobra"
	"golang.org/x/exp/maps"
)

func hash(s string) (v int) {
	for _, r := range s {
		v = (v + int(r)) * 17 % 256
	}

	return v
}

func SolveD15P1(lines []string) (total int) {
	for _, l := range lines {
		total += hash(l)
	}

	return total
}

type Lens struct {
	label    string
	focalLen int
}

func NewLens(l string) Lens {
	lens := Lens{}

	if l[len(l)-1] == '-' {
		lens.label = string(l[:len(l)-1])
	} else {
		split := strings.Split(l, "=")
		lens.label = split[0]
		lens.focalLen = magic.ParseInt(split[1])
	}

	return lens
}

func SolveD15P2(lines []string) (total int) {
	boxes := make(map[int][]Lens)

	for _, l := range lines {
		var op rune
		lens := NewLens(l)
		boxN := hash(lens.label)

		if l[len(l)-1] == '-' {
			op = 'd'
		} else {
			op = 'i'
		}

		if _, ok := boxes[boxN]; !ok {
			boxes[boxN] = []Lens{}
		}

		if box, ok := boxes[boxN]; ok {
			if op == 'i' {
				found := slices.IndexFunc(box, func(v Lens) bool {
					return v.label == lens.label
				})

				if found > -1 {
					box[found] = lens
				} else {
					boxes[boxN] = append(box, lens)
				}
			} else {
				found := slices.IndexFunc(box, func(v Lens) bool {
					return v.label == lens.label
				})

				if found > -1 {
					boxes[boxN] = slices.Delete(box, found, found+1)
				}
			}
		}

	}

	for _, k := range maps.Keys(boxes) {
		for i, lens := range boxes[k] {
			total += lens.focalLen * (k + 1) * (i + 1)
		}

	}

	return total
}

var day15Cmd = &cobra.Command{
	Use:   "day15 [path/to/input]",
	Short: "Solver for day 15",
	Args:  cobra.MinimumNArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		lines := strings.Split(strings.TrimSpace(magic.Read(args[0])), ",")

		start := time.Now()
		a := SolveD15P1(lines)
		fmt.Println("Part 1:", a, time.Since(start))

		start = time.Now()
		b := SolveD15P2(lines)
		fmt.Println("Part 2:", b, time.Since(start))
	},
}

func init() {
	rootCmd.AddCommand(day15Cmd)
}
