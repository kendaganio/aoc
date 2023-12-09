package cmd

import (
	"fmt"
	"math"
	"strings"
	"time"

	"github.com/kendaganio/aoc/2023/magic"
	"github.com/spf13/cobra"
)

type Range struct {
	Start int
	End   int
}

func (a Range) Overlaps(b Range) bool {
	return math.Max(float64(a.End), float64(b.End))-math.Min(float64(a.Start), float64(b.Start)) < (float64(a.End)-float64(a.Start))+(float64(b.End)-float64(b.Start))
}

func (a Range) Intersection(b Range) Range {
	return Range{
		Start: int(math.Max(float64(a.Start), float64(b.Start))),
		End:   int(math.Min(float64(a.End), float64(b.End))),
	}
}

type Mapper struct {
	Range  Range
	Offset int
}

func (m Mapper) Convert(i int) int {
	return i + m.Offset
}

type Step struct {
	Mappers []Mapper
}

func NewStep(raw string) (step Step) {
	nums := strings.Split(raw, ":")[1]
	lines := strings.Split(strings.TrimSpace(nums), "\n")

	for _, line := range lines {
		split := strings.Fields(line)
		dst := magic.ParseInt(string(split[0]))
		src := magic.ParseInt(string(split[1]))
		rng := magic.ParseInt(string(split[2]))

		step.Mappers = append(step.Mappers, Mapper{
			Range: Range{
				Start: src,
				End:   src + rng,
			},
			Offset: dst - src,
		})
	}

	return
}

func (s Step) Convert(r Range) (out Range) {
	out = r

	for _, mapper := range s.Mappers {
		if mapper.Range.Overlaps(r) {
			out = r.Intersection(mapper.Range)
			out.Start += mapper.Offset
			out.End += mapper.Offset
			break
		}
	}

	return
}

func SolveD5P1(seeds []int, steps []Step) {
	start := time.Now()
	lowest := math.MaxInt

	for _, s := range seeds {
		convertedRange := Range{Start: s, End: s + 1}

		for _, step := range steps {
			convertedRange = step.Convert(convertedRange)
		}

		if lowest > convertedRange.Start {
			lowest = convertedRange.Start
		}
	}

	fmt.Println("Part 1:", lowest, time.Since(start))
}

func SolveD5P2(seedRanges []Range, steps []Step) {
	start := time.Now()
	lowest := math.MaxInt

	for _, sr := range seedRanges {
		convertedRange := sr

		for _, step := range steps {
			convertedRange = step.Convert(convertedRange)
		}

		if lowest > convertedRange.Start {
			lowest = convertedRange.Start
		}
	}

	fmt.Println("Part 2:", lowest, time.Since(start))
}

var day5Cmd = &cobra.Command{
	Use:   "day5 [path/to/input]",
	Short: "Solver for day 5",
	Args:  cobra.MinimumNArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		file := magic.Read(args[0])
		split := strings.Split(file, "\n\n")

		seeds := []int{}
		seedStrings := strings.Fields(strings.Split(split[0], ":")[1])
		for _, seedString := range seedStrings {
			seeds = append(seeds, magic.ParseInt(seedString))
		}

		steps := []Step{}
		for _, raw := range split[1:] {
			steps = append(steps, NewStep(raw))
		}

		seedRanges := []Range{}
		for i := 0; i < len(seeds); i += 2 {
			seedRanges = append(seedRanges, Range{Start: seeds[i], End: seeds[i] + seeds[i+1] - 1})
		}

		SolveD5P1(seeds, steps)
		SolveD5P2(seedRanges, steps)
	},
}

func init() {
	rootCmd.AddCommand(day5Cmd)
}
