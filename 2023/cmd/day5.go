package cmd

import (
	"fmt"
	"math"
	"strings"

	"github.com/kendaganio/aoc/2023/magic"
	"github.com/spf13/cobra"
)

type Range struct {
	start int
	items int
}

type Map struct {
	dst int
	src int
	rng int
}

func (m Map) Convert(v int) int {
	if v >= m.src && v < m.src+m.rng {
		return m.dst + (v - m.src)
	} else {
		return v
	}
}

func (m Map) HasValuesInRange(r Range) bool {
	// return m.src+m.rng >= r.start || m.src <= r.start+r.rng
	return true
}

func FindConvertedValue(maps []Map, v int) int {
	for _, m := range maps {
		newV := m.Convert(v)
		if newV != v {
			return newV
		}
	}

	return v
}

func ParseMapping(raw string) (maps []Map) {
	nums := strings.Split(raw, ":")[1]
	lines := strings.Split(strings.TrimSpace(nums), "\n")

	maps = []Map{}

	for _, line := range lines {
		split := strings.Fields(line)
		maps = append(maps, Map{
			dst: magic.ParseInt(string(split[0])),
			src: magic.ParseInt(string(split[1])),
			rng: magic.ParseInt(string(split[2])),
		})
	}

	return
}

func SolveD5P1(seeds []int, maps [][]Map) int {
	lowest := math.MaxInt

	for _, s := range seeds {
		convertedValue := s
		for _, m := range maps {
			convertedValue = FindConvertedValue(m, convertedValue)
		}

		if convertedValue < lowest {
			lowest = convertedValue
		}
	}

	return lowest
}

func SolveD5P2(seeds []int, maps [][]Map) int {
	lowsPerRange := make(chan int)

	seedRanges := []Range{}
	for i := 0; i < len(seeds); i += 2 {
		seedRanges = append(seedRanges, Range{start: seeds[i], items: seeds[i+1]})
	}

	for _, sr := range seedRanges {
		go func(sr Range) {
			lowest := math.MaxInt
			// fmt.Println("start:", sr)

			for s := sr.start; s < sr.start+sr.items; s++ {
				convertedValue := s
				for _, ms := range maps {
					convertedValue = FindConvertedValue(ms, convertedValue)
				}

				if convertedValue < lowest {
					lowest = convertedValue
				}
			}

			// fmt.Println("end:", sr, lowest)
			lowsPerRange <- lowest
		}(sr)
	}

	lowest := math.MaxInt
	for range seedRanges {
		lowInRange := <-lowsPerRange
		if lowInRange < lowest {
			lowest = lowInRange
		}
	}

	return lowest
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

		maps := [][]Map{}
		for _, raw := range split[1:] {
			maps = append(maps, ParseMapping(raw))
		}

		fmt.Println("Part 1:", SolveD5P1(seeds, maps))
		fmt.Println("This part is slow, around ~5 mins")
		fmt.Println("Part 2:", SolveD5P2(seeds, maps))
	},
}

func init() {
	rootCmd.AddCommand(day5Cmd)
}
