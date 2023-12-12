package cmd

import (
	"fmt"
	"regexp"
	"strings"
	"time"

	"github.com/kendaganio/aoc/2023/magic"
	"github.com/spf13/cobra"
)

type Spring struct {
	pattern string
	damaged []int
}

func (s Spring) IndexUnknown() (index int) {
	return strings.Index(s.pattern, "?")
}

func (s Spring) IsValid() bool {
	reg := ""
	for _, d := range s.damaged {
		reg = fmt.Sprintf("%s\\.+#{%v}", reg, d)
	}
	reg = fmt.Sprintf("\\.*%s\\.*", reg[3:])

	found := regexp.MustCompile(reg).FindString(s.pattern)
	return len(found) == len(s.pattern)
}

func combine(s string, sizes []int, count int, cache map[string]int) (out int) {
	cacheKey := fmt.Sprintf("%v:%v:%v", s, sizes, count)

	if len(s) == 0 {
		if len(sizes) == 0 && count == 0 {
			return 1
		} else {
			return 0
		}
	}

	if cached, ok := cache[cacheKey]; ok {
		return cached
	}

	if s[0] == '?' {
		out += combine("."+s[1:], sizes, count, cache)
		out += combine("#"+s[1:], sizes, count, cache)
	}

	if s[0] == '#' && len(sizes) > 0 && sizes[0] > count {
		out += combine(s[1:], sizes, count+1, cache)
	}

	if s[0] == '.' {
		if count > 0 && len(sizes) > 0 {
			if count == sizes[0] {
				out += combine(s[1:], sizes[1:], 0, cache)
			}
		} else {
			out += combine(s[1:], sizes, count, cache)
		}

	}

	cache[cacheKey] = out
	return out
}

func (s Spring) FindValidCombinations() int {
	cache := make(map[string]int)
	return combine(s.pattern, s.damaged, 0, cache)
	// if s.IndexUnknown() > -1 {
	// 	p := s.pattern
	//
	// 	a := Spring{
	// 		pattern: strings.Replace(p, "?", ".", 1),
	// 		damaged: s.damaged,
	// 	}
	//
	// 	b := Spring{
	// 		pattern: strings.Replace(p, "?", "#", 1),
	// 		damaged: s.damaged,
	// 	}
	//
	// 	return a.FindValidCombinations() + b.FindValidCombinations()
	// } else {
	// 	if s.IsValid() {
	// 		return 1
	// 	} else {
	// 		return 0
	// 	}
	// }
}

func NewSpring(line string) Spring {
	split := strings.Split(line, " ")

	return Spring{
		pattern: split[0] + ".",
		damaged: magic.SplitToInts(split[1], ","),
	}
}

func NewSpringPart2(line string) Spring {
	split := strings.Split(line, " ")
	pattern := ""
	damaged := ""

	for i := 0; i < 5; i++ {
		pattern = pattern + split[0] + "?"
		damaged = damaged + split[1] + ","
	}

	return Spring{
		pattern: pattern[:len(pattern)-1] + ".",
		damaged: magic.SplitToInts(damaged[:len(damaged)-1], ","),
	}
}

func SolveD12P1(lines []string) (total int) {
	for _, l := range lines {
		count := NewSpring(l).FindValidCombinations()
		total += count
	}

	return total
}

func SolveD12P2(lines []string) (total int) {
	out := make(chan int, len(lines))

	for i, l := range lines {
		go func(l string, i int) {
			count := NewSpringPart2(l).FindValidCombinations()
			out <- count
		}(l, i)
	}

	for range lines {
		total += <-out
	}

	return total
}

var day12Cmd = &cobra.Command{
	Use:   "day12 [path/to/input]",
	Short: "Solver for day 12",
	Args:  cobra.MinimumNArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		lines := magic.Lines(args[0])

		start := time.Now()
		a := SolveD12P1(lines)
		fmt.Println("Part 1:", a, time.Since(start))

		start = time.Now()
		b := SolveD12P2(lines)
		fmt.Println("Part 2:", b, time.Since(start))
	},
}

func init() {
	rootCmd.AddCommand(day12Cmd)
}
