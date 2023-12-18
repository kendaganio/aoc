package cmd

import (
	"fmt"
	"regexp"
	"strconv"
	"time"

	"github.com/kendaganio/aoc/2023/magic"
	"github.com/spf13/cobra"
)

type Instruction struct {
	Dir Direction
	N   int
	Hex string
}

func (i Instruction) String() string {
	return fmt.Sprintf("%s %d %s", i.Dir.String(), i.N, i.Hex)
}

func NewDirection(s string) Direction {
	switch s[0] {
	case 'R':
		return Right
	case 'U':
		return Up
	case 'D':
		return Down
	case 'L':
		return Left
	default:
		return Unknown
	}
}

func NewInstruction(s string) Instruction {
	m := regexp.MustCompile(`(R|D|L|U) (\d*) \((#.*)\)`)
	found := m.FindStringSubmatch(s)

	return Instruction{
		Dir: NewDirection(found[1]),
		N:   magic.ParseInt(found[2]),
		Hex: found[3],
	}
}

func NewInstruction2(s string) Instruction {
	m := regexp.MustCompile(`(R|D|L|U) (\d*) \(#(.*)\)`)
	found := m.FindStringSubmatch(s)

	n, err := strconv.ParseInt(found[3][:5], 16, 64)
	if err != nil {
		panic("boom")
	}

	dir := Unknown
	switch found[3][5] {
	case '0':
		dir = Right
	case '1':
		dir = Down
	case '2':
		dir = Left
	case '3':
		dir = Up
	}

	return Instruction{
		Dir: Direction(dir),
		N:   int(n),
		Hex: found[3],
	}
}

func (a Point) Add(b Point) Point {
	return Point{a.X + b.X, a.Y + b.Y}
}

func Shoelace(pts []Point) (det int) {
	for i := 0; i < len(pts)-1; i++ {
		det += (pts[i].X * pts[i+1].Y) - (pts[i+1].X * pts[i].Y)
	}

	return det / 2
}

func PickInterior(b int, area int) int {
	// A = i + (b / 2) - 1
	//
	//	A: area
	//	i: count interior points
	//	b: count boundary points
	return area - (b / 2) + 1
}

func SolveD18P1(ins []Instruction) (total int) {
	pts := 0
	start := Point{0, 0}
	holes := []Point{}

	getOffset := func(dir Direction, n int) Point {
		return map[Direction]Point{
			Up:    {0, n * -1},
			Down:  {0, n},
			Left:  {n * -1, 0},
			Right: {n, 0},
		}[dir]
	}

	for _, in := range ins {
		start = start.Add(getOffset(in.Dir, in.N))
		holes = append(holes, start)
		pts += in.N
	}

	area := Shoelace(holes)
	interior := PickInterior(pts, area)

	return interior + pts
}

var day18Cmd = &cobra.Command{
	Use:   "day18 [path/to/input]",
	Short: "Solver for day 18",
	Args:  cobra.MinimumNArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		lines := magic.Lines(args[0])

		ins := make([]Instruction, len(lines))
		ins2 := make([]Instruction, len(lines))

		for i, l := range lines {
			ins[i] = NewInstruction(l)
			ins2[i] = NewInstruction2(l)
		}

		start := time.Now()
		a := SolveD18P1(ins)
		fmt.Println("Part 1:", a, time.Since(start))

		start = time.Now()
		b := SolveD18P1(ins2)
		fmt.Println("Part 2:", b, time.Since(start))
	},
}

func init() {
	rootCmd.AddCommand(day18Cmd)
}
