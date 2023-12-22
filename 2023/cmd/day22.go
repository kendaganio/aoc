package cmd

import (
	"fmt"
	"strings"
	"time"

	mapset "github.com/deckarep/golang-set/v2"
	"github.com/kendaganio/aoc/2023/magic"
	"github.com/oleiade/lane/v2"
	"github.com/spf13/cobra"
	"golang.org/x/exp/maps"
)

type ThreeDPoint struct {
	X int
	Y int
	Z int
}

type Brick struct {
	ID    int
	Start ThreeDPoint
	End   ThreeDPoint
}

func NewBrick(raw string, id int) Brick {
	split := strings.Split(raw, "~")
	startRaw := magic.SplitToInts(split[0], ",")
	endRaw := magic.SplitToInts(split[1], ",")

	return Brick{
		ID:    id,
		Start: ThreeDPoint{startRaw[0], startRaw[1], startRaw[2]},
		End:   ThreeDPoint{endRaw[0], endRaw[1], endRaw[2]},
	}
}

func (b Brick) Drop() Brick {
	return Brick{
		ID:    b.ID,
		Start: ThreeDPoint{b.Start.X, b.Start.Y, b.Start.Z - 1},
		End:   ThreeDPoint{b.End.X, b.End.Y, b.End.Z - 1},
	}
}

type HyperPlane map[ThreeDPoint]int

func (hp *HyperPlane) Insert(b Brick) {
	for x := b.Start.X; x <= b.End.X; x++ {
		for y := b.Start.Y; y <= b.End.Y; y++ {
			for z := b.Start.Z; z <= b.End.Z; z++ {
				(*hp)[ThreeDPoint{x, y, z}] = b.ID
			}
		}
	}
}

func (hp *HyperPlane) Remove(b Brick) {
	for x := b.Start.X; x <= b.End.X; x++ {
		for y := b.Start.Y; y <= b.End.Y; y++ {
			for z := b.Start.Z; z <= b.End.Z; z++ {
				delete(*hp, ThreeDPoint{x, y, z})
			}
		}
	}
}

func (hp HyperPlane) HasCollision(b Brick) (id []int, hit bool) {
	hits := map[int]bool{}

	for x := b.Start.X; x <= b.End.X; x++ {
		for y := b.Start.Y; y <= b.End.Y; y++ {
			for z := b.Start.Z; z <= b.End.Z; z++ {

				if id, ok := hp[ThreeDPoint{x, y, z}]; ok {
					hits[id] = true
				}

			}
		}
	}

	// hits the floor

	if len(hits) > 0 {
		return maps.Keys(hits), true
	}

	if b.Start.Z <= 1 {
		return []int{}, true
	} else {
		return []int{}, false
	}
}

type Dep struct {
	Supports    []int
	SupportedBy []int
}

func findAllSupportedBricks(depGraph map[int]Dep, id int) mapset.Set[int] {
	falling := mapset.NewSet[int]()
	falling.Add(id)
	q := []int{id}

	for len(q) > 0 {
		nextID := q[0]
		q = q[1:]

		if deps, ok := depGraph[nextID]; ok {
			for _, s := range deps.Supports {
				if supDep, ok := depGraph[s]; ok {
					supportedBySet := mapset.NewSet[int](supDep.SupportedBy...)

					if supportedBySet.Difference(falling).Cardinality() < 1 {
						if !falling.Contains(s) {
							falling.Add(s)
							q = append(q, s)
						}
					}
				}
			}
		}
	}

	return falling
}

func SolveD22P1(lines []string) (total int) {
	q := lane.NewMinPriorityQueue[Brick, int]()
	hp := make(HyperPlane)
	dropped := []Brick{}
	depGraph := map[int]Dep{}

	for i, l := range lines {
		b := NewBrick(l, i)
		q.Push(b, b.Start.Z)
	}

	for !q.Empty() {
		b, _, _ := q.Pop()

		for {
			if hits, collided := hp.HasCollision(b.Drop()); collided {
				if _, ok := depGraph[b.ID]; !ok {
					depGraph[b.ID] = Dep{[]int{}, []int{}}
				}

				for _, i := range hits {
					if _, ok := depGraph[i]; !ok {
						depGraph[i] = Dep{[]int{}, []int{}}
					}

					if g, ok := depGraph[b.ID]; ok {
						g.SupportedBy = append(g.SupportedBy, i)
						depGraph[b.ID] = g
					}

					if g, ok := depGraph[i]; ok {
						g.Supports = append(g.Supports, b.ID)
						depGraph[i] = g
					}
				}

				break
			}

			b = b.Drop()
		}

		hp.Insert(b)
		dropped = append(dropped, b)
	}

	zappable := 0
	supported := 0
	for brickID := 0; brickID < len(dropped); brickID++ {
		if deps, ok := depGraph[brickID]; ok {
			for _, supportedBrick := range deps.Supports {
				if len(depGraph[supportedBrick].SupportedBy)-1 == 0 {
					zappable++
					supported += findAllSupportedBricks(depGraph, brickID).Cardinality() - 1
					break
				}
			}
		}

	}

	fmt.Println("Part 2:", supported)
	return len(dropped) - zappable
}

var day22Cmd = &cobra.Command{
	Use:   "day22 [path/to/input]",
	Short: "Solver for day 22",
	Args:  cobra.MinimumNArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		lines := magic.Lines(args[0])

		start := time.Now()
		a := SolveD22P1(lines)
		fmt.Println(a, time.Since(start))
	},
}

func init() {
	rootCmd.AddCommand(day22Cmd)
}
