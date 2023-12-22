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
	ID          int
	Start       ThreeDPoint
	End         ThreeDPoint
	Supports    mapset.Set[int]
	SupportedBy mapset.Set[int]
}

func NewBrick(raw string, id int) Brick {
	split := strings.Split(raw, "~")
	startRaw := magic.SplitToInts(split[0], ",")
	endRaw := magic.SplitToInts(split[1], ",")

	return Brick{
		ID:          id,
		Start:       ThreeDPoint{startRaw[0], startRaw[1], startRaw[2]},
		End:         ThreeDPoint{endRaw[0], endRaw[1], endRaw[2]},
		Supports:    mapset.NewSet[int](),
		SupportedBy: mapset.NewSet[int](),
	}
}

func (b Brick) Drop() Brick {
	return Brick{
		ID:          b.ID,
		Start:       ThreeDPoint{b.Start.X, b.Start.Y, b.Start.Z - 1},
		End:         ThreeDPoint{b.End.X, b.End.Y, b.End.Z - 1},
		Supports:    b.Supports,
		SupportedBy: b.SupportedBy,
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

	if len(hits) > 0 {
		return maps.Keys(hits), true
	}

	// hits the floor
	if b.Start.Z <= 1 {
		return []int{}, true
	} else {
		return []int{}, false
	}
}

func collapseBricksAbove(bricks map[int]Brick, brick Brick) mapset.Set[int] {
	falling := mapset.NewSet[int](brick.ID)
	q := lane.NewMinPriorityQueue[Brick, int]()
	q.Push(brick, brick.Start.Z)

	for !q.Empty() {
		b, _, _ := q.Pop()

		for _, supportedBrickId := range b.Supports.ToSlice() {
			if s, ok := bricks[supportedBrickId]; ok {

				if s.SupportedBy.Difference(falling).Cardinality() < 1 {
					falling.Add(supportedBrickId)
					q.Push(s, s.Start.Z)
				}
			}
		}
	}

	falling.Remove(brick.ID)
	return falling
}

func SolveD22P1(lines []string) (total int) {
	q := lane.NewMinPriorityQueue[Brick, int]()
	zappable, collapsed := 0, 0
	hp := make(HyperPlane)
	bricks := map[int]Brick{}

	for i, l := range lines {
		b := NewBrick(l, i)
		bricks[b.ID] = b
		q.Push(b, b.Start.Z)
	}

	for !q.Empty() {
		b, _, _ := q.Pop()

		for {
			if hits, collided := hp.HasCollision(b.Drop()); collided {
				for _, hit := range hits {
					b.SupportedBy.Add(hit)

					if below, ok := bricks[hit]; ok {
						below.Supports.Add(b.ID)
					}
				}
				break
			}
			b = b.Drop()
		}

		hp.Insert(b)
	}

	for _, brick := range maps.Values(bricks) {
		for _, supportedBrickID := range brick.Supports.ToSlice() {
			if supportedBrick, ok := bricks[supportedBrickID]; ok {
				if supportedBrick.SupportedBy.Cardinality()-1 < 1 {
					zappable++
					collapsed += collapseBricksAbove(bricks, brick).Cardinality()
					break
				}
			}
		}
	}

	fmt.Println("Part 1:", len(bricks)-zappable)
	fmt.Println("Part 2:", collapsed)
	return
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
