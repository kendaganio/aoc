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

const (
	// https://en.wiktionary.org/wiki/butaw
	Butaw = iota + 1
	Pair
	TwoPair
	Trio
	FullHouse
	Quad
	FiveOfAKind
)

type Rank int

func (s Rank) String() string {
	return [...]string{
		"Butaw", "Pair", "TwoPair", "Trio", "FullHouse", "Quad", "FiveOfAKind",
	}[s-1]
}

type Hand struct {
	Cards string
	Rank  Rank
	Bet   int
}

func NewHand(line string, withJokers bool) Hand {
	parts := strings.Split(line, " ")

	return Hand{
		Cards: parts[0],
		Rank:  getHandRank(parts[0], withJokers),
		Bet:   magic.ParseInt(parts[1]),
	}
}

func getHandRank(cards string, withJokers bool) (rank Rank) {
	groupedCards := make(map[rune]int)
	jokers := 0

	for _, card := range cards {
		if withJokers && card == 'J' {
			jokers++
		} else {
			groupedCards[card]++
		}
	}

	if jokers > 0 {
		k, _ := magic.MaxValueIn(groupedCards)
		groupedCards[k] += jokers
	}

	switch len(groupedCards) {
	case 5:
		rank = Butaw
	case 4:
		rank = Pair
	case 3:
		var v []int = maps.Values(groupedCards)
		if slices.Contains(v, 3) {
			rank = Trio
		} else {
			rank = TwoPair
		}
	case 2:
		var v []int = maps.Values(groupedCards)
		if slices.Contains(v, 4) {
			rank = Quad
		} else {
			rank = FullHouse
		}
	case 1, 0:
		rank = FiveOfAKind
	}

	return rank
}

func compareCards(ranks string, a rune, b rune) int {
	if a == b {
		return 0
	}

	return strings.IndexRune(ranks, a) - strings.IndexRune(ranks, b)
}

func getComparerFunc(ranks string) func(a Hand, b Hand) int {
	return func(a Hand, b Hand) int {
		if a.Rank == b.Rank {
			for i := range a.Cards {
				if cardCmp := compareCards(ranks, rune(a.Cards[i]), rune(b.Cards[i])); cardCmp == 0 {
					continue
				} else {
					return cardCmp
				}
			}
		}

		return int(a.Rank - b.Rank)
	}
}

func SolveD7P1(lines []string) {
	start := time.Now()
	hands := []Hand{}
	winnings := 0

	for _, line := range lines {
		hands = append(hands, NewHand(line, false))
	}

	slices.SortFunc(hands, getComparerFunc("23456789TJQKA"))

	for i, h := range hands {
		winnings += (1 + i) * h.Bet
	}

	fmt.Println("Part 1:", winnings, time.Since(start))
}

func SolveD7P2(lines []string) {
	start := time.Now()
	hands := []Hand{}
	winnings := 0

	for _, line := range lines {
		hands = append(hands, NewHand(line, true))
	}

	slices.SortFunc(hands, getComparerFunc("J23456789TQKA"))

	for i, h := range hands {
		winnings += (1 + i) * h.Bet
	}

	fmt.Println("Part 2:", winnings, time.Since(start))
}

var day7Cmd = &cobra.Command{
	Use:   "day7 [path/to/input]",
	Short: "Solver for day 7",
	Args:  cobra.MinimumNArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		lines := magic.Lines(args[0])

		SolveD7P1(lines)
		SolveD7P2(lines)
	},
}

func init() {
	rootCmd.AddCommand(day7Cmd)
}
