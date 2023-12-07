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

type Strength int

func (s Strength) String() string {
	return [...]string{
		"Butaw", "Pair", "TwoPair", "Trio", "FullHouse", "Quad", "FiveOfAKind",
	}[s-1]
}

type Hand struct {
	Cards    string
	Strength Strength
	Bet      int
}

func NewHand(line string, withJokers bool) Hand {
	parts := strings.Split(line, " ")

	return Hand{
		Cards:    parts[0],
		Bet:      magic.ParseInt(parts[1]),
		Strength: getHandStrength(parts[0], withJokers),
	}
}

func getHandStrength(cards string, withJokers bool) Strength {
	groupedCards := make(map[string]int)
	jokers := 0

	if withJokers && cards == "JJJJJ" {
		return FiveOfAKind
	}

	for _, card := range cards {
		if withJokers && card == 'J' {
			jokers++
		} else {
			groupedCards[string(card)]++
		}
	}

	if jokers > 0 {
		var chosenKey string
		maxOcc := 0

		for k, v := range groupedCards {
			if v > maxOcc {
				maxOcc = v
				chosenKey = k
			}
		}

		groupedCards[chosenKey] += jokers
	}

	switch len(groupedCards) {
	case 5: // butaw
		return Butaw
	case 4: // one pair
		return Pair
	case 1: // five
		return FiveOfAKind
	case 3: // two pair & trio
		var v []int = maps.Values(groupedCards)
		if slices.Contains(v, 3) {
			return Trio
		} else {
			return TwoPair
		}
	case 2: // full & quad
		var v []int = maps.Values(groupedCards)
		if slices.Contains(v, 4) {
			return Quad
		} else {
			return FullHouse
		}
	}

	return Butaw
}

func CompareIndividualCards(ranks string, a rune, b rune) int {
	if a == b {
		return 0
	}

	return strings.IndexRune(ranks, a) - strings.IndexRune(ranks, b)
}

func GetComparerFunc(ranks string) func(a Hand, b Hand) int {
	return func(a Hand, b Hand) int {
		if a.Strength == b.Strength {
			for i := range a.Cards {
				if cardCmp := CompareIndividualCards(ranks, rune(a.Cards[i]), rune(b.Cards[i])); cardCmp == 0 {
					continue
				} else {
					return cardCmp
				}
			}
		}

		return int(a.Strength - b.Strength)
	}
}

func SolveD7P1(lines []string) {
	start := time.Now()
	hands := []Hand{}
	winnings := 0

	for _, line := range lines {
		hands = append(hands, NewHand(line, false))
	}

	slices.SortFunc(hands, GetComparerFunc("23456789TJQKA"))

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

	slices.SortFunc(hands, GetComparerFunc("J23456789TQKA"))

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
