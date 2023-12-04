package cmd

import (
	"fmt"

	"github.com/kendaganio/aoc/2023/magic"
	"github.com/spf13/cobra"
)

func SolveD0P1(lines []string) int {
	return 1
}

func SolveD0P2(lines []string) int {
	return 2
}

// Cobra stuff
var day0Cmd = &cobra.Command{
	Use:   "day0 [path/to/input]",
	Short: "This does nothing, just a template: Solver for dayx",
	Args:  cobra.MinimumNArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		lines := magic.Lines(args[0])

		fmt.Println("Part 1:", SolveD0P1(lines))
		fmt.Println("Part 2:", SolveD0P2(lines))
	},
}

func init() {
	rootCmd.AddCommand(day0Cmd)
}
