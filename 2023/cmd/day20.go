package cmd

import (
	"fmt"
	"strings"
	"time"

	"github.com/kendaganio/aoc/2023/magic"
	"github.com/spf13/cobra"
	"golang.org/x/exp/maps"
)

type Pulse int

const (
	Low Pulse = iota
	High
	None
)

func (p Pulse) String() string {
	return []string{"Low", "High"}[p]
}

type ModuleMap map[string]Module

type Module interface {
	Send() (p Pulse, targets []string)
	Receive(from string, p Pulse)
}

type Broadcast struct {
	LastPulse Pulse
	Targets   []string
}

func (b *Broadcast) Send() (Pulse, []string) {
	return b.LastPulse, b.Targets
}

func (b *Broadcast) Receive(_ string, p Pulse) {
	b.LastPulse = p
}

type FlipFlop struct {
	Name      string
	On        bool
	LastPulse Pulse
	Targets   []string
}

func (f *FlipFlop) Send() (Pulse, []string) {
	if f.LastPulse == Low {
		f.On = !f.On

		// reset
		f.LastPulse = High

		if f.On {
			return High, f.Targets
		} else {
			return Low, f.Targets
		}
	}

	return Low, []string{}
}

func (f *FlipFlop) Receive(_ string, p Pulse) {
	f.LastPulse = p
}

type Conjunction struct {
	Name    string
	Memory  map[string]Pulse
	Sources []string
	Targets []string
}

func (c *Conjunction) Send() (Pulse, []string) {
	pulses := maps.Values(c.Memory)
	allHigh := true

	for _, p := range pulses {
		allHigh = allHigh && p == High
	}

	if allHigh {
		return Low, c.Targets
	}

	return High, c.Targets
}

func (c *Conjunction) Receive(from string, p Pulse) {
	c.Memory[from] = p
}

type ModuleInstruction struct {
	P   Pulse
	Src string
	Dst []string
}

func SolveD20P1(modmap ModuleMap) (total int) {
	counts := make(map[Pulse]int)
	cycles := make(map[string]int)

	i := 1
	for {
		modmap["broadcaster"].Receive("button", Low)
		counts[Low]++
		p, targets := modmap["broadcaster"].Send()

		instructions := []ModuleInstruction{
			{p, "broadcaster", targets},
		}

		for len(instructions) > 0 {
			ins := instructions[0]
			instructions = instructions[1:]

			if ins.P == High && (ins.Src == "js" || ins.Src == "qs" || ins.Src == "dt" || ins.Src == "ts") {
				cycles[ins.Src] = i
			}

			if len(cycles) == 4 {
				fmt.Println("Part 2:", Lcm(maps.Values(cycles)...))
				return
			}

			for _, dst := range ins.Dst {
				counts[ins.P]++

				if mod, ok := modmap[dst]; ok {
					mod.Receive(ins.Src, ins.P)
					p, targets := mod.Send()
					instructions = append(instructions, ModuleInstruction{p, dst, targets})
				}
			}
		}

		if i == 1000 {
			fmt.Println("Part 1:", counts[Low]*counts[High])
		}

		i++
	}
}

var day20Cmd = &cobra.Command{
	Use:   "day20 [path/to/input]",
	Short: "Solver for day 20",
	Args:  cobra.MinimumNArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		lines := magic.Lines(args[0])
		modmap := make(ModuleMap)

		for _, l := range lines {
			split := strings.Split(l, " -> ")
			targets := strings.Split(split[1], ", ")

			switch split[0][0] {
			case 'b':
				modmap["broadcaster"] = &Broadcast{
					Targets: targets,
				}
			case '%':
				name := split[0][1:]
				modmap[name] = &FlipFlop{
					Name:      name,
					On:        false,
					LastPulse: High,
					Targets:   targets,
				}
			case '&':
				name := split[0][1:]
				modmap[name] = &Conjunction{
					Name:    name,
					Memory:  make(map[string]Pulse),
					Targets: targets,
				}
			}
		}

		for _, l := range lines {
			split := strings.Split(l, " -> ")
			targets := strings.Split(split[1], ", ")

			source := ""
			if split[0][0] == 'b' {
				source = "broadcaster"
			} else {
				source = split[0][1:]
			}

			for _, t := range targets {
				if module, ok := modmap[t]; ok {
					switch module.(type) {
					case *Conjunction:
						con := module.(*Conjunction)
						con.Sources = append(con.Sources, source)
						con.Memory[source] = Low
					}
				}

			}
		}

		start := time.Now()
		SolveD20P1(modmap)
		fmt.Println(time.Since(start))
	},
}

func init() {
	rootCmd.AddCommand(day20Cmd)
}
