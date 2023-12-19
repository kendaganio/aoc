package cmd

import (
	"fmt"
	"regexp"
	"strings"
	"time"

	"github.com/kendaganio/aoc/2023/magic"
	"github.com/spf13/cobra"
)

type Operator int

const (
	Null Operator = iota
	Gt
	Lt
)

type Part [4]int

func NewPart(s string) Part {
	part := Part{}
	cats := strings.Split(s[1:len(s)-1], ",")

	for i, cat := range cats {
		values := strings.Split(cat, "=")
		part[i] = magic.ParseInt(values[1])
	}

	return part
}

type Workflow map[string][]Rule

func NewWorkflow(lines []string) Workflow {
	wf := make(Workflow)

	for _, s := range lines {
		m := regexp.MustCompile(`(\w*)\{(.*)\}`)
		found := m.FindStringSubmatch(s)
		rawRules := strings.Split(found[2], ",")

		rules := []Rule{}
		for _, raw := range rawRules {
			rules = append(rules, NewRule(raw))
		}

		wf[found[1]] = rules
	}

	return wf
}

func (w Workflow) Run(key string, p Part) string {
	rules, ok := w[key]
	if !ok {
		return "not found"
	}

	for _, rule := range rules {
		if rule.op == Null {
			return rule.next
		}

		if rule.op == Gt && p[rule.l] > rule.r {
			return rule.next
		}

		if rule.op == Lt && p[rule.l] < rule.r {
			return rule.next
		}
	}

	return ""
}

type Rule struct {
	l    int
	r    int
	op   Operator
	next string
}

func NewRule(s string) Rule {
	m := regexp.MustCompile(`((x|m|a|s)?(>|<)?(\d*)?\:)?(\w+)`)
	found := m.FindStringSubmatch(s)

	comp := Null
	if found[3] == "<" {
		comp = Lt
	}
	if found[3] == ">" {
		comp = Gt
	}

	aa := map[string]int{
		"x": 0,
		"m": 1,
		"a": 2,
		"s": 3,
	}

	return Rule{
		l:    aa[found[2]],
		op:   comp,
		r:    magic.ParseIntWithDefault(found[4], -1),
		next: found[5],
	}
}

func SolveD19P1(wf Workflow, parts []Part) (total int) {
	for _, part := range parts {
		currentWorkflow := "in"

		for {
			currentWorkflow = wf.Run(currentWorkflow, part)

			if currentWorkflow == "A" {
				total += part[0] + part[1] + part[2] + part[3]
				break
			}

			if currentWorkflow == "R" {
				break
			}
		}
	}

	return total
}

type PartRange [4]Range

func RunRange(name string, wf Workflow, rp PartRange) []PartRange {
	rules, nextRange, valid := wf[name], rp, []PartRange{}

	if name == "R" {
		return []PartRange{}
	}

	if name == "A" {
		return []PartRange{nextRange}
	}

	for _, rule := range rules {
		in := nextRange

		if rule.op == Null {
			return append(valid, RunRange(rule.next, wf, nextRange)...)
		}

		if rule.op == Gt && nextRange[rule.l].End > rule.r {
			in[rule.l].Start = rule.r + 1
			nextRange[rule.l].End = rule.r
			valid = append(valid, RunRange(rule.next, wf, in)...)
		}

		if rule.op == Lt && nextRange[rule.l].End > rule.r {
			in[rule.l].End = rule.r - 1
			nextRange[rule.l].Start = rule.r
			valid = append(valid, RunRange(rule.next, wf, in)...)
		}
	}

	return valid
}

func SolveD19P2(wf Workflow) (total int) {
	rp := PartRange{
		{Start: 1, End: 4000},
		{Start: 1, End: 4000},
		{Start: 1, End: 4000},
		{Start: 1, End: 4000},
	}

	count := RunRange("in", wf, rp)

	for _, c := range count {
		total += (c[0].End - c[0].Start + 1) *
			(c[1].End - c[1].Start + 1) *
			(c[2].End - c[2].Start + 1) *
			(c[3].End - c[3].Start + 1)
	}

	return total
}

var day19Cmd = &cobra.Command{
	Use:   "day19 [path/to/input]",
	Short: "Solver for day 19",
	Args:  cobra.MinimumNArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		split := strings.Split(strings.TrimSpace(magic.Read(args[0])), "\n\n")

		wf := NewWorkflow(strings.Split(split[0], "\n"))

		parts := []Part{}
		for _, p := range strings.Split(split[1], "\n") {
			parts = append(parts, NewPart(p))
		}

		start := time.Now()
		a := SolveD19P1(wf, parts)
		fmt.Println("Part 1:", a, time.Since(start))

		start = time.Now()
		b := SolveD19P2(wf)
		fmt.Println("Part 2:", b, time.Since(start))
	},
}

func init() {
	rootCmd.AddCommand(day19Cmd)
}
