package magic

import (
	"os"
	"strconv"
	"strings"
)

func ParseInt(s string) (i int) {
	i, err := strconv.Atoi(s)
	if err != nil {
		panic("not a number")
	}
	return
}

func Lines(filePath string) []string {
	bytes, err := os.ReadFile(filePath)
	if err != nil {
		panic(err)
	}

	lines := strings.Split(string(bytes), "\n")
	return lines[:len(lines)-1]
}

func Read(filePath string) string {
	bytes, err := os.ReadFile(filePath)
	if err != nil {
		panic(err)
	}

	return string(bytes)
}

func Border(lines []string, borderChar string) []string {
	y := len(lines)
	x := len(lines[0])

	newLine := strings.Repeat(borderChar, x+2)
	borderedLines := []string{newLine}

	for i := 0; i < y; i++ {
		borderedLines = append(borderedLines, borderChar+lines[i]+borderChar)
	}

	borderedLines = append(borderedLines, newLine)

	return borderedLines
}
