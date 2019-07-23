package main

import (
  "bufio"
  "os"
	"fmt"
	"github.com/docopt/docopt-go"
)

func main() {
	usage := `Checksum practice.

Usage:
  cs parity <word_size>

Performs a variety of checksum functionality. The possible commands are:
  parity    computes a parity checksum, adding a parity bit for each word
`

	arguments, _ := docopt.ParseDoc(usage)

	if _, ok := arguments["parity"]; ok {
		wordSize, _ := arguments.Int("<word_size>")
		parity(wordSize)
	}
}

func parity(wordSize int) {
  scanner := bufio.NewScanner(os.Stdin)

  for scanner.Scan() {
    line := scanner.Text()
    fmt.Println(line)
  }
}
