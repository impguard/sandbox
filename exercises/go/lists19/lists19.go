package main

import (
	"os"
	"fmt"
)

// Write a function that takes a list of strings an prints them, one per line,
// in a rectangular frame. For example the list ["Hello", "World", "in", "a",
// "frame"] gets printed as:
//
// *********
// * Hello *
// * World *
// * in    *
// * a     *
// * frame *
// *********

func printString(str string, width int) {
	spacesLength := width - len(str)

	fmt.Printf("* ")
	fmt.Printf(str)
	printCharacter(" ", spacesLength)
	fmt.Printf(" *\n")
}

func printCharacter(char string, length int) {
	for i := 0; i < length; i++ {
		fmt.Printf(char)
	}
}

func main() {
	strings := os.Args[1:]

	maxLength := 0
	for _, str := range strings {
		length := len(str)

		if length > maxLength {
			maxLength = length
		}
	}

	width := maxLength + 4

	printCharacter("*", width)
	fmt.Println()
	for _, str := range strings {
		printString(str, maxLength)
	}
	printCharacter("*", width)
	fmt.Println()
}
