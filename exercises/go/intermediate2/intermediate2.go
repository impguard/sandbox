package main

import (
	"os"
	"fmt"
	"strconv"
	"math"
)

func leapYearRule(year float64) []int {
	var err float64
	var results []int

	for i := 0; i < 50; i++ {
		days := year + err
		delta := math.Ceil(days) - days

		if delta == 0 {
			return
		}


	}

	return results
}

func main() {
	args := os.Args[1:]

	if len(args) != 1 {
		fmt.Println("usage: ./intermediate2 <DAYS_IN_YEAR>")
		os.Exit(1)
	}

	year, err := strconv.ParseFloat(args[0], 64)

	if err != nil {
			fmt.Println("Make sure the year passed is a number")
	}

	leapYearRule(year)
}