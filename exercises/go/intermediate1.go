package main

//
// Write a program that outputs all possibilities to put + or - or nothing
// between the numbers 1,2,…,9 (in this order) such that the result is 100. For
// example 1 + 2 + 3 - 4 + 5 + 6 + 78 + 9 = 100.
//

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func part(num int, rest []int, sum int) [][]string {
	numStr := strconv.Itoa(num)

	if (num == sum) && (len(rest) == 0) {
		return [][]string{[]string{numStr}}
	} else if len(rest) == 0 {
		return [][]string{}
	}

	// Either I choose +
	addResults := part(rest[0], rest[1:], sum - num)
	for i, r := range addResults {
		addResults[i] = append(r, "+", numStr)
	}

	// Or I choose -
	subResults := part(rest[0], rest[1:], sum + num)
	for i, r := range subResults {
		subResults[i] = append(r, "-", numStr)
	}

	// Or I choose to combine numbers
	newNum, _ := strconv.Atoi(fmt.Sprintf("%v%v", rest[0], num))
	combineResults := part(newNum, rest[1:], sum)

	results := [][]string{}
	results = append(results, addResults...)
	results = append(results, subResults...)
	results = append(results, combineResults...)

	return results
}

func sumPossibilities(min int, max int, sum int) {
	if (max < min) {
		return
	}

	nums := make([]int, max - min)
	for i := max - 1; i >= min; i-- {
		nums[max - i - 1] = i
	}

	results := part(max, nums, sum)

	for _, result := range results {
		fmt.Println(strings.Join(result, " "))
	}
}

func main() {
	args := os.Args[1:]

	min, _ := strconv.Atoi(args[0])
	max, _ := strconv.Atoi(args[1])
	sum, _ := strconv.Atoi(args[2])

	sumPossibilities(min, max, sum)
}
