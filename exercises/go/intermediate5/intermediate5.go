package main

import (
	"bufio"
	"flag"
	"fmt"
	"math/rand"
	"os"
	"strconv"
	"strings"
)

type Cache map[string](map[string]int)

func (cache Cache) init(key string, other string) {
	if _, ok := cache[key]; !ok {
		cache[key] = make(map[string]int)
	}

	if _, ok := cache[key][other]; !ok {
		cache[key][other] = 0
	}
}

func (cache Cache) incr(key string, other string) {
	cache.init(key, other)
	cache[key][other] += 1
}

func (cache Cache) set(key string, other string, count int) {
	cache.init(key, other)
	cache[key][other] = count
}

func (cache Cache) sampleKey(key string) string {
	total := 0
	for _, count := range cache[key] {
		total += count
	}

	index := rand.Intn(total)
	result := "unknown"

	for word, count := range cache[key] {
		index -= count

		if index < 0 {
			result = word
			break
		}
	}

	return result
}

func (cache Cache) sample() string {
	index := rand.Intn(len(cache))

	result := "unknown"
	i := 0
	for word := range cache {
		if i == index {
			result = word
			break
		}

		i += 1
	}

	return result
}

func (cache Cache) train(filepath string) {
	file, err := os.Open(filepath)
	if err != nil {
		panic(err)
	}

	defer file.Close()

	scanner := bufio.NewScanner(file)
	scanner.Split(bufio.ScanWords)

	var prevWord string
	for scanner.Scan() {
		word := scanner.Text()

		if prevWord != "" {
			cache.incr(prevWord, word)
		}

		prevWord = word
	}
}

func (cache Cache) save(filepath string) {
	file, err := os.Create(filepath)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	builder := strings.Builder{}
	for word, counts := range cache {
		builder.Reset()
		builder.WriteString(word)
		for other, count := range counts {
			builder.WriteString(fmt.Sprintf(" %s\x1f%d", other, count))
		}
		builder.WriteString("\n")
		file.WriteString(builder.String())
	}
}

func load(filepath string) Cache {
	file, err := os.Open(filepath)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	cache := make(Cache)

	scanner := bufio.NewScanner(file)
	scanner.Split(bufio.ScanLines)

	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Split(line, " ")
		word := parts[0]

		for _, tuple := range parts[1:] {
			pair := strings.Split(tuple, "\x1f")
			other := pair[0]
			count, _ := strconv.Atoi(pair[1])

			cache.set(word, other, count)
		}
	}

	return cache
}

func (cache Cache) generate(length int) string {
	builder := strings.Builder{}

	word := cache.sample()
	for i := 0; i < length; i++ {
		builder.WriteString(fmt.Sprintf("%s ", word))

		if _, ok := cache[word]; !ok {
			word = cache.sample()
		} else {
			word = cache.sampleKey(word)
		}
	}

	return builder.String()
}

var usage string = `usage: ./intermediate5 <CMD>...args

Each subcommand has its own set of args, pass -h to see help for each
subcommand.

Commands:

train
	train a word cache. Train will augment the provided output file, delete the
	file before training to train a fresh word cache.

generate
	generate a random story given a word cache.
`

func main() {
	trainCmd := flag.NewFlagSet("train", flag.ExitOnError)
	trainInput := trainCmd.String(
		"input", "", "Input file to train on")
	trainOutput := trainCmd.String(
		"output", "", "Output file to save serialized trained output")

	generateCmd := flag.NewFlagSet("generate", flag.ExitOnError)
	generateInput := generateCmd.String(
		"input", "", "Input file to load cache set")
	generateOutput := generateCmd.String(
		"output", "", "Output file to save generated story")
	generateLength := generateCmd.Int(
		"length", 100, "Generated story length")

	if len(os.Args) < 2 {
		fmt.Println(usage)
		return
	}

	switch os.Args[1] {
	case "train":
		trainCmd.Parse(os.Args[2:])
		cache := load(*trainOutput)
		cache.train(*trainInput)
		cache.save(*trainOutput)

	case "generate":
		generateCmd.Parse(os.Args[2:])
		cache := load(*generateInput)

		file, err := os.Create(*generateOutput)
		if err != nil {
			panic(err)
		}

		file.WriteString(cache.generate(*generateLength))

	default:
		fmt.Println(usage)
	}
}
