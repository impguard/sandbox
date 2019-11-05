package main

import (
	"fmt"
	"sync"
)

// Fetcher returns the body of URL and a slice of URL's found on that page
type Fetcher interface {
	Fetch(url string) (body string, urls []string, err error)
}

// Crawl will crawl the passed url to some depth
func Crawl(url string, depth int, fetcher Fetcher) {
	if depth <= 0 {
		return
	}

	channel := make(chan content)
	cache := make(map[string]bool)

	var opts options = options{
		maxDepth: depth,
		fetcher:  fetcher,
		channel:  channel,
		cache:    cache,
	}

	opts.wg.Add(1)
	go crawl(url, 0, &opts)

	go func() {
		for c := range channel {
			if c.err != nil {
				fmt.Println(c.err)
			} else {
				fmt.Printf("found: %s %q\n", c.url, c.body)
			}
		}
	}()

	opts.wg.Wait()
	close(opts.channel)

	return
}

type content struct {
	url  string
	body string
	err  error
}

type options struct {
	maxDepth int
	fetcher  Fetcher
	channel  chan content
	wg       sync.WaitGroup
	mutex    sync.Mutex
	cache    map[string]bool
}

func (opts *options) cacheVisit(url string) bool {
	opts.mutex.Lock()
	_, didVisit := opts.cache[url]
	if !didVisit {
		opts.cache[url] = true
	}
	opts.mutex.Unlock()
	return didVisit
}

func crawl(url string, depth int, opts *options) {
	defer opts.wg.Done()

	if depth > opts.maxDepth {
		return
	}

	if opts.cacheVisit(url) {
		return
	}

	body, urls, err := opts.fetcher.Fetch(url)

	opts.channel <- content{url, body, err}

	if err != nil {
		return
	}

	for _, u := range urls {
		opts.wg.Add(1)
		go crawl(u, depth+1, opts)
	}
}

func main() {
	Crawl("https://golang.org/", 4, fetcher)
}

// fakeFetcher is Fetcher that returns canned results.
type fakeFetcher map[string]*fakeResult

type fakeResult struct {
	body string
	urls []string
}

func (f fakeFetcher) Fetch(url string) (string, []string, error) {
	if res, ok := f[url]; ok {
		return res.body, res.urls, nil
	}
	return "", nil, fmt.Errorf("not found: %s", url)
}

// fetcher is a populated fakeFetcher.
var fetcher = fakeFetcher{
	"https://golang.org/": &fakeResult{
		"The Go Programming Language",
		[]string{
			"https://golang.org/pkg/",
			"https://golang.org/cmd/",
		},
	},
	"https://golang.org/pkg/": &fakeResult{
		"Packages",
		[]string{
			"https://golang.org/",
			"https://golang.org/cmd/",
			"https://golang.org/pkg/fmt/",
			"https://golang.org/pkg/os/",
		},
	},
	"https://golang.org/pkg/fmt/": &fakeResult{
		"Package fmt",
		[]string{
			"https://golang.org/",
			"https://golang.org/pkg/",
		},
	},
	"https://golang.org/pkg/os/": &fakeResult{
		"Package os",
		[]string{
			"https://golang.org/",
			"https://golang.org/pkg/",
		},
	},
}
