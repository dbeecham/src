package main

import "fmt"

func truth() int {
	return 1;
}

func callme(i int, f func() int) int {
	return i + f()
}

func main() {
	fmt.Println(callme(4, truth))
}
