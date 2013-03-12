package main

import "fmt"

func sum(args ...int) (i int) {
	for _, n := range args {
		i += n
	}
	return
}

func main() {
	fmt.Println(sum(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
}
