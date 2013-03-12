package main

import "fmt"

func incrementer() func() int {
    x := 0
    return func() int {
        x += 1
        return x
    }
}

func main() {
    i := incrementer()
    fmt.Println(i())
    fmt.Println(i())
    fmt.Println(i())
    fmt.Println(i())
}
