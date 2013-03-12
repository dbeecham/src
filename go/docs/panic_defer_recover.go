package main

import "fmt"

func main() {
    // Run this before returning.
    defer func() {
        str := recover()
        fmt.Println(str)
    }()

    panic("should be recovered.")

    fmt.Println("Unreachable code.")
}
