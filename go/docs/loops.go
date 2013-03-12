package main

import "fmt"

func main() {
    i := 1

    for ; i <= 10; i++ {
        fmt.Println(i)
    }

    i = 1

    // while
    for i <= 10 {
        fmt.Println(i)
        i += 1
    }

    // range
    x := {0, 1, 2, 3}
    for _, i := range x {
        fmt.Println(i);
    }
}
