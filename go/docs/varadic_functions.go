package main

import "fmt"

func greatestNumber(s ...int) (r int) {
    r = 0

    for _, val := range s {
        if r < val {
            r = val
        }
    }

    return
}

func main() {
    fmt.Println(greatestNumber(1, 2, 3, 19, 919, 1314, 12, 18))
}
