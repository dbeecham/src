package main

import "fmt"

func main() {
    // Unsigned integer types:
    var x uint8 = 0
    var x uint16 = 0
    var x uint32 = 0
    var x uint64 = 0

    // Signed integer types:
    var x int8 = -1  // aka "byte"
    var x int32 = -1 // aka "rune"
    var x int64 = -1

    // Floating point types:
    var x float32 = 1.0 // "single precision"
    var x float64 = 1.0 // "double precision"

    // Complex types:
    var x complex64 = 1 + 1i
    var x complex128 = 1 + 1i

    // Booleans:
    var x bool = true
    var x bool = false

    // Strings:
    var x string = "string"

    // Machine dependent units:
    var x int = -1
    var x uint = 0
    var x uintptr
}
