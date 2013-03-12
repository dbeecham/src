/* Calculates fibonacci numbers. */
package fibonacci

// go doc about the function
func Generator() func() int {
	i0 := 0
	i1 := 1

	return func() int {
		i2 := i0 + i1
		i0 = i1
		i1 = i2

		return i2
	}
}
