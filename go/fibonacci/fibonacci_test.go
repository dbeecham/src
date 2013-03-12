package fibonacci

import (
	"testing"
)

func TestGenerator(t *testing.T) {
    nextFib := Generator()
	for _, expect := range []int{1, 2, 3, 5, 8, 12, 21} {
        if nextFib() != expect {
            t.Fail();
        }
	}
}
