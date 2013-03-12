package main

import (
	"fmt"
	"net/http"
)

type Hello struct {
	Str string
}

func (h Hello) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	fmt.Fprint(w, h.Str);
}

func main() {
	h0 := Hello{"Hey, world."}

	http.ListenAndServe("localhost:4000", h0)
}
