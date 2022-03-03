all-y = example.stripped

default: all

all: $(all-y)

example: example.o

%: %.zig
	zig build-exe -O Debug -femit-bin=$@ $^

%.stripped: %
	strip -o $@ $^

vpath %.zig src
