all-y = example.stripped

default: all

all: $(all-y)

%: %.o
	zig build-exe --strip -O ReleaseSmall -femit-bin=$@ $^

%.o: %.zig
	zig build-obj --strip -O ReleaseSmall -femit-bin=$@ $^

%.stripped: %
	strip -o $@ $^

vpath %.zig src
