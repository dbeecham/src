# lessons:
#  * you want to --strip individual object files; hw-o-zig-stripped-nostripped takes nonstripped
#    object files and --strips at link time, which produces larger binaries

default: all


# reference c hw
all-y += hw-c
hw-c: hw_c.c
	gcc -o $@ $^

# reference c built from objects
all-y += hw-o-c
hw-o-c: hw_c.o
	gcc -o $@ $^

all-y += hw-c.stripped
hw-c.stripped: hw-c
	strip -o $@ $^

all-y += hw-c-stripped.stripped hw-c-stripped.stripped.symbols
hw-c-stripped.stripped: hw-c-stripped
	strip -o $@ $^

hw-c-stripped: hw_c.c
	gcc -Os -fomit-frame-pointer -o $@ $^

# normal build
all-y += hw-build-exe
hw-build-exe: hw.zig
	zig build-exe $^ -femit-bin=$@

# build small
all-y += hw-build-exe-stripped
hw-build-exe-stripped: hw.zig
	zig build-exe --strip -O ReleaseSmall $^ -femit-bin=$@

# link object files
all-y += hw-o-zig
hw-o-zig: hw.o
	zig build-exe $^ -femit-bin=$@

# link object files, but strip it after
all-y += hw-o-zig-stripped-nostripped
hw-o-zig-stripped-nostripped: hw.o
	zig build-exe $^ --strip -femit-bin=$@

# link stripped object files, strip after
all-y += hw-o-zig-stripped-stripped
hw-o-zig-stripped-stripped: hw-stripped.o
	zig build-exe $^ --strip -femit-bin=$@

# link stripped object files, dont strip after
all-y += hw-o-zig-nostripped-stripped
hw-o-zig-nostripped-stripped: hw-stripped.o
	zig build-exe $^ -femit-bin=$@

# use built-in c library
all-y += hw-lib-c
hw-lib-c: hw.zig
	zig build-exe --library c -femit-bin=$@ $^


# build shared object file
all-y += libhw-zig.so
libhw-zig.so: hw.zig
	zig build-lib $^ -dynamic -femit-bin=$@

# static library
all-y += libhw-zig.a
libhw-zig.a: hw.zig
	zig build-lib $^ -femit-bin=$@

# build library using object files
all-y += libhw-o.a
libhw-o.a: hw.o
	zig build-lib $^ -femit-bin=$@

# build library using stripped object files
all-y += libhw-o-stripped.a
libhw-o-stripped.a: hw-stripped.o
	zig build-lib --strip -O ReleaseSmall $^ -femit-bin=$@

# link against musl
all-y += hw-build-exe-musl
hw-build-exe-musl: hw.zig
	zig build-exe --strip -O ReleaseSmall -target x86_64-linux-musl $^ -femit-bin=$@

# also bring in own c library
all-y += hw-build-exe-musl-w-library hw-build-exe-musl-w-library.symbols
hw-build-exe-musl-w-library: hw.zig
	zig build-exe --strip -O ReleaseSmall --library c -target x86_64-linux-musl $^ -femit-bin=$@


# force dynamic linking in executable file
all-y += hw-build-exe-dynamic hw-build-exe-dynamic.symbols
hw-build-exe-dynamic: hw.zig
	zig build-exe --strip -O ReleaseSmall -dynamic $^ -femit-bin=$@

# add all symbols to symbol table
all-y += hw-build-exe-rdynamic hw-build-exe-rdynamic.symbols
hw-build-exe-rdynamic: hw.zig
	zig build-exe --strip -O ReleaseSmall -rdynamic $^ -femit-bin=$@

# force dynamic linking and add all symbols to symbol table
all-y += hw-build-exe-dynamic-rdynamic hw-build-exe-dynamic-rdynamic.symbols
hw-build-exe-dynamic-rdynamic: hw.zig
	zig build-exe --strip -O ReleaseSmall -dynamic -rdynamic $^ -femit-bin=$@

# link against external c library
all-y += hw-build-exe-lc hw-build-exe-lc.symbols
hw-build-exe-lc: hw.zig
	zig build-exe -lc --strip -O ReleaseSmall -femit-bin=$@ $^

# link against external c library with object files (does not work: duplicate symbol: _start)
all-n += hw-o-lc hw-o-lc.symbols
hw-o-lc: hw.o
	zig build-exe -lc -femit-bin=$@ $^

# use gcc as linker with zig object files (does not work: multiple definitions of _start)
all-n += hw-o-gcc
hw-o-gcc: hw.o
	gcc $^ -o $@

# does not work (undefined symbol: _start)
all-n += hw-lib-c-o
hw-lib-c-o: hw.o
	zig build-exe --library c -femit-bin=$@ $^


all: $(all-y)


# implicit rules

# build object files
%.o: %.zig
	zig build-obj $^ -femit-bin=$@

# build stripped object files
%-stripped.o: %.zig
	zig build-obj --strip -O ReleaseSmall $^ -femit-bin=$@

%.o: %.c
	gcc -c -o $@ $^


info: sizes objinfo


.PHONY: sizes
sizes: all
	du -b libhw* hw* | sort -n | grep -v symbols

%.symbols: %
	readelf --symbols $^ > $@


clean:
	rm -f *.o libhw* hw-* hw


vpath %.zig src
vpath %.c src
