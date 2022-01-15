default: all

all-y += hw-gcc hw-gcc.disasm hw-gcc.symbols hw-gcc.stripped
hw-gcc: hw.S
	gcc -nostdlib -o $@ $^

all-y += hw-ld-o hw-ld-o.disasm hw-ld-o.symbols hw-ld-o.stripped
hw-ld-o: hw.o
	ld -o $@ $^

hw.o: hw.S
	gcc -c -o $@ $^

all-y += hw-zig hw-zig.disasm hw-zig.symbols
hw-zig: hw.S
	zig build-exe -femit-bin=$@ $^

all-y += hw-zig-stripped hw-zig-stripped.disasm hw-zig-stripped.symbols hw-zig-stripped.stripped
hw-zig-stripped: hw.S
	zig build-exe -O ReleaseSmall --strip -femit-bin=$@ $^

all: $(all-y)

sizes: all
	du -b hw-* | sort -n | grep -v symbols | grep -v disasm | grep -v elfinfo

%.symbols: %
	readelf --symbols $^ > $@

%.disasm: %
	objdump -d $^ > $@

%.stripped: %
	strip -o $@ $^



vpath %.S src
