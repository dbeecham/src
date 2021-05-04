all: hw

hw: hw.c
	gcc hw.c -o hw -O2 -m64 -march=core2 `pkg-config --cflags --libs gtk+-2.0`
