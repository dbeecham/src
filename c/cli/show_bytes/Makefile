default: test

show_bytes.o: show_bytes.c
	gcc show_bytes.c -std=c99 -c

test.o: test.c
	gcc test.c -std=c99 -c

test: test.o show_bytes.o
	gcc test.o show_bytes.o -o test
