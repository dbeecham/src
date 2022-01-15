NIM = nim
PREFIX = /usr

default: hw

%: %.nim
	$(NIM) compile -o:$@ $^

install:
	cp nim $(DESTDIR)$(PREFIX)/bin/hw
