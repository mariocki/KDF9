CC=gcc
YACC=yacc
CFLAGS=-I. -fPIE -fPIC
YFLAGS=-d --debug

kal3: kal3.c kal3syntax.c
	$(CC) -o kal3 kal3.c kal3syntax.c $(CFLAGS)

.PHONY: clean
clean:
	rm -f kal3
	rm -f kal3syntax.c
	rm -f y.tab.h