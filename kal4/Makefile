CC=gcc
YACC=yacc
CFLAGS=-I. -fPIE -fPIC
YFLAGS=-d --debug
RUNTIME=../runtime

kal3: kal4.c kal4syntax.c kal4tab.c
	$(CC) -o kal4 kal4.c kal4tab.c kal4syntax.c $(CFLAGS)

.PHONY: clean
clean:
	$(RM) -f kal4
	$(RM) -f kal4syntax.c
	$(RM) -f y.tab.h y.output

.PHONY: deploy
deploy: kal4
	cp -f kal4 $(RUNTIME)