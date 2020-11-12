CC=gcc
YACC=yacc
CFLAGS=-I. -fPIE -fPIC
YFLAGS=-d --debug
RUNTIME=../runtime

kal3: kal3.c kal3syntax.c
	$(CC) -o kal3 kal3.c kal3syntax.c $(CFLAGS)

.PHONY: clean
clean:
	$(RM) -f kal3
	$(RM) -f kal3syntax.c
	$(RM) -f y.tab.h

.PHONY: deploy
deploy: kal3
	cp -f kal3 $(RUNTIME)