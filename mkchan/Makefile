CC=gcc
CFLAGS=-I. -fPIE -fPIC
YFLAGS=-d --debug
RUNTIME=../runtime

mkchan: 
	$(CC) -o mkchan mkchan.c $(CFLAGS)

.PHONY: clean
clean:
	$(RM) mkchan

.PHONY: deploy
deploy: mkchan
	cp -f mkchan $(RUNTIME)