CC=gcc
CFLAGS=-O3
RUNTIME=../runtime

KDF9Flex: KDF9Flex.c
	$(CC) -o KDF9Flex KDF9Flex.c $(CFLAGS)

clean:
	$(RM) -f KDF9Flex

.PHONY: deploy
deploy: KDF9Flex
	cp -f KDF9Flex $(RUNTIME)
	cp -f absconvf.txt $(RUNTIME)