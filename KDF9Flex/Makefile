CC=gcc
CFLAGS=-O3

KDF9Flex: KDF9Flex.c
	$(CC) -o KDF9Flex KDF9Flex.c $(CFLAGS)

clean:
	$(RM) -f KDF9Flex

.PHONY: deploy
deploy: KDF9Flex
	cp -f KDF9Flex ../runtime/	
	cp -f absconvf.txt ../runtime/