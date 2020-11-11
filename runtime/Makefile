.PHONY: deploy
deploy: 
	> CP0
	> DR0
	> FD0
	> LP0
	> MT0
	> MT1
	> MT2
	> MT3
	> MT4
	> MT5
	> MT6
	> MT7
	> ST0
	> TP0
	> TP1
	> GP0
	> ee9_test_case.log
	> trace.log
	> KDF9.log
	> settings_1.txt
	> settings_2.txt

.PHONY: clean
clean:
	$(RM) -f ee9
	$(RM) -f kal3 absconvf.txt
	$(RM) -f KDF9Flex
	$(RM) -f mt.out
	$(RM) -f a.out
	$(RM) -f Assembly/*-listing.txt
	$(RM) -f CP0
	$(RM) -f DR0
	$(RM) -f FD0
	$(RM) -f GP0
	$(RM) -f LP0
	$(RM) -f MT[01234567]
	$(RM) -f ST0
	$(RM) -f TP[01]
	$(RM) -f TR[01]
	$(RM) -f ee9_test_case.log
	$(RM) -f trace.log
	$(RM) -f KDF9.log
	$(RM) -f settings_1.txt
	$(RM) -f settings_2.txt

.PHONY: test
test:
	./ee9_self_test.sh