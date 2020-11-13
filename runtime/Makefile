.PHONY: deploy
deploy: 
	> CP0
	> CP1
	> CR0
	> CR1
	> DR0
	> FD0
	> FW0
	> GP0
	> LP0
	> LP1
	> MT0
	> MT1
	> MT2
	> MT3
	> MT4
	> MT5
	> MT6
	> MT7
	> SI0
	> SI1
	> ST0
	> ST1
	> TP0
	> TP1
	> TR0
	> TR1
	> ee9_test_case.log
	> trace.log
	> KDF9.log
	> settings_1.txt
	> settings_2.txt

.PHONY: clean
clean:
	$(RM) -f ee9
	$(RM) -f kal3 mkchan a2b mtp
	$(RM) -f mt.out
	$(RM) -f a.out
	$(RM) -f Assembly/*-listing.txt
	$(RM) -f CP[01]
	$(RM) -f CR[01]
	$(RM) -f DR0
	$(RM) -f FD0
	$(RM) -f FW0
	$(RM) -f GP0
	$(RM) -f LP[01]
	$(RM) -f MT[01234567]
	$(RM) -f SI[01]
	$(RM) -f ST[01]
	$(RM) -f TP[01]
	$(RM) -f TR[01]
	$(RM) -f ee9_test_case.log
	$(RM) -f trace.log
	$(RM) -f KDF9.log
	$(RM) -f settings_1.txt
	$(RM) -f settings_2.txt
	${RM} -f Binary/MKSYS2 Binary/KAB00DH--USU

.PHONY: test
test:
	./ee9_self_test