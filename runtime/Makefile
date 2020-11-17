#------------------------------------------------------------------------
#    This file is part of ee9.
#
#    ee9 is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    ee9 is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with ee9.  If not, see <https://www.gnu.org/licenses/>.
#------------------------------------------------------------------------

.PHONY: deploy
deploy: check-env
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
	> settings_1.txt
	> settings_2.txt
	for a in tests/*; do if (/usr/bin/file -b $$a | grep --quiet "Bourne-Again") ; then cat $$a | tr -d '\r' > $$a.tmp; mv -f $$a.tmp $$a; chmod a+x $$a; fi; done
	for a in *; do if (/usr/bin/file -b $$a | grep --quiet "Bourne-Again") ; then cat $$a | tr -d '\r' > $$a.tmp; mv -f $$a.tmp $$a; chmod a+x $$a; fi; done
	echo -n "Compiling: "; for f in Assembly/*.k3; do p=`basename $$f .k3`; echo -n $$p " "; ./ucc $$p; done; echo

.PHONY: clean
clean:
	$(RM) mt.out
	$(RM) a.out
	$(RM) logs/*
	$(RM) *.tmp tests/*.tmp

.PHONY: distclean
distclean: clean
	$(RM) Assembly/*-listing.txt
	$(RM) CP[01]
	$(RM) CR[01]
	$(RM) DR0
	$(RM) FD0
	$(RM) FW0
	$(RM) GP0
	$(RM) LP[01]
	$(RM) MT[0123456789]
	$(RM) SI[01]
	$(RM) ST[01]
	$(RM) TP[01]
	$(RM) TR[01]
	$(RM) Assembly/WEEHAA*
	$(RM) settings_1.txt settings_2.txt settings_for_kalgol.txt
	${RM} Binary/MKSYS2 Binary/KAB00DH--USU
	$(RM) Data/systape.txt Data/systape_kalgol.txt Data/ABS_text.txt Data/kalgol_labels.txt
	for f in Assembly/*.k3; do p=`basename $$f .k3`; $(RM) Binary/$$p; done

.PHONY: check
check:
	tests/ee9_reg_test

.PHONY: check-env
check-env:
ifndef KDF9ROOT
	$(error "KDF9ROOT is undefined")
endif