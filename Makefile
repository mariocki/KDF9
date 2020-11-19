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

BUILD_TYPE=ee9
# ee9 : optimised and with runtime checking
# unop : unoptimised and with no optional warnings
# warn : unoptimised and with many optional warnings
# max : optimised and with no optional warnings or checking
# verbose : optimised and with extra warnings

# Folder locations
SRC=$(CURDIR)/src
export RUNTIME=$(CURDIR)/runtime
KAL3=$(CURDIR)/kal3
KAL4=$(CURDIR)/kal4
MKCHAN=$(CURDIR)/mkchan
KALGOL=$(CURDIR)/kalgol

export prefix?=/usr/local

export CC=gcc
export CFLAGS=-funwind-tables -march=native -O3 -funroll-loops -fno-stack-check
GNAT_BASE_OPTIONS=-gnatl12j96 -gnatw.e -gnatwD -gnatwH -gnatwP -gnatwT -gnatw.W -gnatw.B -gnatwC -gnatw.u -gnatw.Y -gnatw.K -gnatyO
GNAT_WARN_OPTIONS=-gnatwa -gnatwl -gnatwD -gnatwH -gnatwP -gnatwT -gnatw.u -gnatw.W -gnatyO -gnatw.K -gnatw.Y
GNAT_OPTIONS=${GNAT_BASE_OPTIONS} ${GNAT_WARN_OPTIONS} -gnatn 

.PHONY: all
all: ee9 a2b kal3 kal4 kalgol kidopt mkchan mtp

.phony: ee9
ee9 : builddefs ${LIB_DIR} ${OPT_DEPENDS}
	gnatmake -c ee9 -i ${SRC:%=-I%} ${CFLAGS} ${GNAT_OPTIONS} >/dev/null
	gnatbind ${SRC}/ee9.ali ${SRC:%=-aO%/} -shared
	gnatlink ${SRC}/ee9.ali -o ${SRC}/ee9

.PHONY: builddefs
builddefs:
	cp -f builddefs/adc-${BUILD_TYPE}.adc ${SRC}/gnat.adc

.PHONY: a2b
a2b: 
	gnatmake -j4 -c -i ${SRC}/a2b.adb ${SRC:%=-I%} ${CFLAGS} ${GNAT_OPTIONS} >/dev/null
	gnatbind ${SRC}/a2b ${SRC:%=-aO%/} -shared
	gnatlink ${SRC}/a2b -o ${SRC}/a2b

.PHONY: kidopt
kidopt:
	gnatmake -j4 -c -i ${SRC}/kidopt.adb ${SRC:%=-I%} ${CFLAGS} ${GNAT_OPTIONS} >/dev/null
	gnatbind ${SRC}/kidopt ${SRC:%=-aO%/} -shared
	gnatlink ${SRC}/kidopt -o ${SRC}/kidopt	

.PHONY: mtp
mtp:
	gnatmake -j4 -c -i ${SRC}/mtp.adb ${SRC:%=-I%} ${CFLAGS} ${GNAT_OPTIONS} >/dev/null
	gnatbind ${SRC}/mtp ${SRC:%=-aO%/} -shared
	gnatlink ${SRC}/mtp -o ${SRC}/mtp

.PHONY: kal3
kal3:
	$(MAKE) -e -C ${KAL3}

.PHONY: kal4
kal4:
	$(MAKE) -e -C ${KAL4}	

.PHONY: mkchan
mkchan:
	$(MAKE) -e -C ${MKCHAN}

.PHONY: kalgol
kalgol:
	$(MAKE) -e -C ${KALGOL}

.PHONY: clean
clean:
	$(MAKE) -e -C ${KAL3} clean
	$(MAKE) -e -C ${KAL4} clean
	$(MAKE) -e -C ${KALGOL}	clean
	$(MAKE) -e -C ${MKCHAN}	clean
	$(RM) ${SRC:%=%/*.ali}
	$(RM) ${SRC:%=%/*.o}
	$(RM) ${SRC}/ee9 ${SRC}/a2b ${SRC}/kidopt ${SRC}/mtp ${SRC}/gnat.adc

.PHONY: install
install: all
	$(RM) -r $(DESTDIR)$(prefix)/share/kdf9/
	install -d $(DESTDIR)$(prefix)/bin/
	install -d $(DESTDIR)$(prefix)/share/kdf9/
	cp -aR runtime $(DESTDIR)$(prefix)/share/kdf9/
	install -s -m 755 ${SRC}/ee9 ${SRC}/a2b ${SRC}/kidopt ${SRC}/mtp $(DESTDIR)$(prefix)/bin/
	install -m 755 scripts/* $(DESTDIR)$(prefix)/bin/
	sed "s|%prefix%|$(prefix)|g" < scripts/kdf9_setup > $(DESTDIR)$(prefix)/bin/kdf9_setup
	$(MAKE) -e -C ${KAL3} install
	$(MAKE) -e -C ${KAL4} install
	$(MAKE) -e -C ${MKCHAN}	install
	$(MAKE) -e -C ${KALGOL}	install
