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
GNAT_WARN_OPTIONS=-gnatwa -gnatwl -gnatwD -gnatwH -gnatwP -gnatwT -gnatw.u -gnatw.W -gnatyO -gnatw.Y
GNAT_OPTIONS=${GNAT_BASE_OPTIONS} ${GNAT_WARN_OPTIONS} -gnatn

.PHONY: all
all: ee9 a2b kal3 kal4 kalgol kidopt mkchan mtp

.phony: ee9
ee9 : builddefs ${LIB_DIR} ${OPT_DEPENDS}
	gnatmake -c ee9 -i ${SRC:%=-I%} ${CFLAGS} ${GNAT_OPTIONS} >> build.log
	gnatbind ${SRC}/ee9.ali ${SRC:%=-aO%/} -shared
	gnatlink ${SRC}/ee9.ali -o ${SRC}/ee9

.PHONY: builddefs
builddefs:
	cp -f builddefs/adc-${BUILD_TYPE}.adc ${SRC}/gnat.adc

.PHONY: a2b
a2b: 
	gnatmake -j4 -c -i ${SRC}/a2b.adb ${SRC:%=-I%} ${CFLAGS} ${GNAT_OPTIONS} >> build.log
	gnatbind ${SRC}/a2b.ali ${SRC:%=-aO%/} -shared
	gnatlink ${SRC}/a2b.ali -o ${SRC}/a2b

.PHONY: kidopt
kidopt:
	gnatmake -j4 -c -i ${SRC}/kidopt.adb ${SRC:%=-I%} ${CFLAGS} ${GNAT_OPTIONS} >> build.log
	gnatbind ${SRC}/kidopt.ali ${SRC:%=-aO%/} -shared
	gnatlink ${SRC}/kidopt.ali -o ${SRC}/kidopt

.PHONY: mtp
mtp:
	gnatmake -j4 -c -i ${SRC}/mtp.adb ${SRC:%=-I%} ${CFLAGS} ${GNAT_OPTIONS} >> build.log
	gnatbind ${SRC}/mtp.ali ${SRC:%=-aO%/} -shared
	gnatlink ${SRC}/mtp.ali -o ${SRC}/mtp

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
	$(RM) build.log
	$(RM) ${SRC:%=%/*.ali}
	$(RM) ${SRC:%=%/*.o}
	$(RM) ${SRC}/ee9 ${SRC}/a2b ${SRC}/kidopt ${SRC}/mtp ${SRC}/to_9_from_1934 ${SRC}/gnat.adc

.PHONY: update
update:
	$(MAKE) -e -C ${KAL3} update
	$(MAKE) -e -C ${KAL4} update
	$(MAKE) -e -C ${KALGOL}	update
	$(MAKE) -e -C ${MKCHAN}	update

.PHONY: install
install: all
	$(RM) -r $(DESTDIR)$(prefix)/share/kdf9/
	install -d $(DESTDIR)$(prefix)/bin/
	install -d $(DESTDIR)$(prefix)/share/kdf9/
	install -d $(DESTDIR)$(prefix)/share/kdf9/Assembly
	install -m 644 runtime/Assembly/* $(DESTDIR)$(prefix)/share/kdf9/Assembly
	install -d $(DESTDIR)$(prefix)/share/kdf9/Binary
	install -m 644 runtime/Binary/* $(DESTDIR)$(prefix)/share/kdf9/Binary
	install -d $(DESTDIR)$(prefix)/share/kdf9/Data
	install -m 644 runtime/Data/* $(DESTDIR)$(prefix)/share/kdf9/Data
	install -d $(DESTDIR)$(prefix)/share/kdf9/FW0Files
	install -m 644 runtime/FW0Files/* $(DESTDIR)$(prefix)/share/kdf9/FW0Files
	install -d $(DESTDIR)$(prefix)/share/kdf9/Kidsgrove
	install -m 644 runtime/Kidsgrove/* $(DESTDIR)$(prefix)/share/kdf9/Kidsgrove
	install -d $(DESTDIR)$(prefix)/share/kdf9/settings
	install -m 644 runtime/settings/* $(DESTDIR)$(prefix)/share/kdf9/settings
	install -d $(DESTDIR)$(prefix)/share/kdf9/tests
	install -m 755 runtime/tests/* $(DESTDIR)$(prefix)/share/kdf9/tests
	install -m 644 runtime/tests/*.log $(DESTDIR)$(prefix)/share/kdf9/tests
	install -d $(DESTDIR)$(prefix)/share/kdf9/Whetstone
	install -m 644 runtime/Whetstone/* $(DESTDIR)$(prefix)/share/kdf9/Whetstone
	install -s -m 755 ${SRC}/ee9 ${SRC}/a2b ${SRC}/kidopt ${SRC}/mtp ${SRC}/to_9_from_1934 $(DESTDIR)$(prefix)/bin/
	install -m 755 scripts/* $(DESTDIR)$(prefix)/bin/
	sed "s|%prefix%|$(prefix)|g" < scripts/kdf9_setup > $(DESTDIR)$(prefix)/bin/kdf9_setup
	$(MAKE) -e -C ${KAL3} install
	$(MAKE) -e -C ${KAL4} install
	$(MAKE) -e -C ${MKCHAN}	install
	$(MAKE) -e -C ${KALGOL}	install

.PHONY: uninstall
uninstall:
	$(RM) $(DESTDIR)$(prefix)/bin/ee9
	$(RM) $(DESTDIR)$(prefix)/bin/a2b
	$(RM) $(DESTDIR)$(prefix)/bin/kidopt
	$(RM) $(DESTDIR)$(prefix)/bin/mtp
	$(RM) $(DESTDIR)$(prefix)/bin/to_9_from_1934
	for a in scripts/*; do $(RM) $(DESTDIR)$(prefix)/bin/`basename $$a`; done
	$(MAKE) -e -C ${KAL3} uninstall
	$(MAKE) -e -C ${KAL4} uninstall
	$(MAKE) -e -C ${MKCHAN}	uninstall
	$(MAKE) -e -C ${KALGOL}	uninstall
	$(RM) -r $(DESTDIR)$(prefix)/share/kdf9/
