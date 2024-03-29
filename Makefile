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
GNAT_OPTIONS=${GNAT_BASE_OPTIONS} ${GNAT_WARN_OPTIONS} -gnatn -gnatfn -mtune=native -O3

.PHONY: all ee9 builddefs a2b kidopt mtp extract_symbols st_tl ports glance kal3 kal4 mkchan kalgol clean update install uninstall patch docker
all: ee9 a2b kal3 kal4 kalgol kidopt mkchan mtp extract_symbols st_tl ports glance

ee9 : builddefs patch ${LIB_DIR} ${OPT_DEPENDS}
	gnatmake -c ee9 -i ${SRC:%=-I%} ${CFLAGS} ${GNAT_OPTIONS} >> build.log
	gnatbind ${SRC}/ee9.ali ${SRC:%=-aO%/} -shared
	gnatlink ${SRC}/ee9.ali -o ${SRC}/ee9

builddefs:
	cp -f builddefs/adc-${BUILD_TYPE}.adc ${SRC}/gnat.adc

a2b: 
	gnatmake -j4 -c -i ${SRC}/a2b.adb ${SRC:%=-I%} ${CFLAGS} ${GNAT_OPTIONS} >> build.log
	gnatbind ${SRC}/a2b.ali ${SRC:%=-aO%/} -shared
	gnatlink ${SRC}/a2b.ali -o ${SRC}/a2b

kidopt:
	gnatmake -j4 -c -i ${SRC}/kidopt.adb ${SRC:%=-I%} ${CFLAGS} ${GNAT_OPTIONS} >> build.log
	gnatbind ${SRC}/kidopt.ali ${SRC:%=-aO%/} -shared
	gnatlink ${SRC}/kidopt.ali -o ${SRC}/kidopt

mtp:
	gnatmake -j4 -c -i ${SRC}/mtp.adb ${SRC:%=-I%} ${CFLAGS} ${GNAT_OPTIONS} >> build.log
	gnatbind ${SRC}/mtp.ali ${SRC:%=-aO%/} -shared
	gnatlink ${SRC}/mtp.ali -o ${SRC}/mtp

extract_symbols:
	gnatmake -j4 -c -i ${SRC}/extract_symbols.adb ${SRC:%=-I%} ${CFLAGS} ${GNAT_OPTIONS} >> build.log
	gnatbind ${SRC}/extract_symbols.ali ${SRC:%=-aO%/} -shared
	gnatlink ${SRC}/extract_symbols.ali -o ${SRC}/extract_symbols

st_tl:
	gnatmake -j4 -c -i ${SRC}/st_tl.adb ${SRC:%=-I%} ${CFLAGS} ${GNAT_OPTIONS} >> build.log
	gnatbind ${SRC}/st_tl.ali ${SRC:%=-aO%/} -shared
	gnatlink ${SRC}/st_tl.ali -o ${SRC}/st_tl

ports:
	gnatmake -j4 -c -i ${SRC}/ports.adb ${SRC:%=-I%} ${CFLAGS} ${GNAT_OPTIONS} >> build.log
	gnatbind ${SRC}/ports.ali ${SRC:%=-aO%/} -shared
	gnatlink ${SRC}/ports.ali -o ${SRC}/ports

glance:
	gnatmake -j4 -c -i ${SRC}/glance.adb ${SRC:%=-I%} ${CFLAGS} ${GNAT_OPTIONS} >> build.log
	gnatbind ${SRC}/glance.ali ${SRC:%=-aO%/} -shared
	gnatlink ${SRC}/glance.ali -o ${SRC}/glance

kal3: patch
	$(MAKE) -e -C ${KAL3}

kal4:
	$(MAKE) -e -C ${KAL4}

mkchan:
	$(MAKE) -e -C ${MKCHAN}

kalgol:
	$(MAKE) -e -C ${KALGOL}

clean:
	$(MAKE) -e -C ${KAL3} clean
	$(MAKE) -e -C ${KAL4} clean
	$(MAKE) -e -C ${KALGOL}	clean
	$(MAKE) -e -C ${MKCHAN}	clean
	$(RM) build.log
	$(RM) ${SRC:%=%/*.ali}
	$(RM) ${SRC:%=%/*.o}
	$(RM) ${SRC}/ee9 ${SRC}/a2b ${SRC}/kidopt ${SRC}/mtp ${SRC}/st_tl ${SRC}/extract_symbols ${SRC}/ports ${SRC}/glance ${SRC}/gnat.adc

update:
	$(MAKE) -e -C ${KAL3} update
	$(MAKE) -e -C ${KAL4} update
	$(MAKE) -e -C ${KALGOL}	update
	$(MAKE) -e -C ${MKCHAN}	update

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
	install -d $(DESTDIR)$(prefix)/share/kdf9/Whetstone
	install -m 644 runtime/Whetstone/* $(DESTDIR)$(prefix)/share/kdf9/Whetstone
	install -s -m 755 ${SRC}/ee9 ${SRC}/a2b ${SRC}/kidopt ${SRC}/mtp ${SRC}/st_tl ${SRC}/extract_symbols ${SRC}/ports ${SRC}/glance $(DESTDIR)$(prefix)/bin/
	install -m 755 scripts/* $(DESTDIR)$(prefix)/bin/
	sed "s|%prefix%|$(prefix)|g" < scripts/kdf9_setup > $(DESTDIR)$(prefix)/bin/kdf9_setup
	$(MAKE) -e -C ${KAL3} install
	$(MAKE) -e -C ${KAL4} install
	$(MAKE) -e -C ${MKCHAN}	install
	$(MAKE) -e -C ${KALGOL}	install

uninstall:
	$(RM) $(DESTDIR)$(prefix)/bin/ee9
	$(RM) $(DESTDIR)$(prefix)/bin/a2b
	$(RM) $(DESTDIR)$(prefix)/bin/kidopt
	$(RM) $(DESTDIR)$(prefix)/bin/mtp
	$(RM) $(DESTDIR)$(prefix)/bin/extract_symbols
	$(RM) $(DESTDIR)$(prefix)/bin/st_tl
	$(RM) $(DESTDIR)$(prefix)/bin/ports
	$(RM) $(DESTDIR)$(prefix)/bin/glance
	for a in scripts/*; do $(RM) $(DESTDIR)$(prefix)/bin/`basename $$a`; done
	$(MAKE) -e -C ${KAL3} uninstall
	$(MAKE) -e -C ${KAL4} uninstall
	$(MAKE) -e -C ${MKCHAN}	uninstall
	$(MAKE) -e -C ${KALGOL}	uninstall
	$(RM) -r $(DESTDIR)$(prefix)/share/kdf9/

patch:
	$(MAKE) -e -C ${KAL3} patch
	cat kdfruntime.patch | (patch -p1 -r - --no-backup-if-mismatch --forward || true)

docker: 
	docker build -t "mariocki/kdf9" .
	
#
#for a in runtime/tests/*.log; do
#	iconv -f $(file -b --mime-encoding ${a}) -t UTF8 <${a} >/tmp/tmpfile;
#	cat /tmp/tmpfile >${a};
#done
