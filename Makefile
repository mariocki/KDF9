BUILD_TYPE=ee9
# ee9 : optimised and with runtime checking
# unop : unoptimised and with no optional warnings
# warn : unoptimised and with many optional warnings
# max : optimised and with no optional warnings or checking
# verbose : optimised and with extra warnings

# Executable name
EXE=ee9

# Folder locations
SRC=$(CURDIR)/src
export RUNTIME=$(CURDIR)/runtime
KAL3=$(CURDIR)/kal3
KAL4=$(CURDIR)/kal4
MKCHAN=$(CURDIR)/mkchan
KALGOL=$(CURDIR)/kalgol

# Main target to build
MAIN=${SRC}/${EXE}

export CC=gcc
export CFLAGS=-funwind-tables -march=native -O3 -funroll-loops -fno-stack-check
GNAT_BASE_OPTIONS=-gnatl12j96 -gnatw.e -gnatwD -gnatwH -gnatwP -gnatwT -gnatw.W -gnatw.B -gnatwC -gnatw.u -gnatw.Y -gnatw.K -gnatyO
GNAT_WARN_OPTIONS=-gnatwa -gnatwl -gnatwD -gnatwH -gnatwP -gnatwT -gnatw.u -gnatw.W -gnatyO -gnatw.K -gnatw.Y
GNAT_OPTIONS=${GNAT_BASE_OPTIONS} ${GNAT_WARN_OPTIONS} -gnatn 

CSC_LIST=$(SRC)
LIB_DIR=${foreach dir,${CSC_LIST},${dir}}

$(MAIN) : objects ${LIB_DIR} ${OPT_DEPENDS}
	gnatbind ${MAIN} ${CSC_LIST:%=-aO%/} -shared
	gnatlink ${MAIN} -o ${MAIN}

.PHONY: objects
objects: builddefs
	gnatmake -j4 -c -i ${MAIN}.adb ${CSC_LIST:%=-I%} ${CFLAGS} ${GNAT_OPTIONS} >/dev/null

.PHONY: builddefs
builddefs:
	cp -f builddefs/adc-${BUILD_TYPE}.adc ${CSC_LIST}/gnat.adc

.PHONY: kalgol
kalgol: 
	gnatmake -j4 -c -i ${SRC}/a2b.adb ${CSC_LIST:%=-I%} ${CFLAGS} ${GNAT_OPTIONS} >/dev/null
	gnatbind ${SRC}/a2b ${CSC_LIST:%=-aO%/} -shared
	gnatlink ${SRC}/a2b -o ${SRC}/a2b

.PHONY: kidopt
kidopt:
	gnatmake -j4 -c -i ${SRC}/kidopt.adb ${CSC_LIST:%=-I%} ${CFLAGS} ${GNAT_OPTIONS} >/dev/null
	gnatbind ${SRC}/kidopt ${CSC_LIST:%=-aO%/} -shared
	gnatlink ${SRC}/kidopt -o ${SRC}/kidopt	

.PHONY: mtp
mtp:
	gnatmake -j4 -c -i ${SRC}/mtp.adb ${CSC_LIST:%=-I%} ${CFLAGS} ${GNAT_OPTIONS} >/dev/null
	gnatbind ${SRC}/mtp ${CSC_LIST:%=-aO%/} -shared
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

.PHONY: clean
clean:
	$(MAKE) -e -C ${KAL3} clean
	$(MAKE) -e -C ${KAL4} clean
	$(MAKE) -e -C ${MKCHAN}	clean
	$(MAKE) -e -C ${RUNTIME} clean
	$(RM) ${CSC_LIST:%=%/*.ali}
	$(RM) ${CSC_LIST:%=%/*.o}
	$(RM) ${MAIN} ${SRC}/a2b ${SRC}/mtp ${SRC}/kidopt ${SRC}/gnat.adc
	$(RM) ${RUNTIME}/Data/systape_kalgol.txt ${RUNTIME}/Data/systape.txt ${RUNTIME}/Data/crtest_data.txt ${RUNTIME}/Data/mt_test_labels.txt

.PHONY: deploy 
deploy: $(MAIN) kidopt kalgol mtp
	$(MAKE) -e -C ${KAL3} deploy
	$(MAKE) -e -C ${KAL4} clean
	$(MAKE) -e -C ${MKCHAN}	deploy
	cp -f ${MAIN} ${RUNTIME}
	cp -f ${SRC}/kidopt ${RUNTIME}
	cp -f ${SRC}/mtp ${RUNTIME}
	cp -f ${SRC}/a2b ${RUNTIME}
	${RUNTIME}/a2b -r2p < ${KALGOL}/mksys2.bin >${RUNTIME}/Binary/MKSYS2
	${RUNTIME}/a2b -r2p < ${KALGOL}/KAB00.bin >${RUNTIME}/Binary/KAB00DH--USU
	$(MAKE) -e -C ${RUNTIME} deploy
	cp -f ${KALGOL}/systape_kalgol.txt ${RUNTIME}/Data/

.PHONY: test
test: deploy
	$(MAKE) -C ${RUNTIME} test

.PHONY: all
all: $(MAIN) kal3 mkchan kalgol
