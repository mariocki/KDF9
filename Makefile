BUILD_TYPE=max
# ee9 : optimised and with runtime checking
# unop : unoptimised and with no optional warnings
# warn : unoptimised and with many optional warnings
# max : optimised and with no optional warnings or checking
# verbose : optimised and with extra warnings

# Executable name
EXE=ee9

# Folder locations
SRC=src
RUNTIME=runtime
KDF9FLEX=KDF9Flex
KAL3=kal3

# Main target to build
MAIN=${SRC}/${EXE}

export CC=gcc
export CFLAGS=-funwind-tables -march=znver2 -O3 -funroll-loops -fsched-interblock -fomit-frame-pointer -fno-stack-check -falign-loops=8
GNAT_BASE_OPTIONS=-gnatfl05j96 -gnatw.e -gnatwD -gnatwH -gnatwP -gnatwT -gnatw.W -gnatw.B -gnatwC -gnatw.u -gnatw.Y -gnatw.K -gnatyO
GNAT_WARN_OPTIONS=-gnatwa -gnatwl -gnatwD -gnatwH -gnatwP -gnatwT -gnatw.u -gnatw.W -gnatyO -gnatw.K -gnatw.Y
GNAT_OPTIONS=${GNAT_BASE_OPTIONS} ${GNAT_WARN_OPTIONS} -gnatn 

CSC_LIST=src
LIB_DIR=${foreach dir,${CSC_LIST},${dir}}

ifeq ($(OS),Windows_NT)
	OS_SPECIFIC = os_specifics_for_windows.adb
else
	OS_SPECIFIC = os_specifics_for_posix.adb
endif

$(MAIN) : objects ${LIB_DIR} 
	gnatbind ${MAIN} ${CSC_LIST:%=-aO%/} -shared
	gnatlink ${MAIN} -o ${MAIN}

.PHONY: objects
objects: builddefs
	gnatmake -j4 -c -i ${MAIN}.adb ${CSC_LIST:%=-I%} ${CFLAGS} ${GNAT_OPTIONS} >/dev/null

.PHONY: builddefs
builddefs:
	cp -f builddefs/${OS_SPECIFIC} ${CSC_LIST}/os_specifics.adb
	cp -f builddefs/adc-${BUILD_TYPE}.adc ${CSC_LIST}/gnat.adc

.PHONY: kal3
kal3:
	$(MAKE) -e -C ${KAL3}

.PHONY: flex
flex: 
	$(MAKE) -e -C ${KDF9FLEX}

.PHONY: clean
clean:
	$(MAKE) -C ${KDF9FLEX} clean
	$(MAKE) -C ${KAL3} clean
	$(MAKE) -C ${RUNTIME} clean
	$(RM) -f ${CSC_LIST:%=%/*.ali}
	$(RM) -f ${CSC_LIST:%=%/*.o}
	$(RM) -f ${MAIN}
	$(RM) -f ${CSC_LIST}/os_specifics.adb
	$(RM) -f ${CSC_LIST}/gnat.adc

.PHONY: deploy
deploy: $(MAIN) kal3 flex
	$(MAKE) -C ${KDF9FLEX} deploy
	$(MAKE) -C ${KAL3} deploy
	$(MAKE) -C ${RUNTIME} deploy
	cp -f ${MAIN} ${RUNTIME}

.PHONY: test
test: deploy
	$(MAKE) -C ${RUNTIME} test

.PHONY: all
all: $(MAIN) kal3 flex