BUILD_TYPE=max
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
KDF9FLEX=$(CURDIR)/KDF9Flex
KAL3=$(CURDIR)/kal3

# Main target to build
MAIN=${SRC}/${EXE}

export CC=gcc
export CFLAGS=-funwind-tables -march=znver2 -O3 -funroll-loops -fno-stack-check
GNAT_BASE_OPTIONS=-gnatfl05j96 -gnatw.e -gnatwD -gnatwH -gnatwP -gnatwT -gnatw.W -gnatw.B -gnatwC -gnatw.u -gnatw.Y -gnatw.K -gnatyO
GNAT_WARN_OPTIONS=-gnatwa -gnatwl -gnatwD -gnatwH -gnatwP -gnatwT -gnatw.u -gnatw.W -gnatyO -gnatw.K -gnatw.Y
GNAT_OPTIONS=${GNAT_BASE_OPTIONS} ${GNAT_WARN_OPTIONS} -gnatn 

CSC_LIST=$(SRC)
LIB_DIR=${foreach dir,${CSC_LIST},${dir}}

ifeq ($(OS),Windows_NT)
	OS_SPECIFIC = os_specifics_for_windows.adb
	OPT_DEPENDS = get_O_BINARY.o
else
	OS_SPECIFIC = os_specifics_for_posix.adb
	OPT_DEPENDS = 
endif

$(MAIN) : objects ${LIB_DIR} ${OPT_DEPENDS}
	gnatbind ${MAIN} ${CSC_LIST:%=-aO%/} -shared
	gnatlink ${MAIN} ${OPT_DEPENDS} -o ${MAIN}

get_O_BINARY.o:
	$(CC) ${CFLAGS} -c ${CSC_LIST}/get_O_BINARY.c -o ${CSC_LIST}/get_O_BINARY.o

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
	$(MAKE) -e -C ${KDF9FLEX} clean
	$(MAKE) -e -C ${KAL3} clean
	$(MAKE) -e -C ${RUNTIME} clean
	$(RM) -f ${CSC_LIST:%=%/*.ali}
	$(RM) -f ${CSC_LIST:%=%/*.o}
	$(RM) -f ${MAIN}
	$(RM) -f ${CSC_LIST}/os_specifics.adb
	$(RM) -f ${CSC_LIST}/gnat.adc

.PHONY: deploy
deploy: $(MAIN) kal3 flex
	$(MAKE) -e -C ${KDF9FLEX} deploy
	$(MAKE) -e -C ${KAL3} deploy
	$(MAKE) -e -C ${RUNTIME} deploy
	cp -f ${MAIN} ${RUNTIME}

.PHONY: test
test: deploy
	$(MAKE) -C ${RUNTIME} test

.PHONY: all
all: $(MAIN) kal3 flex