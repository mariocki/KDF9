BUILD_TYPE=ee9
# ee9 : optimised and with runtime checking
# unop : unoptimised and with no optional warnings
# warn : unoptimised and with many optional warnings
# max : optimised and with no optional warnings or checking
# verbose : optimised and with extra warnings

MAIN=src/ee9
RUNTIME=runtime

CC=gcc
CFLAGS=-funwind-tables -march=znver2 -O3 -j15 -funroll-loops -fsched-interblock -fomit-frame-pointer -fno-stack-check -falign-loops=8
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
	gnatmake -c -i ${MAIN}.adb ${CSC_LIST:%=-I%} ${CFLAGS} ${GNAT_OPTIONS} >/dev/null

.PHONY: builddefs
builddefs:
	cp -f builddefs/${OS_SPECIFIC} ${CSC_LIST}/os_specifics.adb
	cp -f builddefs/adc-${BUILD_TYPE}.adc ${CSC_LIST}/gnat.adc

.PHONY: kal3
kal3:
	$(MAKE) -C kal3

.PHONY: flex
flex: 
	$(MAKE) -C KDF9Flex

.PHONY: clean
clean:
	$(MAKE) -C KDF9Flex clean
	$(MAKE) -C Kal3 clean
	$(RM) -f ${RUNTIME}/ee9
	$(RM) -f ${CSC_LIST:%=%/*.ali}
	$(RM) -f ${CSC_LIST:%=%/*.o}
	$(RM) -f ${MAIN}
	$(RM) -f ${CSC_LIST}/os_specifics.adb
	$(RM) -f ${CSC_LIST}/gnat.adc

.PHONY: deploy
deploy:
	cp -f ${MAIN} ${RUNTIME}
	> ${RUNTIME}/CP0
	> ${RUNTIME}/DR0
	> ${RUNTIME}/FD0
	> ${RUNTIME}/LP0
	> ${RUNTIME}/MT0
	> ${RUNTIME}/MT1
	> ${RUNTIME}/MT2
	> ${RUNTIME}/MT3
	> ${RUNTIME}/MT4
	> ${RUNTIME}/MT5
	> ${RUNTIME}/MT6
	> ${RUNTIME}/MT7
	> ${RUNTIME}/ST0
	> ${RUNTIME}/TP0
	> ${RUNTIME}/TP1
	> ${RUNTIME}/GP0
	> ${RUNTIME}/ee9_test_case_log.txt
	> ${RUNTIME}/trace.txt
	> ${RUNTIME}/KDF9_log.txt
	> ${RUNTIME}/settings_1.txt
	> ${RUNTIME}/settings_2.txt