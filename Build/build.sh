#!/bin/bash

# Set up useful paths.

ROOT=..
S=${ROOT}/Source
B=${ROOT}/Build
T=${ROOT}/Testing

# Set up common options and parameters.

EXCEPTIONS=-funwind-tables
OTHERS=
BUG_FIXES=
MAKE_OPTIONS=
GCC_OPTIONS="${EXCEPTIONS} ${OTHERS} ${BUG_FIXES} -march=znver2"
BASE_GNAT_OPTIONS="-gnatfl05j96 -gnatw.e -gnatwD -gnatwH -gnatwP -gnatwT -gnatw.W -gnatw.B -gnatwC -gnatw.u -gnatw.Y -gnatw.K -gnatyO"
WARN_GNAT_OPTIONS="-gnatwa -gnatwl -gnatwD -gnatwH -gnatwP -gnatwT -gnatw.u -gnatw.W -gnatyO -gnatw.K -gnatw.Y"

# Set up the OS-specific parameters and files.

OS=UNIX
BASE_LINK_OPTIONS="-bargs -static"
EE9=ee9
KAL3=kal3

# Perform housekeeping tasks.

case $1 in

clean)
   echo Removing compilation workfiles
	rm -f  ${S}/*.ali ${S}/*.o ${S}/*.su
	rm -f  ${B}/*.ali ${B}/*.o ${B}/*.su
	exit 0
	;;

deploy)
	echo Establishing a standard execution environment in ${T}
	> ${T}/CP0
	> ${T}/DR0
	> ${T}/FD0
	> ${T}/LP0
	> ${T}/MT0
	> ${T}/MT1
	> ${T}/MT2
	> ${T}/MT3
	> ${T}/MT4
	> ${T}/MT5
	> ${T}/MT6
	> ${T}/MT7
	> ${T}/ST0
	> ${T}/TP0
	> ${T}/TP1
	> ${T}/TR0
	> ${T}/TR1
	> ${T}/GP0
	> ${T}/ee9_test_case.log
	> ${T}/trace.log
	> ${T}/KDF9.log
	> ${T}/settings_1.txt
	> ${T}/settings_2.txt
	chmod 755 ${T}/dow.sh
	chmod 755 ${T}/ee9
	chmod 755 ${T}/ee9_self_test.sh
	chmod 755 ${T}/ee9_test_case.sh
	chmod 755 ${T}/ee9_test_run.sh
	chmod 755 ${T}/lap.sh
	chmod 755 ${T}/lud.sh
	chmod 755 ${T}/nine.sh
	chmod 755 ${T}/tsd.sh
	chmod 755 ${T}/ucc.sh
	chmod 755 ${T}/whet.sh
	exit 0
	;;

ee9|unop|warn|max|verbose|kal3|distro|all)
	;;

"")
	echo Building mk9 ${1:-ee9} ${OS}
	;;

*)
	echo Invalid parameter \"$1\"
	exit 1
	;;

esac

# Set up optimization variants.

OPT_LINK_OPTIONS="${BASE_LINK_OPTIONS}"
TEST_GNAT_OPTIONS="-gnatoVa ${BASE_GNAT_OPTIONS} -fstack-check"
TEST_LINK_OPTIONS="${BASE_LINK_OPTIONS} -Sin"

# These options are best on test for Core i7 'Sandy Bridge' 2.3 GHz, 6MB L3 cache ...
OPT_CODE_OPTIONS="-funroll-loops -fsched-interblock -fomit-frame-pointer -fno-stack-check"
OPT_ALGN_OPTIONS="-falign-loops=8"
OPT_GNAT_OPTIONS="${BASE_GNAT_OPTIONS} -gnatn ${OPT_ALGN_OPTIONS} ${OPT_CODE_OPTIONS}"
# ... end best

# Build a version of ee9 as specified.
case ${1:-ee9} in

ee9)
	case ${2:-UNIX} in
	Raspberry_Pi|RPi|Raspbian|raspbian)
		echo Compiling ioc-magtape.adb separately for $2, to work around a compiler bug ...
		gcc -c -I. -I${S} -O0 -funwind-tables -gnatfl05 -fomit-frame-pointer -fno-stack-check ${S}/ioc-magtape.adb
		;;

	*)
		;;
	esac

	gnatmake ${MAKE_OPTIONS} -D${S} -aO${S} -aI${S} ${OBJECT} ${GCC_OPTIONS} ${OPT_GNAT_OPTIONS} -O3 ee9 ${OPT_LINK_OPTIONS} 1>/dev/null
	if test -r ee9
	then
	   mv ee9 ${T}/${EE9}
	else
	   echo ee9 COMPILATION FAILED
	   exit 10
	fi
   ;;

kal3)
	echo Compiling kal3 for  ${OS}
	rm -f ${T}/${KAL3}
	gcc -o kal3 ${B}/kal3_source/kal3.c ${B}/kal3_source/y.tab.c
	if test -r kal3
	then
		mv kal3 ${T}/${KAL3}
		exit 0
	else
		echo kal3 COMPILATION FAILED
		exit 10
	fi
	;;

*)
   echo Invalid build-type parameter \"$1\"
   exit 3
   ;;

esac
