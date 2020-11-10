#!/bin/bash

# Set up useful paths.

ROOT=$PWD/..
S=${ROOT}/Source
B=${ROOT}/Build
T=${ROOT}/Testing
OBJECT=-aO${B}

# Set up common options and parameters.

EXCEPTIONS=-funwind-tables
OTHERS=
BUG_FIXES=
MAKE_OPTIONS=
GCC_OPTIONS="${EXCEPTIONS} ${OTHERS} ${BUG_FIXES} -march=znver2"
BASE_GNAT_OPTIONS="-gnatfl05j96  -gnatw.e -gnatwD -gnatwH -gnatwP -gnatwT -gnatw.W -gnatw.B -gnatwC -gnatw.u -gnatw.Y -gnatw.K -gnatyO"
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
	rm -f  ${B}/b\~*.* ${B}/*.ali ${B}/*.o ${B}/*.su
	echo stripping binaries
	strip ${T}/ee9 ${T}/kal3
	exit 0
	;;


tidy)
	echo Establishing a standard execution environment in ${T}
	ls -l ${T}/ee9
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
	> ${T}/GP0
	> ${T}/ee9_test_case_log.txt
	> ${T}/trace.txt
	> ${T}/KDF9_log.txt
	> ${T}/settings_1.txt
	> ${T}/settings_2.txt
	chmod 755 ${T}/dow
	chmod 755 ${T}/ee9
	chmod 755 ${T}/ee9_self_test
	chmod 755 ${T}/ee9_test_case
	chmod 755 ${T}/ee9_test_run
	chmod 755 ${T}/kal3
	chmod 755 ${T}/lap
	chmod 755 ${T}/lud
	chmod 755 ${T}/nine
	chmod 755 ${T}/set_permissions
	chmod 755 ${T}/tsd
	chmod 755 ${T}/ucc
	chmod 755 ${T}/whet
	chmod 755 ${B}/mk9
	ls -la ${T}
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

> build.log

# Build a version of ee9 as specified.
case ${1:-ee9} in

ee9)
	case ${2:-UNIX} in
	Raspberry_Pi|RPi|Raspbian|raspbian)
		echo Compiling ioc-magtape.adb separately for $2, to work around a compiler bug ...
		gcc -c -I. -I${S} -O0 -funwind-tables -gnatfl05 -fomit-frame-pointer -fno-stack-check ${S}/ioc-magtape.adb  >>build.log
		echo "... done"
		;;

	*)
		;;
	esac

	gnatmake ${MAKE_OPTIONS} -aI${S} ${OBJECT} ${GCC_OPTIONS} ${OPT_GNAT_OPTIONS} -O3 ee9 ${OPT_LINK_OPTIONS} >>build.log
	if test -r ee9
	then
	   strip ee9
	   mv ee9 ${T}/${EE9}
	   fgrep ">>>" build.log
	else
	   echo ee9 COMPILATION FAILED
	   fgrep ">>>" build.log
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
		echo kal3 COMPILED O.K.
		exit 0
	else
		echo kal3 COMPILATION FAILED
		exit 10
	fi
	;;

all)
   echo Building kal3 and ee9
	./mk9 clean ${OS}
	./mk9 kal3 ${OS}
	./mk9 ee9  ${OS}
	;;

distro)
   echo Building a distribution for ${OS}
	./mk9 all ${OS}
	./mk9 clean ${OS}
	./mk9 tidy ${OS}
	./mk9 zip ${OS}
	;;

*)
   echo Invalid build-type parameter \"$1\"
   exit 3
   ;;

esac
