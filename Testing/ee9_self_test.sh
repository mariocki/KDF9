#!/bin/bash
cp settings_1.txt settings_1.bak
cp settings_2.txt settings_2.bak

trap "cp settings_1.bak settings_1.txt; \
      cp settings_2.bak settings_2.txt; \
      rm -f settings_?.bak;             \
      exit 999"                         \
      SIGTERM SIGQUIT SIGKILL SIGINT SIGHUP

echo
echo This runs a variety of tests, designed to verify that ee9 is working.
echo The distributed ee9 takes less than 15 seconds on a 2011 MacBook Pro.
echo
echo N.B. All I/O device files \(e.g. TR1 or MT0\) may be overwritten by this test.
echo
echo "     Quit at the prompt if you do not want that to be done."
echo
echo The ee9 output scrolls past rapidly, but at the end you will
echo get to see, page-by-page, a listing of the relevant results.
echo
echo Before each new phase, it will pause \(like this\) and wait for you to ...
printf "\\a\\a\\a"
echo
echo -n "Press RETURN to start the tests, or q to quit: "

read reply
case ${reply:zzz} in
q|Q) exit 0
     ;;
zzz) ;;
esac

> settings_1.txt
> settings_2.txt
> ee9_test_case.log

> CP0
> DR0
> FD0
> GP0
> LP0
> MT0
> MT1
> MT2
> MT3
> MT4
> MT5
> MT6
> MT7
> ST0
> TP0
> TP1

/bin/sh ./ee9_test_case.sh HIG $1 >> ee9_test_case.log; echo
/bin/sh ./ee9_test_case.sh ACK $1 >> ee9_test_case.log; echo
/bin/sh ./ee9_test_case.sh HLT $1 >> ee9_test_case.log; echo
/bin/sh ./ee9_test_case.sh MOB $1 >> ee9_test_case.log; echo
/bin/sh ./ee9_test_case.sh GPS $1 >> ee9_test_case.log; echo
/bin/sh ./ee9_test_case.sh QRT $1 >> ee9_test_case.log; echo
/bin/sh ./ee9_test_case.sh WBM $1 >> ee9_test_case.log; echo
/bin/sh ./ee9_test_case.sh RLT $1 >> ee9_test_case.log; echo
/bin/sh ./ee9_test_case.sh TRB $1 >> ee9_test_case.log; echo
/bin/sh ./ee9_test_case.sh TO4 $1 >> ee9_test_case.log; echo
/bin/sh ./ee9_test_case.sh TOX $1 >> ee9_test_case.log; echo
/bin/sh ./ee9_test_case.sh MTW $1 >> ee9_test_case.log; echo
/bin/sh ./ee9_test_case.sh INP $1 >> ee9_test_case.log; echo
/bin/sh ./ee9_test_case.sh WAB gn >> ee9_test_case.log; echo

cp settings_1.bak settings_1.txt
cp settings_2.bak settings_2.txt
rm -f settings_[12].bak

printf "\\a\\a\\a"
echo Press RETURN when you are ready to see the results:
read reply

less -E ee9_test_case.log

diff ee9_test_case.log ee9_good_test_case.log  >differences.log

printf "\\a\\a\\a"
echo Press RETURN to complete the procedure:
read reply

if [ ! \( -s differences.log \) ]
then
   echo
   echo "Done: ee9 is working as expected."
   echo
   echo "To see the plotted graph, open GP0 with a program that supports"
   echo "Encapsulated PostScript (EPS); e.g. Preview in OS X."
   echo
else
   echo
   echo The test has shown incorrect results, held in \"ee9_test_case.log\".
   echo The following lists the differences from the qualification test log.
   printf "\\a\\a\\a"
   echo Press RETURN to see the discrepancies:
   read reply
   echo
   less -E differences.log
fi

rm -f differences.log

exit 0
