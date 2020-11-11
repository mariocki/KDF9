#!/bin/sh
HERE=`pwd`

cp FW0_for_Director FW0

mode=-d$1
miscellany=-m$2
if [ "$mode" = "-d" ]
then
        $HERE/ee9 -sb -df  <Binary/KKT40E007UPU  >TP0;
elif [ -d$mode = -df ]
then
   time $HERE/ee9 -sb -df  <Binary/KKT40E007UPU  >TP0;
elif [ $miscellany = -m ]
then
        $HERE/ee9 -sb "$mode"               <Binary/KKT40E007UPU  >TP0;
else
        $HERE/ee9 -sb "$mode" "$miscellany" <Binary/KKT40E007UPU  >TP0;
fi

if [ -s CP0 ]
then
   echo
   echo CP0:
   echo ===
   more CP0
   echo ===
fi

if [ -s TP0 ]
then
   echo
   echo TP0:
   echo ===
   more TP0
   echo ===
fi

if [ -s LP0 ]
then
   echo
   echo LP0:
   echo ===
   more LP0
   echo ===
fi
echo
