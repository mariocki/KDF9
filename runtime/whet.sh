#!/bin/sh

program="$1"
if /bin/sh ./lap.sh "$program"
then
   cp FW0_for_Whetstone FW0
   ./dow.sh $2 $3
   echo

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
else
   echo
   exit 1
fi
