#!/bin/bash
case $1 in
WAB) this_test="./nine.sh TR2GP wabbit_kdf9"
     signature=\#2227242756376336;;
HIG) this_test="./nine.sh HiGuys -"
     signature=\#6442100001003204;;
ACK) this_test="./nine.sh Ack37 -"
     signature=\#6014542523645635;;
HLT) this_test="./nine.sh Leech Leech_data0"
     signature=\#5751545277412347;;
MOB) this_test="./whet.sh ManOrBoy"
     signature=\#5543551060332736 ;;
GPS) this_test="./whet.sh GPS"
     signature=\#1120516516377620;;
QRT) this_test="./whet.sh QR"
     signature=\#3153160472261612;;
WBM) this_test="./whet.sh Whetstone"
     signature=\#6324561700445753;;
RLT) this_test="./nine_test.sh RLT RLT_data.txt"
     signature=\#1341633223251244;;
TRB) this_test="./nine_test.sh TRB -"
     signature=\#3666136115202052;;
TO4) this_test="./nine.sh OUT4 -"
     signature=\#0170377556024614;;
TOX) this_test="./nine.sh OUTX -"
     signature=\#3600710030216145;;
MTW) this_test="./whet.sh MTW"
     signature=\#6265413557755246;;
INP) this_test="./whet.sh input"
     signature=\#4656332070015232;;
*)   echo test case "$1"\;  UNKNOWN!
     exit 2;;
esac

if /bin/sh ./ee9_test_run.sh "$1" "$this_test t ahipr$2" "$signature"
then
  exit 0
else
  exit 1
fi
