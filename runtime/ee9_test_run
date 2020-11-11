#!/bin/sh
> KDF9.log
this_test="$2"
echo "+++++++++++++++++++ BEGIN  Test Case $1 +++++++++++++++++++"
if /bin/sh $this_test
then
   echo The signature of this test should be "$3". It was as follows:
   grep "Digital signature of traced orders" KDF9.log
   exit_code=0
else
   echo !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   echo Test Case "$1": $this_test FAILED!
   exit_code=1
fi
echo "------------------- END OF Test Case $1 -------------------"
echo
echo
exit $exit_code
