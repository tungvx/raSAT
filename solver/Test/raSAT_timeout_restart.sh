# Usage: sh raSAT.sh testDir lb ub sbox tout
# testDir: the directory containing testing .smt2 files.
# lb: lower bound of the variables
# up: upper bound of the variables.
# sbox: search box, epsilon
# tout: time out
export IFS=","
RESULT=$1/SUM_UP.xls
echo "Problem,nVars,nAPIs,time<$5,result" > $RESULT
ls $1/*.smt2
for file in $1/*.smt2; do
   time=0;
   result="UNKNOWN"
   while [ $(echo "$time < $5" | bc) -ne 0 -a "$result" = "Timeout" ]
   do
	./raSAT $file lb="$2 $3" $4 $5
        read problem nVars nAPIs currentTime result < ${file%.smt2}
	time=`echo $time + $currentTime | bc`
   done	
   echo "$problem,$nVars,$nAPIs,$time,$result" >> $RESULT 
done
