# Usage: sh raSAT.sh testDir lb ub sbox tout
# testDir: the directory containing testing .smt2 files.
# lb: lower bound of the variables
# up: upper bound of the variables.
# sbox: search box, epsilon
# tout: time out
export IFS=","
RESULT=$1/SUM_UP.xls
echo "Problem,nVars,nAPIs,time<$5 for bound: $2 -> $3,iaTime,testingTime,usTime,result" > $RESULT
ls $1/*.smt2
for file in $1/*.smt2; do
   fIaTime=0;
   fTestingTime=0;
   fUsTime=0;  
   time=0;
	./raSAT $file lb="$2 $3" $4 `echo $5 - $time | bc`
        read problem nVars nAPIs currentTime iaTime testingTime usTime result < $file.tmp
	time=`echo $time + $currentTime | bc`
	fIaTime=`echo $fIaTime + $iaTime | bc`
	fTestingTime=`echo $fTestingTime + $testingTime | bc`
	fUsTime=`echo $fUsTime + $usTime | bc`
   echo "$problem,$nVars,$nAPIs,$time,$fIaTime,$fTestingTime,$fUsTime,$result" >> $RESULT 
   rm -f $file.tmp $file.in $file.out $file.rs
done
