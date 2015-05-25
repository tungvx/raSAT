# Usage: sh raSAT.sh testfile lb ub sbox tout
# testDir: the directory containing testing .smt2 files.
# lb: lower bound of the variables
# up: upper bound of the variables.
# sbox: search box, epsilon
# tout: time out
time=0;
result="unknown"
sbox=1
while [ $(echo "$time < $STAREXEC_CPU_LIMIT" | bc) -ne 0 -a "$result" = "unknown" ] 
  
  do
	./raSAT $1 lb="-10 10" $sbox `echo $STAREXEC_CPU_LIMIT - $time | bc`
  read result < $1.tmp
	time=`echo $time + $currentTime | bc`
	
  if [ "$result" = "unsat" ]; then
  	./raSAT $1 lb="-inf inf" 0.1 `echo $STAREXEC_CPU_LIMIT - $time | bc`
    read result < $1.tmp
      time=`echo $time + $currentTime | bc`
  fi
done
echo $result
