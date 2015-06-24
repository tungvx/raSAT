# Usage: sh raSAT.sh testfile lb ub sbox tout
# testDir: the directory containing testing .smt2 files.
# lb: lower bound of the variables
# up: upper bound of the variables.
# sbox: search box, epsilon
# tout: time out
result="unknown"
sbox=1
while [ $(echo "$time < $STAREXEC_CPU_LIMIT" | bc) -ne 0 -a "$result" = "unknown" ] 
  do
  sbox=`echo "$sbox / 10" | bc)`
  echo $sbox
	./raSAT $1 lb="-10 10" $sbox `echo $STAREXEC_CPU_LIMIT - $time | bc`
  read result < $1.tmp
	
  if [ "$result" = "unsat" ]; then
  	./raSAT $1 lb="-inf inf" 0.1 `echo $STAREXEC_CPU_LIMIT - $time | bc`
    read result < $1.tmp
  fi
done
echo $result
