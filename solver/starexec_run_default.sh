# Usage: sh raSAT.sh testfile lb ub sbox tout
# testDir: the directory containing testing .smt2 files.
# lb: lower bound of the variables
# up: upper bound of the variables.
# sbox: search box, epsilon
# tout: time out
result="unknown"
sbox=1
round=0
while [ "$result" = "unknown" ] 
  do
  round=$((round + 1))
  sbox=`echo "scale=$round; $sbox / 10" | bc`
	./raSAT $1 lb="-10 10" $sbox $STAREXEC_CPU_LIMIT
  read result < $1.tmp
	
  if [ "$result" = "unsat" ]; then
  	./raSAT $1 lb="-inf inf" $sbox $STAREXEC_CPU_LIMIT
    read result < $1.tmp
  fi
done
echo $result
