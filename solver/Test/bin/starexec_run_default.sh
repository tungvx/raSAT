# Usage: sh raSAT.sh testfile lb ub sbox tout
# testDir: the directory containing testing .smt2 files.
# lb: lower bound of the variables
# up: upper bound of the variables.
# sbox: search box, epsilon
# tout: time out
export IFS=","
time=0;
result="unknown"
while [ $(echo "$time < $STAREXEC_CPU_LIMIT" | bc) -ne 0 -a "$result" = "unknown" ] 
  do
	./raSAT $1 lb="0 10" 0.1 `echo $STAREXEC_CPU_LIMIT - $time | bc`
  read problem nVars nAPIs currentTime iaTime testingTime usTime parsingTime decompositionTime miniSATTime miniSATVars miniSATClauses miniSATCalls raSATClauses decomposedLearnedClauses UNSATLearnedClauses unknownLearnedClauses result < $1.tmp
	time=`echo $time + $currentTime | bc`
	
  if [ "$result" = "unsat" ]; then
  	./raSAT $1 lb="-inf inf" 0.1 `echo $STAREXEC_CPU_LIMIT - $time | bc`
    read problem nVars nAPIs currentTime iaTime testingTime usTime parsingTime decompositionTime miniSATTime miniSATVars miniSATClauses miniSATCalls raSATClauses decomposedLearnedClauses UNSATLearnedClauses unknownLearnedClauses result < $1.tmp
      time=`echo $time + $currentTime | bc`
  fi
done
echo $result
