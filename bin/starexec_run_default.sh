# Usage: sh raSAT.sh testfile lb ub sbox tout
# testDir: the directory containing testing .smt2 files.
# lb: lower bound of the variables
# up: upper bound of the variables.
# sbox: search box, epsilon
./raSAT $1 lb="-inf inf" $sbox $STAREXEC_CPU_LIMIT
read result < $1.tmp
echo $result

