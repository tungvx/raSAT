# Usage: sh raSAT.sh testfile lb ub sbox tout
# testDir: the directory containing testing .smt2 files.
# lb: lower bound of the variables
# up: upper bound of the variables.
# sbox: search box, epsilon
./raSAT $1 lb="-10 10"
read result < $1.tmp

if [ "$result" = "unsat" ]; then
	./raSAT $1 lb="-inf inf"
  read result < $1.tmp
fi

echo $result
