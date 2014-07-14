# Usage: sh raSAT.sh testDir
# testDir: the directory containing testing .smt2 files.
# sbox: search box, epsilon
# tout: time out
ls $1/*.smt2
for file in $1/*.smt2; do
  echo $file
	./raSAT $file lb="0 10" 0.1 500
	echo "\n\n"
   rm -f $file.tmp
done
