# Usage: sh raSAT.sh testDir sbox tout
# testDir: the directory containing testing .smt2 files.
# sbox: search box, epsilon
# tout: time out
export IFS=","
RESULT=$1/SUM_UP.xls
echo "Problem,nVars,nAPIs,time<$3 ,iaTime,testingTime,usTime,parsingTime,decompositionTime,miniSATTime,miniSATVars,miniSATClauses,miniSATCalss,raSATClauses,decomposedClauses,UNSATLearnedClauses,UNKNOWNLearnedClauses,result" > $RESULT
ls $1/*.smt2
for file in $1/*.smt2; do
   fIaTime=0;
   fTestingTime=0;
   fUsTime=0;  
   fParsingTime=0;
   fDecompositionTime=0;
   fMiniSATTime=0;
   time=0;
   fMiniSATCalls=0;
   fRaSATClauses=0;
   fDecomposedLearnedClauses=0;
   fUNSATLearnedClauses=0;
   fUnknownLearnedClauses=0;
   result="unknown"
   while [ $(echo "$time < $3" | bc) -ne 0 -a "$result" = "unknown" ]
   do
	./raSAT $file $2 `echo $3 - $time | bc`
        read problem nVars nAPIs currentTime iaTime testingTime usTime parsingTime decompositionTime miniSATTime miniSATVars miniSATClauses miniSATCalls raSATClauses decomposedLearnedClauses UNSATLearnedClauses unknownLearnedClauses result < $file.tmp
	time=`echo $time + $currentTime | bc`
	fIaTime=`echo $fIaTime + $iaTime | bc`
	fTestingTime=`echo $fTestingTime + $testingTime | bc`
	fUsTime=`echo $fUsTime + $usTime | bc`
	fParsingTime=`echo $fParsingTime + $parsingTime | bc`
  fDecompositionTime=`echo $fDecompositionTime + $decompositionTime | bc`
  fMiniSATTime=`echo $fMiniSATTime + $miniSATTime | bc`
  fMiniSATCalls=`echo $fMiniSATCalls + $miniSATCalls | bc`
  fRaSATClauses=`echo $fRaSATClauses + $raSATClauses | bc`
  fDecomposedLearnedClauses=`echo $fDecomposedLearnedClauses + $decomposedLearnedClauses | bc`
  fUNSATLearnedClauses=`echo $fUNSATLearnedClauses + $UNSATLearnedClauses | bc`
  fUnknownLearnedClauses=`echo $fUnknownLearnedClauses + $unknownLearnedClauses | bc`
   done	
   echo "$problem,$nVars,$nAPIs,$time,$fIaTime,$fTestingTime,$fUsTime,$fParsingTime,$fDecompositionTime,$fMiniSATTime,$miniSATVars,$miniSATClauses,$fMiniSATCalls,$fRaSATClauses,$fDecomposedLearnedClauses,$fUNSATLearnedClauses,$fUnknownLearnedClauses,$result" >> $RESULT 
   rm -f $file.tmp
done
