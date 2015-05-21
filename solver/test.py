import fnmatch
import os
import subprocess
import csv

matches = []
def run(directory, initLowerBound, initUpperBound, initSbox, timeout, resultFile):
  lowerBound = initLowerBound
  upperBound = initUpperBound
  #sbox = initSbox
  solvedProblems = 0
  with open(os.path.join(directory, resultFile), 'wb') as csvfile:
    spamwriter = csv.writer(csvfile)
    spamwriter.writerow(['Problem', 'nVars', 'maxVars', 'nAPIs', 'time', 'iaTime', 'testingTime', 'usCoreTime', 'parsingTime', 'decompositionTime', 'miniSATTime', 'miniSATVars', 'miniSATClauses', 'miniSATCalls', 'raSATClauses', 'decomposedLearnedClauses', 'UNSATLearnedClauses', 'unknownLearnedClauses', 'result', 'raSATResult', 'EQ', 'NEQ'])
    csvfile.close()
  for root, dirnames, filenames in os.walk(directory):
    for filename in fnmatch.filter(filenames, '*.smt2'):
      print "Checking ", filename
      sbox = initSbox * 10
      nVars = 0
      maxVars = 0
      nAPIs = 0
      iaTime = 0
      testingTime=0
      usTime=0
      parsingTime=0
      decompositionTime=0
      miniSATTime=0
      miniSATVars = 0;
      time=0
      miniSATCalls=0
      miniSATClauses = 0
      raSATClauses=0
      decomposedLearnedClauses=0
      UNSATLearnedClauses=0
      unknownLearnedClauses=0
      result='unknown'
      raSATResult = 'unknown'
      isEquation = '0'
      isNotEquation = '0'
      try:
        f = open(os.path.join(root, filename))
        for line in f:
          if line.startswith('(set-info :status'):
            result = line[18:len(line)-2]
        f.close()
      except IOError:
        result = 'unknown'
      bounds = ['lb=-10 10', 'lb=-inf inf']
      boundsNum = len(bounds)
      boundIndex = 0
      while (raSATResult != 'sat' and time < timeout and boundIndex < boundsNum):
        if raSATResult == 'unknown': 
          sbox = sbox / 10
        subprocess.call(["./raSAT", os.path.join(root, filename), bounds[boundIndex], 'sbox=' + str(sbox), 'tout=' + str(timeout-time)])
        try:
          with open(os.path.join(root, filename) + '.tmp', 'rb') as csvfile:
            reader = csv.reader(csvfile)
            output = reader.next()
            nVars = output[1]
            maxVars = output[2]
            nAPIs = output[3]
            time += float(output[4])
            iaTime += float(output[5])
            testingTime += float(output[6])
            usTime += float(output[7])
            parsingTime += float(output[8])
            decompositionTime += float(output[9])
            miniSATTime += float(output[10])
            miniSATVars += float(output[11])
            miniSATClauses += float(output[12])
            miniSATCalls += float(output[13])
            raSATClauses += float(output[14])
            decomposedLearnedClauses += float(output[15])
            UNSATLearnedClauses += float(output[16])
            unknownLearnedClauses += float(output[17])
            isEquation = output[18]
            isNotEquation = output[19]
            raSATResult = output[20]
            csvfile.close()
        except IOError:
          raSATResult = 'timeout'
        
        if raSATResult == 'unsat':
          boundIndex += 1
      
      if raSATResult == 'sat' or raSATResult == 'unsat':
        solvedProblems += 1     
      with open(os.path.join(directory, resultFile), 'a') as csvfile:
        spamwriter = csv.writer(csvfile)
        spamwriter.writerow([os.path.join(root, filename), nVars, maxVars, nAPIs, time, iaTime, testingTime, usTime, parsingTime, decompositionTime, miniSATTime, miniSATVars, miniSATClauses, miniSATCalls, raSATClauses, decomposedLearnedClauses, UNSATLearnedClauses, unknownLearnedClauses, result, raSATResult, isEquation, isNotEquation])
        csvfile.close()
     
      try:
        os.remove(os.path.join(root, filename) + '.tmp')
      except OSError:
        pass
        
      try:
        os.remove(os.path.join(root, filename)[:-5] + '.in')
      except OSError:
        pass
     
      try:
        os.remove(os.path.join(root, filename)[:-5] + '.out')
      except OSError:
        pass
        
      try:
        os.remove(os.path.join(root, filename)[:-5] + '.rs')
      except OSError:
        pass

  with open(os.path.join(directory, resultFile), 'a') as csvfile:
    spamwriter = csv.writer(csvfile)
    spamwriter.writerow(['Problem', 'nVars', 'maxVars', 'nAPIs', 'time', 'iaTime', 'testingTime', 'usCoreTime', 'parsingTime', 'decompositionTime', 'miniSATTime', 'miniSATVars', 'miniSATClauses', 'miniSATCalls', 'raSATClauses', 'decomposedLearnedClauses', 'UNSATLearnedClauses', 'unknownLearnedClauses', 'result', solvedProblems, 'EQ', 'NEQ'])
    csvfile.close()

#run ('zankl', -10, 10, 0.1, 500, 'with_dependency_sensitivity_restartSmallerBox_boxSelectionUsingSensitivity.xls')
#run ('QF_NRA/meti-tarski', -10, 10, 0.1, 500, 'with_dependency_sensitivity_restartSmallerBox_boxSelectionUsingSensitivity.xls')
#run ('Test/meti-tarski', -1, 1, 0.1, 60, 'result.xls')
#run ('Test/zankl', -10, 10, 0.1, 30, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/AProVE', -10, 10, 0.1, 60, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/calypto', -10, 10, 0.1, 60, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/leipzig', -10, 10, 0.1, 60, 'result.xls')
run ('Test/smtlib-20140121/QF_NIA/mcm', -10, 10, 0.1, 60, '1-5-8.csv')

#run ('Test/smtlib-20140121/QF_NRA/hycomp', -10, 10, 0.1, 60, '1-5-8.csv')
#run ('Test/test', -10, 10, 0.1, 60, 'result.csv')
