import fnmatch
import os
import subprocess
import csv

matches = []
def run(directory, initLowerBound, initUpperBound, initSbox, timeout, resultFile):
  sbox = initSbox
  lowerBound = initLowerBound
  upperBound = initUpperBound
  with open(os.path.join(directory, resultFile), 'wb') as csvfile:
    spamwriter = csv.writer(csvfile)
    spamwriter.writerow(['Problem', 'nVars', 'maxVars', 'nAPIs', 'time', 'iaTime', 'testingTime', 'usCoreTime', 'parsingTime', 'decompositionTime', 'miniSATTime', 'miniSATVars', 'miniSATClauses', 'miniSATCalls', 'raSATClauses', 'decomposedLearnedClauses', 'UNSATLearnedClauses', 'unknownLearnedClauses', 'result', 'raSATResult', 'EQ', 'NEQ'])
    csvfile.close()
  for root, dirnames, filenames in os.walk(directory):
    for filename in fnmatch.filter(filenames, '*.smt2'):
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
      while (time < timeout and raSATResult == 'unknown'):
        subprocess.call(["./raSAT", os.path.join(root, filename), 'lb=' + str(lowerBound) + ' ' + str(upperBound), 'sbox=' + str(sbox), 'tout=' + str(timeout)])
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
            miniSATVars += int(output[11])
            miniSATClauses += int(output[12])
            miniSATCalls += int(output[13])
            raSATClauses += int(output[14])
            decomposedLearnedClauses += int(output[15])
            UNSATLearnedClauses += int(output[16])
            unknownLearnedClauses += int(output[17])
            isEquation = output[18]
            isNotEquation = output[19]
            raSATResult = output[20]
            csvfile.close()
        except IOError:
          result = 'timeout'
        
        if result == 'unsat':
          subprocess.call(["./raSAT", os.path.join(root, filename), 'lb=-inf inf', 'sbox=' + str(sbox), 'tout=' + str(timeout)])
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
              miniSATVars += int(output[11])
              miniSATClauses += int(output[12])
              miniSATCalls += int(output[13])
              raSATClauses += int(output[14])
              decomposedLearnedClauses += int(output[15])
              UNSATLearnedClauses += int(output[16])
              unknownLearnedClauses += int(output[17])
              isEquation = output[18]
              isNotEquation = output[19]
              raSATResult = output[20]
              csvfile.close()
          except IOError:
            raSATResult = 'timeout'
            
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

#run ('Test/smtlib-20140121/QF_NRA/zankl', -10, 10, 0.1, 500, 'result.xls')
#run ('Test/smtlib-20140121/QF_NRA/meti-tarski', -10, 10, 0.1, 60, 'result.xls')
#run ('Test/meti-tarski', -1, 1, 0.1, 60, 'result.xls')
run ('Test/zankl', -10, 10, 0.1, 30, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/AProVE', -10, 10, 0.1, 60, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/calypto', -10, 10, 0.1, 60, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/leipzig', -10, 10, 0.1, 60, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/mcm', -10, 10, 0.1, 60, 'result.xls')

