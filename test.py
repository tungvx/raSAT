import fnmatch
import os
import subprocess
import csv
import sys
import concurrent.futures
import copy
import re

SOLVED_PROBLEM="*.smt2"

PROBLEM='Problem' 
N_VARS='nVars'
MAX_VARS='maxVars' 
N_APIS='nAPIs'
TIME='time'
IA_TIME='iaTime' 
TESTING_TIME='testingTime' 
US_CORE_TIME='usCoreTime'
PARSING_TIME='parsingTime'
DECOMPOSITION_TIME='decompositionTime'
MINISAT_TIME='miniSATTime'
MINISAT_VARS='miniSATVars'
MINISAT_CLAUSES='miniSATClauses'
MINISAT_CALLS='miniSATCalls'
RASAT_CLAUSES='raSATClauses'
DECOMPOSED_LEARNED_CLAUSES='decomposedLearnedClauses'
UNSAT_LEANRED_CLAUSES='UNSATLearnedClauses'
UNKNOWN_LEARNED_CLAUSES='unknownLearnedClauses'
RESULT='result'
RASAT_RESULT='raSATResult'
EQ='EQ'
NEQ='NEQ'

HEADERS=[PROBLEM, N_VARS, MAX_VARS, N_APIS, TIME, IA_TIME, TESTING_TIME, US_CORE_TIME
         PARSING_TIME, DECOMPOSITION_TIME, MINISAT_TIME, MINISAT_VARS, MINISAT_CLAUSES
         MINISAT_CALLS, RASAT_CLAUSES, DECOMPOSED_LEARNED_CLAUSES, UNKNOWN_LEARNED_CLAUSES
         RESULT, RASAT_RESULT, EQ, NEQ]

default_result = {PROBLEM:os.path.join(root, filename),
          N_VARS: 0,
          MAX_VARS: 0,
          N_APIS: 0,
          TIME: 0,
          IA_TIME: 0,
          TESTING_TIME: 0,
          US_CORE_TIME 0,
          PARSING_TIME: parsingTime,
          DECOMPOSITION_TIME: 0,
          MINISAT_TIME: 0,
          MINISAT_VARS: 0,
          MINISAT_CLAUSES: 0,
          MINISAT_CALLS: 0,
          RASAT_CLAUSES: 0,
          DECOMPOSED_LEARNED_CLAUSES: 0,
          UNSAT_LEANRED_CLAUSES 0,
          UNKNOWN_LEARNED_CLAUSES: 0,
          RESULT: 'unknown',
          raSATResult: 'unknown', 
          EQ: '0',
          NEQ: '0'}

def remove_tmp(root, filename):
  # Remove temp files
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

def solve(filename):
  global root, initSbox, initLowerBound, initUpperBound, timeout

  remove_tmp(root, filename)

  result = copy.deepcopy(default_result)

  sbox = initSbox * 10
  lowerBound = initLowerBound
  upperBound = initUpperBound

  #try to get the result of the problem:
  try:
    f = open(os.path.join(root, filename))
    '(set-info :status sat)'
    m = re.search('\(set-info :status (sat|unsat|unknown))\)', f.read())
    if m:
      result[RESULT]=m.group(1)
  except IOError:
    pass

  # Run command to solve constraints:
  try:
    subprocess.run(["./raSAT", os.path.join(root, filename), 
                  'lb=' + str(lowerBound) + ' ' + str(upperBound)], timeout=timeout)
  except TimeoutExpired:
    return result

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



def run(directory, initLowerBound, initUpperBound, initSbox, timeout, resultFile):
  with concurrent.futures.ProcessPoolExecutor() as executor:
    for root, dirnames, filenames in os.walk(directory):
      results = executor.map(solve, fnmatch.filter(filenames, PROBLEM))    

  
  #sbox = initSbox
  solvedProblems = 0
  with open(os.path.join(directory, resultFile), 'wb') as csvfile:
    spamwriter = csv.DictWriter(csvfile, fieldnames=HEADERS)
    spamwriter.writeheader()
    spamwriter.writerows(results)
  for root, dirnames, filenames in os.walk(directory):
    for filename in fnmatch.filter(filenames, '*.smt2'):
      
      
        
      if raSATResult == 'unsat':

        # Remove temp files
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

        signal.signal(signal.SIGALRM, alarm_handler)
        signal.alarm(timeout)
        try:
          proc = subprocess.Popen(["./raSAT", os.path.join(root, filename), 'lb=' + str(lowerBound) + ' ' + str(upperBound)])
          proc.wait()
          signal.alarm(0)
        except Alarm:
          proc.kill()
          raSAT_result = 'timeout'
          pass

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
      
      if raSATResult == 'sat' or raSATResult == 'unsat':
        solvedProblems += 1     
      with open(os.path.join(directory, resultFile), 'a') as csvfile:
        spamwriter = csv.writer(csvfile)
        spamwriter.writerow([os.path.join(root, filename), nVars, maxVars, nAPIs, time, iaTime, testingTime, usTime, parsingTime, decompositionTime, miniSATTime, miniSATVars, miniSATClauses, miniSATCalls, raSATClauses, decomposedLearnedClauses, UNSATLearnedClauses, unknownLearnedClauses, result, raSATResult, isEquation, isNotEquation])
        csvfile.close()


      # if (raSATResult == 'timeout'):
      #   sys.exit()

      # print os.path.join(root, filename), raSATResult
     
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

run ('Test/smtlib-20140121/QF_NRA/zankl', -10, 10, 0.1, 10, '1-5-8-11_zankl.csv')
#run ('zankl', -10, 10, 0.1, 500, 'with_dependency_sensitivity_restartSmallerBox_boxSelectionUsingSensitivity.xls')
#run ('QF_NRA/meti-tarski', -10, 10, 0.1, 500, 'with_dependency_sensitivity_restartSmallerBox_boxSelectionUsingSensitivity.xls')
#run ('Test/meti-tarski', -1, 1, 0.1, 60, 'result.xls')
#run ('Test/zankl', -10, 10, 0.1, 30, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/AProVE', -10, 10, 0.1, 60, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/calypto', -10, 10, 0.1, 60, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/leipzig', -10, 10, 0.1, 60, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/mcm', -10, 10, 0.1, 60, 'result.xls')

