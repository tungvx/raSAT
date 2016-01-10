import fnmatch
import os
import subprocess
import csv
import sys
import concurrent.futures
import copy
import re
from subprocess import TimeoutExpired
import time

SMT2=".smt2"

BOUNDED_SMT2 = '.bound'

LOWER_BOUND = '(- 1000)'
UPPER_BOUND = '1000'

TIME_OUT = "timeout"
ERROR = "error"

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

HEADERS=[PROBLEM, N_VARS, MAX_VARS, N_APIS, TIME, IA_TIME, TESTING_TIME, US_CORE_TIME,
         PARSING_TIME, DECOMPOSITION_TIME, MINISAT_TIME, MINISAT_VARS, MINISAT_CLAUSES,
         MINISAT_CALLS, RASAT_CLAUSES, DECOMPOSED_LEARNED_CLAUSES, UNKNOWN_LEARNED_CLAUSES,
         UNSAT_LEANRED_CLAUSES, RESULT, RASAT_RESULT, EQ, NEQ]


default_result = {
          PROBLEM:"",
          N_VARS: 0,
          MAX_VARS: 0,
          N_APIS: 0,
          TIME: 0,
          IA_TIME: 0,
          TESTING_TIME: 0,
          US_CORE_TIME: 0,
          PARSING_TIME: 0,
          DECOMPOSITION_TIME: 0,
          MINISAT_TIME: 0,
          MINISAT_VARS: 0,
          MINISAT_CLAUSES: 0,
          MINISAT_CALLS: 0,
          RASAT_CLAUSES: 0,
          DECOMPOSED_LEARNED_CLAUSES: 0,
          UNSAT_LEANRED_CLAUSES: 0,
          UNKNOWN_LEARNED_CLAUSES: 0,
          RESULT: 'unknown',
          RASAT_RESULT: 'unknown', 
          EQ: '0',
          NEQ: '0'
}

def gen_bounds(root, filename):
  filePath = os.path.join(root, filename)
  with open(filePath, 'r') as inputFile:
    content = inputFile.read()
    # content = content.replace('(check-sat)', '').strip()
    # content = content.replace('(exit)', '').strip()

    asserts = []
    for m in re.finditer(r"\(declare-fun (.*) \(\) (Real|Int)\)", content):
      asserts.append('(assert (>= {} {}))'.format (m.group(1), LOWER_BOUND))
      asserts.append('(assert (<= {} {}))'.format (m.group(1), UPPER_BOUND))

    # content += '\n' + '\n'.join(asserts)
    # content += '\n(check-sat)\n'
    # content += '(exit)\n'

    # add assertions into the content:
    content = content.replace('(check-sat)', '\n'.join(asserts) + '\n(check-sat)')

    # print (content)

    # Write content into new file:
    with open(filePath + BOUNDED_SMT2, 'w+') as boundFile:
      boundFile.write(content)

    return filename + BOUNDED_SMT2

def generate_if_not_exists(root, smt2Filename, SOLVED_PROBLEM):
  if SMT2 == SOLVED_PROBLEM:
    return smt2Filename
  elif BOUNDED_SMT2 == SOLVED_PROBLEM:
    if os.path.isfile(os.path.join(root, smt2Filename+SOLVED_PROBLEM)):
      return smt2Filename+SOLVED_PROBLEM
    return gen_bounds(root, smt2Filename)


def remove_tmp(root, filename):
  # Remove temp files
  filePath = os.path.join(root, filename)
  try:
    os.remove(os.path.join(root, filename) + '.tmp')
  except OSError:
    pass
    
  try:
    os.remove(os.path.splitext(filePath)[0] + '.in')
  except OSError:
    pass
 
  try:
    os.remove(os.path.splitext(filePath)[0] + '.out')
  except OSError:
    pass
    
  try:
    os.remove(os.path.splitext(filePath)[0] + '.rs')
  except OSError:
    pass

def set_result(result, root, filename):
  try:
    with open(os.path.join(root, filename) + '.tmp', 'r') as csvfile:
      reader = csv.reader(csvfile)
      output = reader.__next__()

      # set numbers
      for index in range(1, 4):
        result[HEADERS[index]] = output[index]

      for index in range(4, 18):
        # print (index, HEADERS[index], output[index])
        result[HEADERS[index]] += float(output[index])
      

      result[EQ] = output[18]
      result[NEQ] = output[19]
      result[RASAT_RESULT] = output[20]
  except IOError:
    result[RASAT_RESULT] = 'timeout'

def solve(args):
  # global root, initSbox, initLowerBound, initUpperBound, timeout
  (filename,root, initSbox, initLowerBound, initUpperBound, timeout) = args

  # print ('solving', filename)

  startingTime = time.time()

  result = copy.deepcopy(default_result)
  result[PROBLEM] = os.path.join(root, filename)

  sbox = initSbox * 10
  lowerBound = initLowerBound
  upperBound = initUpperBound

  #try to get the result of the problem:
  try:
    f = open(os.path.join(root, filename))
    '(set-info :status sat)'
    m = re.search('\(set-info :status (sat|unsat|unknown)\)', f.read())
    if m:
      result[RESULT]=m.group(1)
  except IOError:
    pass

  # Run command to solve constraints:
  bounds = ['lb=-10 10', 'lb=-inf inf']
  boundsNum = len(bounds)
  boundIndex = 0
  while (result[RASAT_RESULT] != 'sat' and timeout > 0 and boundIndex < boundsNum):
    # Remove temp files
    remove_tmp(root, filename)

    try:
      subprocess.call(["./raSAT", os.path.join(root, filename), bounds[boundIndex]], timeout=timeout)
    except TimeoutExpired:
      result[RASAT_RESULT] = TIME_OUT
      # Remove temp files
      remove_tmp(root, filename)
      return result
    except:
      result[RASAT_RESULT] = ERROR
      # Remove temp files
      remove_tmp(root, filename)
      return result

    set_result(result, root, filename)

    # Remove temp files
    remove_tmp(root, filename)

    boundIndex += 1

    timeout = timeout - (time.time() - startingTime)

  return result



def run(directory, initLowerBound, initUpperBound, initSbox, timeout, resultFile, PROCESSES_NUM, SOLVED_PROBLEM):
  with concurrent.futures.ProcessPoolExecutor(PROCESSES_NUM) as executor:
    with open(os.path.join(directory, resultFile), 'w+', 1) as csvfile:
      spamwriter = csv.DictWriter(csvfile, fieldnames=HEADERS)
      spamwriter.writeheader()
      solvedFiles = []

      for root, dirnames, filenames in os.walk(directory):
        for smt2Filename in fnmatch.filter(filenames, '*'+SMT2):
          solvedFileName = generate_if_not_exists(root, smt2Filename, SOLVED_PROBLEM)
          solvedFiles.append((solvedFileName, root))


      # results = executor.map(solve, [(filename,root, initSbox, initLowerBound, 
      #                                 initUpperBound, timeout) 
      #                               for (filename, root) in solvedFiles])

      # for result in results:
      #   for key in result:
      #     result[key] = str(result[key])
      #   spamwriter.writerow(result)


      futureObjects = []
      for (filename, root) in solvedFiles:
        future = executor.submit(solve, (filename, root, initSbox, initLowerBound, 
                                          initUpperBound, timeout,))
        futureObjects.append(future)
      for future in futureObjects:
        try:
          result = future.result()
        except Exception as e:
          print (e)
          continue
        for key in result:
          result[key] = str(result[key])
        spamwriter.writerow(result)          

      

# gen_bounds('./Test/smtlib-20140121/QF_NRA/zankl/', 'matrix-1-all-11.smt2', -1000, 1000)
# run ('Test/test', -10, 10, 0.1, 10, '1-5-8-11.csv', 2, '.smt2')
#run ('zankl', -10, 10, 0.1, 500, 'with_dependency_sensitivity_restartSmallerBox_boxSelectionUsingSensitivity.xls')
#run ('QF_NRA/meti-tarski', -10, 10, 0.1, 500, 'with_dependency_sensitivity_restartSmallerBox_boxSelectionUsingSensitivity.xls')
#run ('Test/meti-tarski', -1, 1, 0.1, 60, 'result.xls')
#run ('Test/zankl', -10, 10, 0.1, 30, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/AProVE', -10, 10, 0.1, 60, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/calypto', -10, 10, 0.1, 60, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/leipzig', -10, 10, 0.1, 60, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/mcm', -10, 10, 0.1, 60, 'result.xls')

