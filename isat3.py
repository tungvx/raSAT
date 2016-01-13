import fnmatch
import os
import subprocess
import csv
import time
import concurrent.futures
import re
from subprocess import TimeoutExpired

LOWER_BOUND = "-1000"
UPPER_BOUND = "1000"

HYS = ".hys"
HYS_BOUNDED = ".hys.bound"
SMT2 = ".smt2"

TIME_OUT = "timeout"
SAT = "sat"
UNSAT = "unsat"
UNKNOWN = "unknown"

PROBLEM='Problem'
TIME='time'
ISAT_RESULT="isatResult"

HEADERS = [PROBLEM, TIME, ISAT_RESULT]

def generate_if_not_exists(root, smt2Filename, SOLVED_PROBLEM):
  solvedFileName = os.path.splitext(smt2Filename)[0]+SOLVED_PROBLEM
  # print (solvedFileName)
  if HYS == SOLVED_PROBLEM:
    if not os.path.isfile(os.path.join(root, solvedFileName)):
      subprocess.call(["./smt22hys", os.path.join(root, smt2Filename), os.path.join(root, solvedFileName), "-inf", "inf"])
    
  elif HYS_BOUNDED == SOLVED_PROBLEM:
    if not os.path.isfile(os.path.join(root, solvedFileName)):
      subprocess.call(["./smt22hys", os.path.join(root, smt2Filename), os.path.join(root, solvedFileName), LOWER_BOUND, UPPER_BOUND])
  
  return solvedFileName

def solve(args):
  (filename, root, timeout) = args

  result= {PROBLEM:os.path.join(root, filename)}

  startTime = time.time()
  try: 
    proc = subprocess.Popen(["./isat3", '-I', os.path.join(root, filename)],stdout=subprocess.PIPE,universal_newlines = True)
    iOut, iErr = proc.communicate(timeout=timeout)
  except TimeoutExpired:
    proc.kill()
    result[TIME] = time.time() - startTime
    result[ISAT_RESULT] = TIME_OUT
    return result
    
  result[TIME] = time.time() - startTime
  # print (iOut)
  if iOut == 'UNSATISFIABLE':
    result[ISAT_RESULT] = 'unsat'
  elif iOut.startswith('SATISFIABLE'):
    result[ISAT_RESULT] = 'sat'
  elif iOut.startswith('CANDIDATE SOLUTION'):
    result[ISAT_RESULT] = 'unknown'

  return result
    

def run(directory, timeout, resultFile, PROCESSES_NUM, SOLVED_PROBLEM):
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
        future = executor.submit(solve, (filename, root, timeout,))
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


#run("nonlinear/keymaera", 60, "isat3.xls")
# run("Test/", 10, "isat3.xls", 2, ".hys.bound")
# run("Test/", 10, "isat3.xls", 2, ".hys.bound")
#run ('zankl', -10, 10, 0.1, 500, 'with_dependency_sensitivity_restartSmallerBox_boxSelectionUsingSensitivity.xls')
#run ('QF_NRA/meti-tarski', -10, 10, 0.1, 500, 'with_dependency_sensitivity_restartSmallerBox_boxSelectionUsingSensitivity.xls')
#run ('Test/meti-tarski', -1, 1, 0.1, 60, 'result.xls')
#run ('Test/zankl', -10, 10, 0.1, 30, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/AProVE', -10, 10, 0.1, 60, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/calypto', -10, 10, 0.1, 60, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/leipzig', -10, 10, 0.1, 60, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/mcm', -10, 10, 0.1, 60, 'result.xls')

