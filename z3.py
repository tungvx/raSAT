import fnmatch
import os
import subprocess
import csv
import concurrent.futures
import re
from subprocess import TimeoutExpired
import time

SMT2=".smt2"
BOUNDED_SMT2 = '.bound'

TIME_OUT = "timeout"
SAT = "sat"
UNSAT = "unsat"
UNKNOWN = "unknown"

PROBLEM='Problem'
TIME='time'
RESULT = 'result'
Z3_RESULT="z3Result"

HEADERS = [PROBLEM, TIME, RESULT, Z3_RESULT]

LOWER_BOUND = '(- 1000)'
UPPER_BOUND = '1000'

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
    return gen_bounds(root, smt2Filename)

def remove_file(filePath):
  try:
    os.remove(filePath)
  except OSError:
    pass

def solve(args):
  (smt2Filename, SOLVED_PROBLEM, root, timeout) = args

  filename = generate_if_not_exists(root, smt2Filename, SOLVED_PROBLEM)

  result= {PROBLEM:os.path.join(root, filename)}

  #try to get the result of the problem:
  try:
    f = open(os.path.join(root, filename))
    '(set-info :status sat)'
    m = re.search('\(set-info :status (sat|unsat|unknown)\)', f.read())
    if m:
      result[RESULT]=m.group(1)
  except IOError:
    pass

  startTime = time.time()
  try: 
    proc = subprocess.Popen(["./bin/z3", os.path.join(root, filename), '-T:' + str(timeout)],stdout=subprocess.PIPE,universal_newlines = True)
    iOut, iErr = proc.communicate(timeout=timeout)
  except TimeoutExpired:
    proc.kill()
    result[TIME] = time.time() - startTime
    result[Z3_RESULT] = TIME_OUT
    # remove_file(result[PROBLEM])
    return result
    
  result[TIME] = time.time() - startTime
  result[Z3_RESULT] = iOut.strip()

  # print (result[DREAL_RESULT])
  # print (result)
  # remove_file(result[PROBLEM])
  return result
    

def run(directory, timeout, resultFile, PROCESSES_NUM, SOLVED_PROBLEM):
  with concurrent.futures.ProcessPoolExecutor(PROCESSES_NUM) as executor:
    with open(os.path.join(directory, resultFile), 'w+', 1) as csvfile:
      spamwriter = csv.DictWriter(csvfile, fieldnames=HEADERS)
      spamwriter.writeheader()
      smt2Files = []

      for root, dirnames, filenames in os.walk(directory):
        for filename in filenames:
          if filename.endswith(SMT2):
            smt2Files.append((filename, root))


      # results = executor.map(solve, [(filename,root, initSbox, initLowerBound, 
      #                                 initUpperBound, timeout) 
      #                               for (filename, root) in solvedFiles])

      # for result in results:
      #   for key in result:
      #     result[key] = str(result[key])
      #   spamwriter.writerow(result)


      futureObjects = []
      for (smt2Filename, root) in smt2Files:
        future = executor.submit(solve, (smt2Filename, SOLVED_PROBLEM, root, timeout,))
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
# run("test", 60, "dReal.csv", 2, BOUNDED_SMT2)
#run ('zankl', -10, 10, 0.1, 500, 'with_dependency_sensitivity_restartSmallerBox_boxSelectionUsingSensitivity.xls')
#run ('QF_NRA/meti-tarski', -10, 10, 0.1, 500, 'with_dependency_sensitivity_restartSmallerBox_boxSelectionUsingSensitivity.xls')
#run ('Test/meti-tarski', -1, 1, 0.1, 60, 'result.xls')
#run ('Test/zankl', -10, 10, 0.1, 30, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/AProVE', -10, 10, 0.1, 60, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/calypto', -10, 10, 0.1, 60, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/leipzig', -10, 10, 0.1, 60, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/mcm', -10, 10, 0.1, 60, 'result.xls')

