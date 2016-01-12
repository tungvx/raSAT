import fnmatch
import os
import subprocess
import csv

LOWER_BOUND = -1000
UPPER_BOUND = 1000

HYS = ".hys"
HYS_BOUNDED = ".hys.bound"

def generate_if_not_exists(root, smt2Filename, SOLVED_PROBLEM):
  if HYS == SOLVED_PROBLEM:
    subprocess.call(["./smt22hys", os.path.join(root, filename), -1000, 1000])
    return smt2Filename
  elif BOUNDED_SMT2 == SOLVED_PROBLEM:
    if os.path.isfile(os.path.join(root, smt2Filename+SOLVED_PROBLEM)):
      return smt2Filename+SOLVED_PROBLEM
    return gen_bounds(root, smt2Filename)

def run(directory, timeout, resultFile):
  solvedProblems = 0
  with open(os.path.join(directory, resultFile), 'wb') as csvfile:
    spamwriter = csv.writer(csvfile)
    spamwriter.writerow(['Problem', 'time', 'isat3 Result'])
    csvfile.close()
  for root, dirnames, filenames in os.walk(directory):
    for filename in fnmatch.filter(filenames, '*.hys'):
      startTime = datetime.datetime.now()
      result = 'unknown'
      signal.signal(signal.SIGALRM, alarm_handler)
      signal.alarm(timeout)
      try: 
        proc = subprocess.Popen(["./isat3", '-I', os.path.join(root, filename)], stdout=subprocess.PIPE)
        result=proc.stdout.readline().strip()
        signal.alarm(0)
      except Alarm:
        result = 'timeout'
        
      time = datetime.datetime.now() - startTime
      time = time.seconds + time.microseconds/1E6  
      if result == 'UNSATISFIABLE':
        result = 'unsat'
        solvedProblems += 1
      elif result.startswith('SATISFIABLE'):
        result = 'sat'
        solvedProblems += 1
      elif result.startswith('CANDIDATE SOLUTION'):
        result = 'unknown'
      with open(os.path.join(directory, resultFile), 'a') as csvfile:
        spamwriter = csv.writer(csvfile)
        spamwriter.writerow([os.path.join(root, filename), time, result])
        csvfile.close()

  with open(os.path.join(directory, resultFile), 'a') as csvfile:
    spamwriter = csv.writer(csvfile)
    spamwriter.writerow(['Problem', 'time', solvedProblems])
    csvfile.close()
    

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


#run("nonlinear/keymaera", 60, "isat3.xls")
run("test", 60, "isat3.xls", 2, ".hys")
#run ('zankl', -10, 10, 0.1, 500, 'with_dependency_sensitivity_restartSmallerBox_boxSelectionUsingSensitivity.xls')
#run ('QF_NRA/meti-tarski', -10, 10, 0.1, 500, 'with_dependency_sensitivity_restartSmallerBox_boxSelectionUsingSensitivity.xls')
#run ('Test/meti-tarski', -1, 1, 0.1, 60, 'result.xls')
#run ('Test/zankl', -10, 10, 0.1, 30, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/AProVE', -10, 10, 0.1, 60, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/calypto', -10, 10, 0.1, 60, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/leipzig', -10, 10, 0.1, 60, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/mcm', -10, 10, 0.1, 60, 'result.xls')

