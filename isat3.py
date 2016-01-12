import fnmatch
import os
import subprocess
import csv
import datetime
import signal

class Alarm(Exception):
    pass

def alarm_handler(signum, frame):
    raise Alarm

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
    
#run("nonlinear/keymaera", 60, "isat3.xls")
#run("test", 60, "isat3.xls")
#run ('zankl', -10, 10, 0.1, 500, 'with_dependency_sensitivity_restartSmallerBox_boxSelectionUsingSensitivity.xls')
#run ('QF_NRA/meti-tarski', -10, 10, 0.1, 500, 'with_dependency_sensitivity_restartSmallerBox_boxSelectionUsingSensitivity.xls')
#run ('Test/meti-tarski', -1, 1, 0.1, 60, 'result.xls')
#run ('Test/zankl', -10, 10, 0.1, 30, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/AProVE', -10, 10, 0.1, 60, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/calypto', -10, 10, 0.1, 60, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/leipzig', -10, 10, 0.1, 60, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/mcm', -10, 10, 0.1, 60, 'result.xls')

