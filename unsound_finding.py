import fnmatch
import os
import subprocess
import csv
import datetime
import signal
import sys

class Alarm(Exception):
    pass

def alarm_handler(signum, frame):
    raise Alarm

def run(directory, timeout):
  index = 0
  for root, dirnames, filenames in os.walk(directory):
    for filename in fnmatch.filter(filenames, '*.smt2'):
      index += 1
      print index, '.', os.path.join(root, filename),
      try:
        f = open(os.path.join(root, filename))
        for line in f:
          if line.startswith('(set-info :status'):
            result = line[18:len(line)-2]
        f.close()
      except IOError:
        result = 'unknown'

      print result,
      
      raSAT_result = 'unknown'
      signal.signal(signal.SIGALRM, alarm_handler)
      signal.alarm(timeout)
      try: 
        proc = subprocess.Popen(["./bin/raSAT", os.path.join(root, filename), 'lb=-inf inf'], stdout=subprocess.PIPE)
        raSAT_result=proc.stdout.readline().strip()
        signal.alarm(0)
      except Alarm:
        raSAT_result = 'timeout'
        pass

      print raSAT_result
      if result != 'unknown' and  raSAT_result != 'unknown' and raSAT_result != 'timeout' and raSAT_result != result:
        print 'unsound problem at: ', os.path.join(root, filename)
        sys.exit()
      


run(sys.argv[1], int(sys.argv[2]))

    
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

