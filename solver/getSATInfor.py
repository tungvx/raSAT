import fnmatch
import os
import subprocess
import csv

matches = []
def run(directory, resultFile):
  with open(os.path.join(directory, resultFile), 'wb') as csvfile:
    spamwriter = csv.writer(csvfile)
    spamwriter.writerow(['Problem', 'result'])
    csvfile.close()
  for root, dirnames, filenames in os.walk(directory):
    for filename in fnmatch.filter(filenames, '*.smt2'):
      result = 'unknown'
      try:
        f = open(os.path.join(root, filename))
        for line in f:
          if line.startswith('(set-info :status'):
            result = line[18:len(line)-2]
        f.close()
      except IOError:
        result = 'unknown'
            
      with open(os.path.join(directory, resultFile), 'a') as csvfile:
        spamwriter = csv.writer(csvfile)
        spamwriter.writerow([os.path.join(root, filename), result])
        csvfile.close()

run ('Test/smtlib-20140121/QF_NRA/zankl', 'satInfo.xls')
run ('Test/smtlib-20140121/QF_NRA/meti-tarski', 'satInfo.xls')
run ('Test/meti-tarski', 'satInfo.xls')
run ('Test/zankl', 'satInfo.xls')
run ('Test/smtlib-20140121/QF_NIA/AProVE', 'satInfo.xls')
run ('Test/smtlib-20140121/QF_NIA/calypto', 'satInfo.xls')
run ('Test/smtlib-20140121/QF_NIA/leipzig', 'satInfo.xls')
run ('Test/smtlib-20140121/QF_NIA/mcm', 'satInfo.xls')

