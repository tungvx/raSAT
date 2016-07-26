import os
import fnmatch
import csv

errors = set()
unfinished = set()

PROBLEM='Problem' 
TIME='time'
ISAT_RESULT='iSATResult'


HEADERS=[PROBLEM, TIME, ISAT_RESULT]

unsoud = []

with open(os.path.join('.', 'sumUp.xls'), 'wb') as outfile:
  spamwriter = csv.writer(outfile)
  spamwriter.writerow(HEADERS)
  solvedProblem = 0
  for root, dirnames, filenames in os.walk('.'):
    for filename in fnmatch.filter(filenames, '*.csv'):
      with open(os.path.join(root, filename), 'rb') as csvfile:
        reader = csv.reader(csvfile)
        last = reader.next()
        for output in reader:
          spamwriter.writerow(output)
          result = output[HEADERS.index(ISAT_RESULT)]
          if result == 'sat' or result == 'unsat':
            solvedProblem += 1
            
  HEADERS[HEADERS.index(ISAT_RESULT)] = solvedProblem
  spamwriter.writerow(HEADERS)  
print len(errors), "Errors: ", errors
print len(unfinished), "Unfinisheds: ", unfinished
print len(errors - unfinished), "error finisheds: ", errors - unfinished
