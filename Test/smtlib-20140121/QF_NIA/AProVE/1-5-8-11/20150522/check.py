import os
import fnmatch
import csv

errors = set()
unfinished = set()

with open(os.path.join('.', 'sumUp.xls'), 'wb') as outfile:
  spamwriter = csv.writer(outfile)
  spamwriter.writerow(['Problem', 'nVars', 'maxVars', 'nAPIs', 'time', 'iaTime', 'testingTime', 'usCoreTime', 'parsingTime', 'decompositionTime', 'miniSATTime', 'miniSATVars', 'miniSATClauses', 'miniSATCalls', 'raSATClauses', 'decomposedLearnedClauses', 'UNSATLearnedClauses', 'unknownLearnedClauses', 'result', 'raSATResult', 'EQ', 'NEQ'])
  for root, dirnames, filenames in os.walk('.'):
    for filename in fnmatch.filter(filenames, '*.csv'):
      try:
        with open(os.path.join(root, filename), 'rb') as csvfile:
          reader = csv.reader(csvfile)
          last = reader.next()
          for output in reader:
            spamwriter.writerow(output)
            if output[1] == '0' and output[2] == '0' and output[3] == '0':
              errors.add (filename)
            last = output
          if last[0] != 'Problem':
            unfinished.add (filename)
          csvfile.close()
      except IOError:
        print 'error'  
  outfile.close()              
print len(errors), "Errors: ", errors
print len(unfinished), "Unfinisheds: ", unfinished
print len(errors - unfinished), "error finisheds: ", errors - unfinished
