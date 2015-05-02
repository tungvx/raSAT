import fnmatch
import os
import subprocess
import csv
import sys

benchmarks = set()
def run(filteredfile, filteringfile, outputfile):
  try:
    with open(filteringfile, 'rb') as csvfile:
      reader = csv.reader(csvfile)
      for output in reader:
        if output[0] != 'Problem':
          benchmarks.add(output[0])
      csvfile.close()
      with open(outputfile, 'wb') as outfile:
        with open(filteredfile, 'rb') as csvfile:
          reader = csv.reader(csvfile)
          spamwriter = csv.writer(outfile)
          spamwriter.writerow(reader.next())
          for output in reader:
            if output[0].replace('nonlinear/', 'QF_NRA/').replace('.hys', '.smt2') in benchmarks:
              spamwriter.writerow(output)
          csvfile.close()
        outfile.close()
  except IOError:
    print 'error' 

run (sys.argv[1], sys.argv[2], sys.argv[3])
