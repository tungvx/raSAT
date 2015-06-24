import fnmatch
import os
import subprocess
import csv
import sys
import time

matches = []
def run(filename, initLowerBound, initUpperBound, initSbox, timeout):
  sbox = initSbox * 10
  lowerBound = initLowerBound
  upperBound = initUpperBound
  raSATResult = 'unknown'
  runTime = 0
  startTime = time.time()
  while (runTime < timeout and raSATResult == 'unknown'):
    sbox = sbox / 10
    subprocess.call(["./raSAT", filename, 'lb=' + str(lowerBound) + ' ' + str(upperBound), 'sbox=' + str(sbox), 'tout=' + str(timeout-runTime)])
    try:
      with open(filename + '.tmp', 'rb') as outfile:
        raSATResult = outfile.read().rstrip()
        outfile.close()
    except IOError:
      raSATResult = 'timeout'
    
    if raSATResult == 'unsat':
      subprocess.call(["./raSAT", filename, 'lb=-inf inf', 'sbox=' + str(sbox), 'tout=' + str(timeout-runTime)])
      try:
        with open(filename + '.tmp', 'rb') as outfile:
          raSATResult = outfile.read().rstrip()
          outfile.close()
      except IOError:
        raSATResult = 'timeout'
    runTime = time.time() - startTime
  print raSATResult

run(sys.argv[1], -10, 10, 0.1, float(os.environ['STAREXEC_CPU_LIMIT']))

