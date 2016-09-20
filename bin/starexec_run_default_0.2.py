#!/usr/bin/env python
# Run: python smt.py filename.smt2 timeout
# timeout is in seconds

import os
import subprocess
import sys
import stat
import time

current_path = os.path.dirname(os.path.realpath(__file__))
 
def remove_tmp (filename, version):
  try:
    os.remove(filename + '.' + version + '.tmp')
  except OSError:
    pass
    
  try:
    os.remove(os.path.splitext(filename)[0] + '.' + version +  '.out')
  except OSError:
    pass
    
  try:
    os.remove(os.path.splitext(filename)[0] + '.' + version +  '.in')
  except OSError:
    pass

def run_raSAT (filename, bounds, sbox, timeout):
  startTime = time.time()  

  raSATResult = "unknown"

  # remove tmps files:
  remove_tmp(filename, "0.2")

  subprocess.call([os.path.join(current_path, "./raSAT-0.2"), filename, bounds, 'sbox=' + str(sbox), 'tout=' + str(timeout-(time.time() - startTime))])
  
  try:
    with open(filename + '.0.2.tmp', 'r') as outfile:
      raSATResult = outfile.read().rstrip()
      outfile.close()
      if raSATResult == "unknown":
        sbox /= 10
        remove_tmp(filename, "0.2")
        proc2 = subprocess.Popen([os.path.join(current_path, "./raSAT-0.2"), filename, bounds, 'sbox=' + str(sbox), 'tout=' + str(timeout-(time.time() - startTime))])
  except IOError:
    pass  
    
    
  return raSATResult, sbox

def run(filename, initLowerBound, initUpperBound, sbox, timeout):
  lowerBound = initLowerBound
  upperBound = initUpperBound
  raSATResult = "unknown"
  startTime = time.time()
  while (raSATResult == 'unknown'):
    (raSATResult, sbox) = run_raSAT(filename, 'lb=' + str(lowerBound) + ' ' + str(upperBound), sbox, timeout - (time.time() - startTime))
    
    if raSATResult == 'unsat':
      (raSATResult, sbox) = run_raSAT(filename, 'lb=-inf inf', sbox, timeout - (time.time() - startTime))  
  print (raSATResult)

  # remove tmps files:
  remove_tmp(filename, "0.2")

# get timeout from environment
timeout = float(os.environ.get('STAREXEC_CPU_LIMIT'))

run(sys.argv[1], -10, 10,  0.1, timeout)