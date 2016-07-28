#!/usr/bin/env python
# Run: python smt.py filename.smt2 timeout
# timeout is in seconds

import os
import subprocess
import sys
import stat
import time
import re

 
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
  remove_tmp(filename, "0.3")

  proc2 = subprocess.Popen(["./raSAT-0.2", filename, bounds, 'sbox=' + str(sbox), 'tout=' + str(timeout-(time.time() - startTime))])
  proc3 = subprocess.Popen(["./raSAT-0.3", filename, bounds])
  while True:
    if proc2.poll():
      # try read output of 0.2
      try:
        with open(filename + '.0.2.tmp', 'rb') as outfile:
          raSATResult = outfile.read().rstrip()
          outfile.close()
          if raSATResult == "unknown":
            sbox /= 10
            remove_tmp(filename, "0.2")
            proc2 = subprocess.Popen(["./raSAT-0.2", filename, bounds, 'sbox=' + str(sbox), 'tout=' + str(timeout-(time.time() - startTime))])
      except IOError:
        pass  
    
    if proc3.poll():      
      # try read output of 0.3
      try:
        with open(filename + '.0.3.tmp', 'rb') as outfile:
          raSATResult = outfile.read().rstrip()
          outfile.close()
      except IOError:
        pass

    if raSATResult == "sat" or raSATResult == "unsat":
      if not proc3.poll():
        proc3.kill()
      if not proc2.poll():
        proc2.kill()
      break

    time.sleep(0.01)
  

  return raSATResult, sbox

def string_of_bounds(bound):
  if bound >= 0:
    return str(bound)
  else:
    return "(- " + str(abs(bound)) + ")"

def gen_exclude_bounds(filename, initLowerBound, initUpperBound):
  with open(filename, 'r') as inputFile:
    content = inputFile.read()
    # content = content.replace('(check-sat)', '').strip()
    # content = content.replace('(exit)', '').strip()

    asserts = []
    for m in re.finditer(r"\(declare-fun (.*) \(\) (Real|Int)\)", content):
      asserts.append('(> {} {})'.format (m.group(1), string_of_bounds(initUpperBound)))
      asserts.append('(> {} {})'.format (m.group(1), string_of_bounds(initLowerBound)))

    # content += '\n' + '\n'.join(asserts)
    # content += '\n(check-sat)\n'
    # content += '(exit)\n'

    # add assertions into the content:
    content = content.replace('(check-sat)', 
        "(assert (or {}))".format(' '.join(asserts)) + '\n(check-sat)')

    # print (content)

    BOUNDED_SMT2 = ".bound"
    # Write content into new file:
    with open(filename + BOUNDED_SMT2, 'w+') as boundFile:
      boundFile.write(content)

    return filename + BOUNDED_SMT2

def run(filename, initLowerBound, initUpperBound, sbox, timeout):
  lowerBound = initLowerBound
  upperBound = initUpperBound
  raSATResult = "unknown"
  startTime = time.time()
  while (raSATResult == 'unknown'):
    (raSATResult, sbox) = run_raSAT(filename, 'lb=' + str(lowerBound) + ' ' + str(upperBound), sbox, timeout - (time.time() - startTime))
    
    if raSATResult == 'unsat':
      # create a new input file where 
      boundFile = gen_exclude_bounds(filename, initLowerBound, initUpperBound)
      (raSATResult, sbox) = run_raSAT(boundFile, 'lb=-inf inf', sbox, timeout - (time.time() - startTime))  
      # remove tmps files:
      remove_tmp(boundFile, "0.2")
      remove_tmp(boundFile, "0.3")
      try:
        os.remove(boundFile)
      except OSError:
        pass
  print raSATResult

  # remove tmps files:
  remove_tmp(filename, "0.2")
  remove_tmp(filename, "0.3")

# get timeout from environment
timeout = float(os.environ.get('STAREXEC_CPU_LIMIT'))

run(sys.argv[1], -10, 10,  0.1, timeout)