#!/usr/bin/env python2
# Run: python smt.py filename.smt2 timeout
# timeout is in seconds

import fnmatch
import os
import subprocess
import csv
import sys
import time
import stat

RASAT = "raSAT-0.2"
RASAT_RESULT = "raSATResult"
UNKNOWN = 'unknown'

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
  # remove tmps files:
  remove_tmp(filename, "0.2")

  command = os.path.join(current_path, RASAT) + " " + filename + " " + bounds + " " + sbox + " " + timeout
  proc = subprocess.Popen(command, shell=True)
  # print command
  proc.wait()
  try:
    with open(filename + '.0.2.tmp', 'r') as outfile:
      raSATResult = outfile.read().rstrip()
      outfile.close()
  except IOError:
    raSATResult = 'timeout'

  return raSATResult

def run(filename, initLowerBound, initUpperBound, timeout):
  lowerBound = initLowerBound
  upperBound = initUpperBound
  raSATResult = UNKNOWN
  sbox = 1.
  while (raSATResult == 'unknown'):
    sbox = sbox / 8.
    raSATResult = run_raSAT(filename, 'lb="' + str(lowerBound) + ' ' + str(upperBound) + '"',
                            "sbox="+str(sbox), "tout="+str(timeout))
    
    if raSATResult == 'unsat':
      raSATResult = run_raSAT(filename, 'lb="-inf inf"', 
                            "sbox="+str(sbox),"tout="+str(timeout))  
  print raSATResult

  # remove tmps files:
  remove_tmp(filename, "0.2")


st = os.stat(os.path.join(current_path, RASAT))
os.chmod(os.path.join(current_path, RASAT), st.st_mode | stat.S_IEXEC)
if len(sys.argv) < 2:
  print 'Syntax is:\n python smt.py filename.smt2 timeout'
else:
  timeout = float(os.environ.get('STAREXEC_CPU_LIMIT'))
  run(sys.argv[1], -8, 8, timeout)

