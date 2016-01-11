import fnmatch
import os
import sys

def run(directory):
  index = 0
  for root, dirnames, filenames in os.walk(directory):
    for filename in fnmatch.filter(filenames, '*.smt2'):
      with open(os.path.join(root, filename), 'r') as smt2File:
        if '/' in smt2File.read():
          print "Found div in", os.path.join(root, filename)
          return
      


run(sys.argv[1])
