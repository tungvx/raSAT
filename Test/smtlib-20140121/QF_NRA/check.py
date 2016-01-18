import os
import fnmatch
import csv
import sys
import re

errors = set()
unfinished = set()

PROBLEM='Problem' 
N_VARS='nVars'
MAX_VARS='maxVars' 
N_APIS='nAPIs'
TIME='time'
IA_TIME='iaTime' 
TESTING_TIME='testingTime' 
US_CORE_TIME='usCoreTime'
PARSING_TIME='parsingTime'
DECOMPOSITION_TIME='decompositionTime'
MINISAT_TIME='miniSATTime'
MINISAT_VARS='miniSATVars'
MINISAT_CLAUSES='miniSATClauses'
MINISAT_CALLS='miniSATCalls'
RASAT_CLAUSES='raSATClauses'
DECOMPOSED_LEARNED_CLAUSES='decomposedLearnedClauses'
UNSAT_LEANRED_CLAUSES='UNSATLearnedClauses'
UNKNOWN_LEARNED_CLAUSES='unknownLearnedClauses'
RESULT='result'

RASAT = "raSAT"
ISAT = "isat"
DREAL = "dReal"
tools = {
  RASAT:'raSATResult',
  ISAT: 'isatResult',
  DREAL: 'isatResult'
}

TOOL_RESULT=tools[sys.argv[3]]

EQ='EQ'
NEQ='NEQ'

HEADERS=[PROBLEM, N_VARS, MAX_VARS, N_APIS, TIME, IA_TIME, TESTING_TIME, US_CORE_TIME,
         PARSING_TIME, DECOMPOSITION_TIME, MINISAT_TIME, MINISAT_VARS, MINISAT_CLAUSES,
         MINISAT_CALLS, RASAT_CLAUSES, DECOMPOSED_LEARNED_CLAUSES, UNKNOWN_LEARNED_CLAUSES,
         UNSAT_LEANRED_CLAUSES, RESULT, TOOL_RESULT, EQ, NEQ]

HEADERS_MAP = {}         
for header in HEADERS:
  HEADERS_MAP[header] = header

unsoud = []

# Read status:
status = {}
with open(sys.argv[1]) as csvfile:
  reader = csv.DictReader(csvfile)
  for row in reader:
    status[row[PROBLEM]] = row

# print status
# exit()

with open(sys.argv[2], 'wb') as outfile:
  spamwriter = csv.DictWriter(outfile,fieldnames=HEADERS)
  spamwriter.writeheader()
  solvedProblem = 0
  for root, dirnames, filenames in os.walk('.'):
    for filename in fnmatch.filter(filenames, '*.csv'):
      try:
        with open(os.path.join(root, filename), 'rb') as csvfile:
          reader = csv.DictReader(csvfile)
          # last = reader.next()
          for output in reader:
            # add status information:
            while (True):
              m = re.search('^(QF_N(R|I)A/(\d/))', output[PROBLEM])
              if not m:
                break
              replacedString = m.group().replace(m.group(3), '')
              output[PROBLEM] = output[PROBLEM].replace(m.group(), replacedString)

            key = output[PROBLEM].replace(".bound", "")
            key = key.replace(".hys", ".smt2")
            try:
              problemStatus = status[key]
            except:
              print output[PROBLEM],'not found'
              exit()

            # copy status into output
            output[EQ] = problemStatus[EQ]
            output[NEQ] = problemStatus[NEQ]
            output[RESULT] = problemStatus[RESULT]

            spamwriter.writerow(output)
            result = output[TOOL_RESULT]
            standard = output[RESULT]
            if result == 'sat' or result == 'unsat':
              if standard != 'unknown' and result != standard:
                print 'unsound:', output[PROBLEM], standard, result
              solvedProblem += 1

          #   if output[1] == '0' and output[2] == '0' and output[3] == '0':
          #     errors.add (filename)
          #   last = output
          # if last[0] != 'Problem':
          #   unfinished.add (filename)
          # csvfile.close()
      except IOError:
        print 'error'           
  HEADERS_MAP[TOOL_RESULT] = solvedProblem
  spamwriter.writerow(HEADERS_MAP)  
print len(errors), "Errors: ", errors
print len(unfinished), "Unfinisheds: ", unfinished
print len(errors - unfinished), "error finisheds: ", errors - unfinished
