import os
import fnmatch
import csv

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
RASAT_RESULT='raSATResult'
EQ='EQ'
NEQ='NEQ'

HEADERS=[PROBLEM, N_VARS, MAX_VARS, N_APIS, TIME, IA_TIME, TESTING_TIME, US_CORE_TIME,
         PARSING_TIME, DECOMPOSITION_TIME, MINISAT_TIME, MINISAT_VARS, MINISAT_CLAUSES,
         MINISAT_CALLS, RASAT_CLAUSES, DECOMPOSED_LEARNED_CLAUSES, UNKNOWN_LEARNED_CLAUSES,
         UNSAT_LEANRED_CLAUSES, RESULT, RASAT_RESULT, EQ, NEQ]

unsoud = []

with open(os.path.join('.', 'sumUp.xls'), 'wb') as outfile:
  spamwriter = csv.writer(outfile)
  spamwriter.writerow(HEADERS)
  solvedProblem = 0
  for root, dirnames, filenames in os.walk('.'):
    for filename in fnmatch.filter(filenames, '*.csv'):
      try:
        with open(os.path.join(root, filename), 'rb') as csvfile:
          reader = csv.reader(csvfile)
          last = reader.next()
          for output in reader:
            spamwriter.writerow(output)
            result = output[HEADERS.index(RASAT_RESULT)]
            standard = output[HEADERS.index(RESULT)]
            if result == 'sat' or result == 'unsat':
              if standard != 'unknown' and result != standard:
                print 'unsound:', output[HEADERS.index(PROBLEM)], standard, result
              solvedProblem += 1

          #   if output[1] == '0' and output[2] == '0' and output[3] == '0':
          #     errors.add (filename)
          #   last = output
          # if last[0] != 'Problem':
          #   unfinished.add (filename)
          # csvfile.close()
      except IOError:
        print 'error'           
  HEADERS[HEADERS.index(RASAT_RESULT)] = solvedProblem
  spamwriter.writerow(HEADERS)  
print len(errors), "Errors: ", errors
print len(unfinished), "Unfinisheds: ", unfinished
print len(errors - unfinished), "error finisheds: ", errors - unfinished
