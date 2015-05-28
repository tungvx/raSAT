# HOW TO RUN:
# "python infixToPrefix.py filePath/directoryPath"
# In the case of filePath: The corresponding .smt2 file will be generated. The extension of the input file will be replaced by '.smt2'
# In the case of directory: All the file with extension '.poly' will be used as the input to generate .smt2 files.


from sets import Set

def stack_empty(stack):
  return 1 if(len(stack) == 0) else 0
        
def stack_top(stack):
  return stack[-1]

def push_stack(stack,ele):
  stack.append(ele)

def pop_stack(stack):
  return stack.pop()

def operand(opr):
  return 1 if(not(operator(opr)) and (opr != "(") and (opr != ")")) else 0

def operator(opr):
  return 1 if opr in ["+","-","*","/","^","$"] else 0

def precedence(opr):
  if((opr == "^") or (opr == "$")):return(7)
  if(opr == "*"):return(5)
  if(opr == "/"):return(5)
  if(opr == "+"):return(3)
  if(opr == "-"):return(3)
  if(opr == "("):return(2)
  if(opr == ")"):return(1)

def check(newOperator, topOfStackOperator):
  if newOperator == topOfStackOperator:
    return False
  if newOperator == '-' and topOfStackOperator == '+':
    return False
  return precedence(newOperator) <= precedence(topOfStackOperator)

def infix_to_prefix(infix_expression):
  variables = Set([])
  prefix_list = []
  stack = []
  infix_expression = infix_expression[::-1]
  #print infix_expression
  infix_list = list(infix_expression)
  i = 0
  while i < len(infix_list):
    if operand(infix_list[i]):
      tmp = ''
      while i < len(infix_list) and operand(infix_list[i]):
        tmp += infix_list[i]
        i += 1
      #prefix_list.append(i)
      prefix_list.append(tmp.strip()[::-1])
      try:
        float(tmp.strip())
      except ValueError:
        variables.add(tmp.strip()[::-1])
    if i < len(infix_list):
      if operator(infix_list[i]):
        while((not(stack_empty(stack))) and check(infix_list[i], stack_top(stack))):
          prefix_list.append(stack_top(stack))
          pop_stack(stack)
        push_stack(stack,infix_list[i])
      if(infix_list[i] == ")"):
        push_stack(stack,infix_list[i])
      if(infix_list[i] == "("):
        while(stack_top(stack) != ")"):
          append_operator = pop_stack(stack)
          prefix_list.append(append_operator)
        pop_stack(stack)
      i += 1
  while(not(stack_empty(stack))):
    if(stack_top(stack) == ")"):
      pop_stack(stack)
    else:
      prefix_list.append(pop_stack(stack))
  #print prefix_list       
  prefix_expression = ''
  operandNum = []
  for val in prefix_list[::-1]:
    if operator(val):
      prefix_expression += '(' + val + ' '
      operandNum.append(0)
    else: # operands
      prefix_expression += val
      if operandNum:
        operandNum[-1] += 1
      while operandNum and operandNum[-1] == 2:
        prefix_expression += ')'
        del operandNum[-1]
        if operandNum:
          operandNum[-1] += 1
      prefix_expression += ' '

  return variables, prefix_expression.strip()

import re
import csv
def extract_constraints(mapleExp):
  #remove spaces:
  mapleExp = mapleExp.replace(" ", "")  
  # search for constraints in {}
  matchConstraints = re.search('\{(.*)\}', mapleExp)
  constraintsList = []
  if matchConstraints:
    constraintsList = csv.reader([matchConstraints.group(1)]).next()
  else:
    matchConstraints = re.search('solve\((.*?),.*\]\)', mapleExp)
    if matchConstraints:
      constraintsList = [matchConstraints.group(1)]
    else:
      matchConstraints = re.search('solve\((.*)\)', mapleExp)
      if matchConstraints:
        constraintsList = [matchConstraints.group(1)]
  variables = Set([])
  prefixList = []
  for constraint in constraintsList:
    matchExpression = re.search('(.*?)([><=]=?)(.*)', constraint)
    if matchExpression:
      (leftVariables, leftPrefix) = infix_to_prefix(matchExpression.group(1))
      (rightVariables, rightPrefix) = infix_to_prefix(matchExpression.group(3))
      variables = variables.union(leftVariables).union(rightVariables)
      prefixList.append('(' + matchExpression.group(2) + ' ' + leftPrefix + ' ' + rightPrefix + ')')
    else:
      (newVariables, newPrefix) = infix_to_prefix (constraint)
      variables = variables.union(newVariables)
      prefixList.append('(= ' + newPrefix + ' 0)')
  return variables, prefixList

import fnmatch
import os
import sys

def main(filename):
  f = open (filename, 'r')  
  for line in f:
    if line.startswith('solve('):
      (variables, results) = extract_constraints(line)
      g = open(os.path.splitext(filename)[0] + '.smt2', 'w')
      g.write('(set-logic QF_NRA)\n')
      for variable in variables:
        g.write('(declare-fun ' + variable +  ' () Real)\n')
      for result in results:
        g.write('(assert ' + result + ')\n')
      g.write('(check-sat)\n')
      g.close()
  f.close()
  #print result

if len(sys.argv) >= 2:
  inputFile = sys.argv[1]
  if os.path.isfile(inputFile):
    main(inputFile)
  elif os.path.isdir(inputFile):
    for root, dirnames, filenames in os.walk(inputFile):
      for filename in fnmatch.filter(filenames, '*.poly'):
        main(os.path.join(root, filename))
  else:
    print 'Wrong input'
else:
  print 'Missing file'
#for root, dirnames, filenames in os.walk('Test/first_benchmarks/'):
#  for filename in fnmatch.filter(filenames, '*.poly'):
#    main(os.path.join(root, filename))
