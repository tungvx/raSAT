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
            while i < len(infix_list) and not operator(infix_list[i]):
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

def main(filename):
    f = open (filename, 'r')
    infix_expression = f.read()
    (variables, result) = infix_to_prefix(infix_expression)
    f.close()
    #print result
    f = open(filename[:-5] + '.smt2', 'w')
    f.write('(set-logic QF_NRA)\n')
    for variable in variables:
      f.write('(declare-fun ' + variable +  ' () Real)\n')
    f.write('(assert ( < ' + result + '0))\n')
    f.write('(check-sat)\n')
    f.close()

import fnmatch
import os

for root, dirnames, filenames in os.walk('Test/first_benchmarks/'):
  for filename in fnmatch.filter(filenames, '*.poly'):
    main(os.path.join(root, filename))
    
    
    
    
    
    
    
    
    
    
    
    
