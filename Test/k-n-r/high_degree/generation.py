# run: python generation k n r

def generate (k_int, n_int, r_int):
  k = str(k_int)
  n = str(n_int)
  r = str(r_int)
  f = open (k + '-' + n + '-' + r + '.smt2', 'w')
  f.write ('(set-logic QF_NRA)\n')
  f.write ('(set-info :source | x_1^{1} + ... + x_{0}^{1} < 1 and (x_1 - {2})^{1} + ... + (x_{0} - {2})^{1} <1 |)\n'.format(k, n, r))

  # Declare variables:
  for point in range(1, k_int + 1):
    f.write('(declare-fun x_' + str(point) + ' () Real)\n')

  # Output first constraint:
  firstPolynomial = []

  for point in range(1, k_int+1):
    firstPolynomial.append('(* ' + ' '.join(['x_' + str(point)]*n_int)+')')
  
  firstPolynomial_string = ' '.join(firstPolynomial)
  if len(firstPolynomial) > 1:
    firstPolynomial_string = '+ ' + firstPolynomial_string

  f.write ('(assert (< (' + firstPolynomial_string + ') 1))\n')

  # Output second constraint
  secondPolynomial = []
  for point in range(1, k_int+1):
    secondPolynomial.append('(* ' + ' '.join(['(- x_' + str(point) + ' ' + r + ')']*n_int) + ')')
    
  secondPolynomial_string = ' '.join(secondPolynomial)
  if len(secondPolynomial) > 1:
    secondPolynomial_string = '+ ' + secondPolynomial_string
  f.write ('(assert (< (' + secondPolynomial_string + ') 1))\n')
  f.write("(check-sat)")
  
# high degrees:  
generate(2, 8, 1.83)
generate(2, 8, 1.84)
generate(2, 10, 1.86)
generate(2, 10, 1.87)
generate(2, 12, 1.88)
generate(2, 12, 1.89)
generate(2, 14, 1.90)
generate(2, 14, 1.91)
generate(2, 16, 1.91)
generate(2, 16, 1.92)
generate(2, 18, 1.92)
generate(2, 18, 1.93)
generate(2, 20, 1.93)
generate(2, 20, 1.94)
generate(2, 22, 1.93)
generate(2, 22, 1.94)
