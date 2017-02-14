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

# high variables:
generate(3, 4, 1.51)
generate(3, 4, 1.52)
generate(4, 4, 1.41)
generate(4, 4, 1.42)
generate(5, 4, 1.33)
generate(5, 4, 1.34)
generate(6, 4, 1.27)
generate(6, 4, 1.28)
generate(3, 6, 1.66)
generate(3, 6, 1.67)
generate(4, 6, 1.58)
generate(4, 6, 1.59)
generate(5, 6, 1.52)
generate(5, 6, 1.53)
generate(6, 6, 1.48)
generate(6, 6, 1.49)
generate(3, 8, 1.74)
generate(3, 8, 1.75)
generate(4, 8, 1.68)
generate(4, 8, 1.69)
