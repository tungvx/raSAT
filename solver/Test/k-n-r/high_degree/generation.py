# run: python generation k n r

def generate (k_int, n_int, r_int):
  k = str(k_int)
  n = str(n_int)
  r = str(r_int)
  f = open (k + '-' + n + '-' + r + '.smt2', 'w')
  f.write ('(set-logic QF_NRA)\n')
  f.write ('(set-info :source | x_1^n + x_2^n + ... + x_k^n < 1 and (x_1 - r)^n + (x_2 - r)^n + ... + (x_k - r)^n <1 |)\n')

  # Declare variables:
  for point in range(1, k_int + 1):
    f.write('(declare-fun x_' + str(point) + ' () Real)\n')

  # Output first constraint:
  firstPolynomial = '+ (^ x_1 ' + n + ') '

  for point in range(2, k_int):
    firstPolynomial = '+ (' + firstPolynomial + '(^ x_' + str(point) + ' ' + n + ')) '
    
  firstPolynomial = firstPolynomial + '(^ x_' + k + ' ' + n + ') '
  f.write ('(assert (< (' + firstPolynomial + ') 1))\n')

  # Output second constraint
  secondPolynomial = '+ (^ (- x_1 ' + r + ') ' + n + ') '
  for point in range(2, k_int):
    secondPolynomial = '+ (' + secondPolynomial + '(^ (- x_' + str(point) + ' ' + r + ') ' + n + ')) '
    
  secondPolynomial = secondPolynomial + '(^ (- x_' + k + ' ' + r + ') ' + n + ') '
  f.write ('(assert (< (' + secondPolynomial + ') 1))\n')
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
