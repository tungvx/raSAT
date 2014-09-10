# run: python generation k n r

def generate (k_int, n_int, r_int):
  k = str(k_int)
  n = str(n_int)
  r = str(r_int)
  f = open (k + '-' + n + '-' + r + '.smt2', 'w')
  f.write ('(set-logic QF_NRA)\n')
  f.write ('(set-info :source | x_1^n + x_2^n + ... + x_k^n < 1 and (x_1 - r)^n + (x_2 - r)^n + ... + (x_k - r)^n <1 |)\n')

  # Declare variables:
  for point in range(0, k_int + 1):
    f.write('(declare-fun x_' + str(point) + ' () Real)\n')

  # Output first constraint:
  firstPolynomial = '+ (^ x_1 ' + n + ') '

  for point in range(0, k_int):
    f.write ('(assert (< (+ (^ x_' + str(point) + ' ' + n + ') (^ x_' + str(point+1) + ' ' + n + ')) 1))\n')
    
  f.write ('(assert (< (+ (^ x_' + k + ' ' + n + ') (^ x_0 ' + n + ')) 1))\n')

  # Output second constraint
  secondPolynomial = '+ (^ (- x_1 ' + r + ') ' + n + ') '
  for point in range(0, k_int):
    f.write ('(assert (< (+ (^ (- x_' + str(point) + ' ' + r + ') ' + n + ') (^ (- x_' + str(point+1) + ' ' + r + ') ' + n + ')) 1))\n') 
    
  f.write ('(assert (< (+ (^ (- x_' + k + ' ' + r + ') ' + n + ') (^ (- x_0 ' + r + ') ' + n + ')) 1))\n')
  
  # final commands:
  f.write("(check-sat)")
  
# high number of constraints:  
generate(3, 6, 1.78)
generate(3, 6, 1.79)
generate(5, 6, 1.78)
generate(5, 6, 1.79)
generate(7, 6, 1.78)
generate(7, 6, 1.79)
generate(9, 6, 1.78)
generate(9, 6, 1.79)
generate(11, 6, 1.78)
generate(11, 6, 1.79)
generate(13, 6, 1.78)
generate(13, 6, 1.79)
generate(15, 6, 1.78)
generate(15, 6, 1.79)
