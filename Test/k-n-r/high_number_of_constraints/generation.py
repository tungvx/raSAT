# run: python generation k n r

def generate (k_int, n_int, r_int):
  k = str(k_int)
  n = str(n_int)
  r = str(r_int)
  f = open (k + '-' + n + '-' + r + '.smt2', 'w')
  f.write ('(set-logic QF_NRA)\n')
  f.write ('(set-info :source | for i = 1, ..., {0}: x_i^{1} + x_{4}(i+1)%{3}{5}^{1} < 1 and (x_i - r)^{1} + (x_{4}(i+1)%{3}{5} - {2})^{1} <1 |)\n'.format(k, n, r, str(k_int+1), '{', '}'))

  # Declare variables:
  for point in range(0, k_int + 1):
    f.write('(declare-fun x_' + str(point) + ' () Real)\n')

  for point in range(0, k_int+1):
    f.write ('(assert (< (+ (* ' + ' '.join(['x_' + str(point)]*n_int) + 
            ') (* ' + ' '.join(['x_' + str((point+1)%(k_int+1))]*n_int) + ')) 1))\n')

  # Output second constraint
  for point in range(0, k_int+1):
    f.write ('(assert (< (+ (* ' + 
              ' '.join(['(- x_' + str(point) + ' ' + r + ')']*n_int) + 
            ') (* ' + ' '.join(['(- x_' + str((point+1)%(k_int+1)) + ' ' + r + ')']*n_int) + 
            ')) 1))\n') 
  
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
