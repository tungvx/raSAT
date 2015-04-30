import os
import fnmatch
import csv

errors = set()
unfinished = set()

for root, dirnames, filenames in os.walk('.'):
  for filename in fnmatch.filter(filenames, '*.csv'):
    try:
      with open(os.path.join(root, filename), 'rb') as csvfile:
        reader = csv.reader(csvfile)
        last = reader.next()
        for output in reader:
          if output[1] == '0' and output[2] == '0' and output[3] == '0':
            errors.add (filename)
          last = output
        if last[0] != 'Problem':
          unfinished.add (filename)
        csvfile.close()
    except IOError:
      print 'error'
print len(errors), "Errors: ", errors
print len(unfinished), "Unfinisheds: ", unfinished
