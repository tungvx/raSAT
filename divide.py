import fnmatch
import os
import subprocess
import csv
import sys
import shutil

matches = []
def run(directory, number):
	problems = 0
	for root, dirnames, filenames in os.walk(directory):
		for filename in fnmatch.filter(filenames, '*.smt2'):
			problems += 1
			folder = problems/int(number)
			newFolderPath = os.path.join(directory, str(folder), os.path.relpath(root, directory))
			if not os.path.exists(newFolderPath):
				os.makedirs(newFolderPath)

			shutil.copy(os.path.join(root, filename), newFolderPath)

#run ('zankl', -10, 10, 0.1, 500, 'with_dependency_sensitivity_restartSmallerBox_boxSelectionUsingSensitivity.xls')
#run ('QF_NRA/meti-tarski', -10, 10, 0.1, 500, 'with_dependency_sensitivity_restartSmallerBox_boxSelectionUsingSensitivity.xls')
#run ('Test/meti-tarski', -1, 1, 0.1, 60, 'result.xls')
#run ('Test/zankl', -10, 10, 0.1, 30, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/AProVE', -10, 10, 0.1, 60, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/calypto', -10, 10, 0.1, 60, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/leipzig', -10, 10, 0.1, 60, 'result.xls')
#run ('Test/smtlib-20140121/QF_NIA/mcm', -10, 10, 0.1, 60, 'result.xls')

run (sys.argv[1], sys.argv[2])
