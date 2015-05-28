import os
import shutil

for i in range(881147, 889976):
  tmp = (i-881147)/882
  if not os.path.exists(str(tmp)):
    os.makedirs(str(tmp))
  shutil.move(str(i), str(tmp))  
