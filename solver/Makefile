EXEC      = raSAT
DEPDIR    = mtl utils

# Uncomment the next two lines if you want to use iRRAM.
#CFLAGS = -std=c++0x -DiRRAM_INC -I./iRRAM_2013_01/installed/include
#LFLAGS = -g -Wall -O2 -I./iRRAM_2013_01/installed/include -Xlinker -rpath -Xlinker ./iRRAM_2013_01/installed/lib -L./iRRAM_2013_01/installed/lib -liRRAM -lmpfr -lm -lgmp -lpthread -L/usr/lib64

CFLAGS = -g -std=c++0x
LFLAGS = -lstdc++

include $(MROOT)/mtl/template.mk

c:
	rm -f $(EXEC).exe

