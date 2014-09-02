EXEC      = raSAT
DEPDIR    = mtl utils

# Uncomment the next two lines if you want to use iRRAM.
CFLAGS = -std=c++11 -DiRRAM_INC
LFLAGS = -g -Wall -O2 -IiRRAM_2013_01/installed/include -Xlinker -rpath -Xlinker iRRAM_2013_01/installed/lib -LiRRAM_2013_01/installed/lib -liRRAM -lmpfr -lm -lgmp -lpthread -L/usr/lib64

#CFLAGS = -g

include $(MROOT)/mtl/template.mk

c:
	rm -f $(EXEC).exe

