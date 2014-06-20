EXEC      = raSAT
DEPDIR    = mtl utils

# Uncomment the next two lines if you want to use iRRAM.
CFLAGS = -std=c++11 -DiRRAM_INC
LFLAGS = -Wall -O2 -I/usr/local/include -Xlinker -rpath -Xlinker /usr/local/lib -L/usr/local/lib -liRRAM -lmpfr -lm -lgmp -lpthread -L/usr/lib64

CFLAGS += -g

include $(MROOT)/mtl/template.mk

c:
	rm -f $(EXEC).exe

