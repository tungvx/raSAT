MROOT := $(shell pwd)
export MROOT
raSAT:
	cd camlside; make
	cd solver; make
	mkdir -p bin
	mv solver/raSAT bin/raSAT

clean:
	cd camlside; make clean; make c
	cd solver; make clean; make c	