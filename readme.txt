raSAT: The SMT solver for Polynomial Constraints on Real numbers


1. REQUIREMENTS
	- packages: iRRAM, make, automake, gcc, ocaml, zlib, curses
	- Environments: 64 bit systems (cygwin 64 bit, linux systems)

2. BUILDING raSAT 
	cd Camlside
	make					(Build the library Camllib.a)
	cd ..
	export MROOT=<raSAT-dir>
	cd solver
	make					(Build raSAT in the folder solver)

3. RUNNING raSAT 
	raSAT <smt2 file> bound="lb ub"
	
Examples: 
raSAT Test/zankl/matrix-1-all-2.smt2 bound="-10 10" sbox=0.5 tout=120

Notes: sbox and tout (timeout setting) are optional (default values are 0.1 for sbox and timeout in 60 seconds).
 
