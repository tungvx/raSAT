/*****************************************************************************************[Main.cc]
 Copyright (c) 2003-2006, Niklas Een, Niklas Sorensson
 Copyright (c) 2007-2010, Niklas Sorensson

 Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 associated documentation files (the "Software"), to deal in the Software without restriction,
 including without limitation the rights to use, copy, modify, merge, publish, distribute,
 sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in all copies or
 substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
 OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 **************************************************************************************************/

#include <errno.h>

#include <signal.h>
#include <zlib.h>

#include <stdio.h>

#include "utils/System.h"
#include "utils/ParseUtils.h"
#include "utils/Options.h"
#include "solver/Dimacs.h"
#include "solver/Solver.h"
#include "exactRealArithmetics.h"

//Caml interaction
#include <iostream>
#include "modwrap.h"
#include "file.h"

extern "C" {
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
}
using namespace std;
using namespace Minisat;

//=================================================================================================

void printStats(Solver& solver) {
	double cpu_time = cpuTime();
	double mem_used = memUsedPeak();
	printf("restarts              : %" PRIu64" \n", solver.starts);
	printf("conflicts             : %-12" PRIu64 "   (%.0f /sec)\n", solver.conflicts , solver.conflicts /cpu_time);
	printf("decisions             : %-12" PRIu64 "   (%4.2f %% random) (%.0f /sec)\n", solver.decisions, (float)solver.rnd_decisions*100 / (float)solver.decisions, solver.decisions /cpu_time);
	printf("propagations          : %-12" PRIu64 "   (%.0f /sec)\n", solver.propagations, solver.propagations/cpu_time);
	printf("conflict literals     : %-12" PRIu64 "   (%4.2f %% deleted)\n", solver.tot_literals, (solver.max_literals - solver.tot_literals)*100 / (double)solver.max_literals);
	if (mem_used != 0)
		printf("Memory used           : %.2f MB\n", mem_used);
	printf("CPU time              : %g s\n", cpu_time);
}

// bumping activities of some variables
void solver_bumping(Solver& solv, string bump_vars) {
	//remove space at the beginning and the end of a string
	string s = remove_space(bump_vars);
	int l = s.length();
	int size = 0;
	for (int i = 0; i < l; i++)
		if (s[i] == ' ')
			size++;
	int *a = new int[size + 1];
	int pos = 0, prev = 0;
	for (int i = 0; i < l; i++) {
		if (s[i] == ' ') {
			a[pos] = atoi(s.substr(prev, i - prev).c_str());
			prev = i;
			pos++;
		}
	}
	a[pos] = atoi(s.substr(prev, l - prev).c_str());
	for (int i = 0; i <= pos; i++) {
		//cout <<a[i]<<":";
		int var = abs(a[i]) - 1;
		solv.varBumpActivity(var);
	}
}

// add a list of lit into a vector:
void addLitListToVector(vec<Lit> &litVector, string litList) {
	string s = remove_space(litList);
	int l = s.length();
	int size = 0;
	for (int i = 0; i < l; i++)
		if (s[i] == ' ')
			size++;
	int *a = new int[size + 1];
	int pos = 0, prev = 0;
	for (int i = 0; i < l; i++) {
		if (s[i] == ' ') {
			a[pos] = atoi(s.substr(prev, i - prev).c_str());
			prev = i;
			pos++;
		}
	}
	a[pos] = atoi(s.substr(prev, l - prev).c_str());
	for (int i = 0; i <= pos; i++) {
		//cout <<a[i]<<":";
		int var = abs(a[i]) - 1;
		bool sign = (a[i] > 0) ? false : true;
		litVector.push(mkLit(var, sign));
	}
}

double miniSATVars = 0;
double clauses = 0;
// add some clauses to solver which stores information in reason
int solver_addClause(Solver& solv, string reason) {
	int addedClauses = 0;
	//remove space at the beginning and the end of a string
	string s = remove_space(reason);
	int l = s.length();
	int size = 0;
	for (int i = 0; i < l; i++)
		if (s[i] == ' ')
			size++;
	int *a = new int[size + 1];
	int pos = 0, prev = 0;
	for (int i = 0; i < l; i++) {
		if (s[i] == ' ') {
			a[pos] = atoi(s.substr(prev, i - prev).c_str());
			prev = i;
			pos++;
		}
	}
	a[pos] = atoi(s.substr(prev, l - prev).c_str());
	int prevPos = 0, endPos = 0;
	while (endPos <= pos) {
		if (a[endPos] == 0) {
			vec < Lit > cl;
			for (int i = prevPos; i < endPos; i++) {
				int var = abs(a[i]) - 1;
				if (miniSATVars < var + 1)
					miniSATVars = var + 1;
				bool sign = (a[i] > 0) ? false : true;
				cl.push(mkLit(var, sign));
			}
			solv.addClause(cl);
			clauses++;
			addedClauses++;
			prevPos = endPos + 1;
		}
		endPos++;
	}
	return addedClauses;
}

static Solver* solver;
// Terminate by notifying the solver and back out gracefully. This is mainly to have a test-case
// for this feature of the Solver as it may take longer than an immediate call to '_exit()'.
static void SIGINT_interrupt(int signum) {
	solver->interrupt();
}

// Note that '_exit()' rather than 'exit()' has to be used. The reason is that 'exit()' calls
// destructors and may cause deadlocks if a malloc/free function happens to be running (these
// functions are guarded by locks for multithreaded use).
static void SIGINT_exit(int signum) {
	printf("\n");
	printf("*** INTERRUPTED ***\n");
	if (solver->verbosity > 0) {
		printStats(*solver);
		printf("\n");
		printf("*** INTERRUPTED ***\n");
	}
	_exit(1);
}

//=================================================================================================
// Main:

int main(int argc, char** argv) {
	double initial_time = cpuTime();
	/* Initialize Caml code */
	//caml_main(argv);
	caml_startup(argv); //for bytecode compilation in ocaml

	//Preprocess for parameter and generate MiniSat form here
	if (argc < 3) {
		cout << "Missing arguments...";
		return 0;
	}

	//generate ebg format form from SMT2 format (raSAT input form)
	double lb = getLoBound(argv[2]);
	double ub = getUpBound(argv[2]);

	char *smtfile = argv[1];
	int nvar = num_var(smtfile);
	//cout <<endl<< "number of variables: "<<nvar<<endl;

	string str = get_listvars(smtfile, nvar, lb);
	char *sInt = new char[str.size() + 1];
	strcpy(sInt, str.c_str());

//	cout << endl << "string of interval: " << str << endl;

	string strAs = get_cons(smtfile, nvar);
	char *sAs = new char[strAs.size() + 1];
	strcpy(sAs, strAs.c_str());

//	cout << endl << "constraints: " << strAs << endl;

	CAMLlocal1 (smt);
	caml_register_global_root(&smt);

	smt = caml_genSmtForm(sInt, sAs, lb, ub);
	string smtContent = String_val(Field(smt, 0));
//	cout << endl << "raSAT input form: " << smtContent << endl;
	caml_remove_global_root(&smt);

	//return 1;

	//cout <<endl<<"lo: "<<lo<<endl;
	//cout <<endl<<"up: "<<up<<endl;

	//generate raSAT input file *.rs
	string sfile = "";
	sfile = toFileRs(argv[1]);

	char * rsFile = new char[sfile.size() + 1];
	strcpy(rsFile, sfile.c_str());

	int r = writeFile(rsFile, smtContent);
	if (r != 1) {
		cout << "Can not create " << rsFile << " file!";
		return 0;
	}

	string strIntv = smt_getintv(rsFile);
	//printf("%s", strIntv);
	//cout <<endl<<strIntv<<endl;
	char *sIntv = new char[strIntv.size() + 1];
	strcpy(sIntv, strIntv.c_str());
	//printf("\n%s\n", sIntv);

	string strAss = smt_ass(rsFile);
	char *sAss = new char[strAss.size() + 1];
	strcpy(sAss, strAss.c_str());

	//cout <<endl<<strAss<<endl;

	//nCons store the number of constraints
	int nCons = caml_getNumCons(sAss);

	//cout <<endl<<"Number of constraints:  "<< nCons<<endl;

	double totalTime = 0;
	double iaTime = 0;
	double testingTime = 0;
	double usTime = 0; // time for calculating unsat core
	double miniSATTime = 0; // time of minisat running
	double ocamlTime = 0;
	double parsingTime = 0;
	double decompositionTime = 0;
	double maxClauses = 0;
	double miniSATCalls = 0;
	double nDecompositions = 0;
	double UNSATLearnedClauses = 0;
	double unknownLearnedClauses = 0;
	char *sta = new char[2048];
	char *typeIA = new char[5];
	char *sSAT = new char[4000];

	//interval arithmetic type
	//Jan 14, 2014: force ia = "af2"
	int ia = 0;
	//if (argc >= 3)
	// ia = getIA (argv[2]);
	ia = 2; // ia = "AF2"

	//bound for dynamic interval decomposition
	double esl = 0.1;	//default searching box is 0.1
	if (argc >= 4)
		//esl = atof(argv[3]);
		esl = getSearchingBox(argv[3]);
	//cout <<endl<<"esl = "<<esl<<endl;

	//get the timeout, Jan 14, 2014
	double timeout = 60.0; //default timeout = 60 seconds
	if (argc >= 5)
		timeout = getTimeout(argv[4]);

	//cout <<endl <<"time out" <<timeout<<endl;

	//get information for polynomial constraints: number of variables, constraints

	//ebg -> form of minisat in string 
	CAMLlocal1 (intv);
	caml_register_global_root(&intv);
	intv = caml_genSatForm(sIntv, esl);
	//nVars store the number variables for SAT content
	int nVars = Int_val(Field(intv, 0));
	string satContent = String_val(Field(intv, 1));
	caml_remove_global_root(&intv);

	sfile = toFilein(argv[1]);
	char * inFile = new char[sfile.size() + 1];
	strcpy(inFile, sfile.c_str());

	r = writeFile(inFile, satContent);
	if (r != 1) {
		cout << "Can not create " << inFile << " file!";
		return 0;
	}

	sfile = toFileout(argv[1]);
	char * outFile = new char[sfile.size() + 1];
	strcpy(outFile, sfile.c_str());

	FILE* res = (argc >= 2) ? fopen(outFile, "wb") : NULL;

	/*End preprocess side*/

	/*
	 // Check constraints with infinite bounds first
	 
	 int inf_res = caml_InfCheck (sIntv, sAss);
	 if (inf_res == -1) { // UNSAT by infinte interval bound checking
	 totalTime  = cpuTime(); 
	 sprintf (typeIA, "%s","ICI");
	 printf("\n=============================[ Problem Statistic ]==============================\n");
	 sprintf(sta, "\nInput problem       : %s ", argv[1]);	
	 sprintf(sta, "%s\nNumber of variables   : %d ", sta, nVars);	
	 sprintf(sta, "%s\nNumber of constraints : %d ", sta, nCons);	
	 sprintf(sta, "%s\nTotal CPU time        : %g seconds", sta, totalTime);	
	 sprintf(sta, "%s\nInterval Arithmetic   : %s", sta, typeIA);
	 sprintf(sta, "%s\nTimeout setting   : %g seconds\n", sta, timeout);
	 sprintf(sta, "%sResult                : UNSAT\n\n", sta);
	 cout <<sta;	fprintf(res, sta);
	 fclose(res);
	 }
	 else
	 */
	{ //Start process for combination of SAT and IA  
	  //double initial_time = cpuTime();
		bool check = true;
		lbool ret = l_False;
		int finalRes = -1; //Initial state is unsat for solver
		string logResult;
		string strTestUS = "";

		try {
			setUsageHelp(
					"USAGE: %s [options] <input-file> <result-output-file>\n\n  where input may be either in plain or gzipped DIMACS.\n");
			// printf("This is MiniSat 2.0 beta\n");

#if defined(__linux__)
			fpu_control_t oldcw, newcw;
			_FPU_GETCW(oldcw); newcw = (oldcw & ~_FPU_EXTENDED) | _FPU_DOUBLE; _FPU_SETCW(newcw);
			printf("WARNING: for repeatability, setting FPU to use double precision\n");
#endif 

			// Extra options:
			//
			IntOption verb("MAIN", "verb",
					"Verbosity level (0=silent, 1=some, 2=more).", 1,
					IntRange(0, 2));
			IntOption cpu_lim("MAIN", "cpu-lim",
					"Limit on CPU time allowed in seconds.\n", INT32_MAX,
					IntRange(0, INT32_MAX));
			IntOption mem_lim("MAIN", "mem-lim",
					"Limit on memory usage in megabytes.\n", INT32_MAX,
					IntRange(0, INT32_MAX));

			parseOptions(argc, argv, true);

			Solver S;
			//double initial_time = cpuTime();

			S.verbosity = verb;

			solver = &S;
			// Use signal handlers that forcibly quit until the solver will be able to respond to
			// interrupts:
			signal(SIGINT, SIGINT_exit);
			signal(SIGXCPU, SIGINT_exit);

			// Set limit on CPU-time:

			if (cpu_lim != INT32_MAX) {
				rlimit rl;
				getrlimit(RLIMIT_CPU, &rl);
				if (rl.rlim_max == RLIM_INFINITY
						|| (rlim_t) cpu_lim < rl.rlim_max) {
					rl.rlim_cur = cpu_lim;
					if (setrlimit(RLIMIT_CPU, &rl) == -1)
						printf(
								"WARNING! Could not set resource limit: CPU-time.\n");
				}
			}

			// Set limit on virtual memory:
			if (mem_lim != INT32_MAX) {
				rlim_t new_mem_lim = (rlim_t) mem_lim * 1024 * 1024;
				rlimit rl;
				getrlimit(RLIMIT_AS, &rl);
				if (rl.rlim_max == RLIM_INFINITY || new_mem_lim < rl.rlim_max) {
					rl.rlim_cur = new_mem_lim;
					if (setrlimit(RLIMIT_AS, &rl) == -1)
						printf(
								"WARNING! Could not set resource limit: Virtual memory.\n");
				}
			}

			if (argc == 1)
				printf(
						"Reading from standard input... Use '--help' for help.\n");

			gzFile in = (argc == 1) ? gzdopen(0, "rb") : gzopen(inFile, "rb");
			if (in == NULL)
				printf("ERROR! Could not open file: %s\n",
						argc == 1 ? "<stdin>" : inFile), exit(1);
			/*
			 if (S.verbosity > 0){
			 printf("\n============================[ Problem Statistics ]=============================\n");
			 printf("|                                                                             |\n"); }
			 */

			parse_DIMACS(in, S);
			gzclose(in);

			/*
			 if (S.verbosity > 0){
			 printf("|  Number of variables:  %12d                                         |\n", S.nVars());
			 printf("|  Number of clauses:    %12d                                         |\n", S.nClauses()); }
			 
			 double parsed_time = cpuTime();
			 if (S.verbosity > 0){
			 printf("|  Parse time:           %12.2f s                                       |\n", parsed_time - initial_time);
			 printf("|                                                                             |\n"); }
			 */

			// Change to signal-handlers that will only notify the solver and allow it to terminate
			// voluntarily:
			signal(SIGINT, SIGINT_interrupt);
			signal(SIGXCPU, SIGINT_interrupt);

			if (!S.simplify()) {
				if (res != NULL) {
					if (finalRes == 0)
						fprintf(res, "UNKNOWN\n");
					else
						fprintf(res, "UNSAT\n");
					fclose(res);
				}
				check = false;
				if (S.verbosity > 0) {
					printf(
							"===============================================================================\n");
					printf("Solved by unit propagation\n");
					printStats(S);
					printf("\n");
				}
				printf("UNSATISFIABLE\n");
				exit(20);
			}

			vec < Lit > dummy;
			//int nLearn=0;
			lbool ret = l_False;
			string str_dIntv = "";
			string bump_vars = "";
			string tmp = "";
			CAMLlocal1 (theoCheck);
			caml_register_global_root(&theoCheck);
			//placed while here

			cout << "\nStart searching, ";
			cout << "please wait....\n";
			while (check) {
				miniSATCalls++;
				if (maxClauses < S.nClauses())
					maxClauses = S.nClauses();
				double miniSATStart = cpuTime();
				S.model.clear(true);
				ret = S.solveLimited(dummy);
				if (ret != l_True) {
					check = false;
				}

				miniSATTime += cpuTime() - miniSATStart;

				if (ret == l_True) {
					sprintf(sSAT, "%s", "");
					for (int i = 0; i < S.nVars(); i++) {
						if (S.model[i] != l_Undef) {
							int c = (S.model[i] == l_True) ? i + 1 : -(i + 1);
							if (c > 0)
								sprintf(sSAT, "%s%s%d", sSAT,
										(i == 0) ? "" : " ", c);
						}
					}
//					cout << "clauses: " << S.nClauses() << endl;
					//theoCheck = caml_doTest (sIntv, sAss, sSAT, ia);

					char *c_dIntv = new char[str_dIntv.size() + 1];
					strcpy(c_dIntv, str_dIntv.c_str());

					char *c_strTestUS = ""; //sua ngay 10/01/2014

					if (strTestUS == "") {
						strcpy(c_strTestUS, "");
					} else {
						c_strTestUS = new char[strTestUS.size() + 1];
						strcpy(c_strTestUS, strTestUS.c_str());
					}

					/* 
					 cout <<endl<<"sIntv: "<<sIntv<<endl;
					 cout <<endl<<"c_dIntv: "<<c_dIntv<<endl;
					 cout <<endl<<"sAss: "<<sAss<<endl;
					 cout <<endl<<"sSAT: "<<sSAT<<endl;
					 cout <<endl<<"ia: "<<ia<<endl;
					 cout <<endl<<"before theoCheck: "<<endl;																																					
					 */
					/*
					 cout << endl << "sIntv:" << sIntv << endl;
					 cout << endl << "c_dIntv:" << c_dIntv << endl;
					 cout << endl << "sAss:" << sAss << endl;
					 cout << endl << "sSAT:" << sSAT << endl;
					 cout << endl << "ia:" << ia << endl;
					 cout << endl << "esl:" << esl << endl;
					 cout << endl << "c_strTestUS: " << c_strTestUS << endl;
					 */
//					cout << endl << "sSAT:" << sSAT << endl;
					double startCheck = cpuTime();
//					cout << "START SEARCH:\n";
					theoCheck = caml_dynTest(sIntv, c_dIntv, sAss, sSAT, ia,
							esl, c_strTestUS, iaTime, testingTime, usTime,
							parsingTime, decompositionTime,
							timeout - (cpuTime() - initial_time));
					dummy.clear();
//					cout << "Searched \n\n";
					ocamlTime += cpuTime() - startCheck;
//					cout << "Check: " << cpuTime() - startCheck << endl;
					int sat = Int_val(Field(theoCheck, 0));
					logResult = String_val(Field(theoCheck, 3));

					strTestUS = String_val(Field(theoCheck, 5));
					//cout <<endl<<"strTestUS: "<<strTestUS<<endl;

					//get list of variables for bumping
					bump_vars = String_val(Field(theoCheck, 6));

					tmp = String_val(Field(theoCheck, 7));

//					cout << "sat=" << sat;
//					cout << endl << "logResult: " << logResult << endl;

					//dynamic interval decomposition
					str_dIntv = String_val(Field(theoCheck, 4));
//					cout << endl << "str_dIntv: " << str_dIntv << endl;

					//cout <<endl<<"checkVarID: "<<tmp<<endl;
					//char c = getchar();

					// get the executed time of IA operations:
					iaTime = Double_val(Field(theoCheck, 9));
//					cout << "IA time: " << iaTime << endl;

					// get the testing time:
					testingTime = Double_val(Field(theoCheck, 10));

					// get the time of unsat core operation
					usTime = Double_val(Field(theoCheck, 11));

					parsingTime = Double_val(Field(theoCheck, 12));

					decompositionTime = Double_val(Field(theoCheck, 13));

					//check timeout occurs
					if ((cpuTime() - initial_time) >= timeout) {
						check = false;
						finalRes = -2;
					} else if (sat == 1) {
						// get the assignments of testing.
						char* assignments = String_val(Field(theoCheck, 1));
						// get the tested expresions.
						char* testingClauses = String_val(Field(theoCheck, 2));
						// get the clauses which are SAT by IA and the variables ranges.
						char* satClausesAndVarsIntervals = String_val(
								Field(theoCheck, 8));
						if (checkSAT(testingClauses, assignments)) {
							check = false;
							finalRes = sat;
						} else {
							cout
									<< "iRRAM detects round off error:.....................\n";
							cout << logResult << endl;
							cout
									<< "............................................. end iRRAM\n";
						}
					} else if (sat == 0) { //adding unknown reason to solver
						finalRes = sat;
						string cl_uk = String_val(Field(theoCheck, 2));
//						cout << endl << "Add unknown clause:" << cl_uk << endl;
						unknownLearnedClauses += solver_addClause(S, cl_uk);
					} else if (sat == -2) { //adding decomposition clauses to solver
						string cl_uk = String_val(Field(theoCheck, 2));
//						cout << endl << "Add decomposition clauses:" << cl_uk
//								<< "|" << endl;
						nDecompositions += solver_addClause(S, cl_uk);
//						cout << endl << "Bumping activity of variables: "
//								<< bump_vars << endl;
						solver_bumping(S, bump_vars);
//						addLitListToVector(dummy, bump_vars);
					} else { // adding unsat reason to solver
							 //finalRes = finalRes*sat;		  
						string cl_us = String_val(Field(theoCheck, 1));
//						cout << endl << "Add unsat clause:" << cl_us << "|"
//						<< endl;
						UNSATLearnedClauses += solver_addClause(S, cl_us);
					}

				} else if (ret == l_False) {
//					cout << "All Searched" << endl;
					check = false;
				} else {
					check = false;
					fprintf(res, "INDET\n");
					fclose(res);
				}
			}							 // End while loop

			caml_remove_global_root(&theoCheck);

			//================================================================================================
			// Screen information
			if (!check) {
				// file for output compact result
				ofstream final_result;
				final_result.open(getFileNameWithoutExt(argv[1]));
				// For theory result
				double totalTime = cpuTime() - initial_time;
				char *sta = new char[1024];
				sprintf(typeIA, "%s",
						(ia == 0) ? "CI" : (ia == 1) ? "AF1" :
						(ia == 2) ? "AF2" : (ia == 3) ? "CAI1" :
						(ia == 4) ? "CAI2" : (ia == 5) ? "CAI3" : "CI");
				printf(
						"\n===========================[ Problem Statistic ]===================================\n");

				final_result << argv[1] << ","; //output problem name to the final compact result file:
				sprintf(sta, "\nInput problem         : %s ", argv[1]);
				final_result << nVars << ","; // output the number of variables to final compact result.
				sprintf(sta, "%s\nNumber of variables   : %d ", sta, nVars);
				final_result << nCons << ","; // output the number of apis to final compact result.
				sprintf(sta, "%s\nNumber of constraints : %d ", sta, nCons);
				sprintf(sta, "%s\nInterval Arithmetic   : %s", sta, typeIA);
				sprintf(sta, "%s\nUnit searching box    : %g", sta, esl);
				sprintf(sta, "%s\nTimeout setting       : %g seconds\n", sta,
						timeout);

				final_result << totalTime << ","; // output the total running time to final compact result.
				sprintf(sta, "%s\nTotal running time    : %g seconds\n", sta,
						totalTime);

				final_result << iaTime << ","; // output the total time of IA operations
				sprintf(sta, "%s\nIA time               : %g seconds\n", sta,
						iaTime);

				final_result << testingTime << ","; // output the total time of testing operations
				sprintf(sta, "%s\nTesting time          : %g seconds\n", sta,
						testingTime);

				final_result << usTime << ","; // output the total time of unsat core computations
				sprintf(sta, "%s\nUNSAT Core time       : %g seconds\n", sta,
						usTime);

				final_result << parsingTime << ",";
				sprintf(sta, "%s\nParsing time          : %g seconds\n", sta,
						parsingTime);

				final_result << decompositionTime << ",";
				sprintf(sta, "%s\nDecomposition time    : %g seconds\n", sta,
						decompositionTime);

				sprintf(sta, "%s\nOcaml time            : %g seconds\n", sta,
						ocamlTime);

				final_result << miniSATTime << ",";
				sprintf(sta, "%s\nMiniSAT time          : %g seconds\n", sta,
						miniSATTime);

				final_result << miniSATVars << ",";
				sprintf(sta, "%s\nMiniSAT vars          : %g\n", sta,
						miniSATVars);

				final_result << maxClauses << ",";
				sprintf(sta, "%s\nMiniSAT max clauses   : %g\n", sta,
						maxClauses);

				final_result << miniSATCalls << ",";
				sprintf(sta, "%s\nMiniSAT calls         : %g\n", sta,
						miniSATCalls);

				final_result << clauses << ",";
				sprintf(sta, "%s\nraSAT clauses         : %g\n", sta, clauses);

				final_result << nDecompositions << ",";
				sprintf(sta, "%s\nDecomposed clauses    : %g\n", sta,
						nDecompositions);

				final_result << UNSATLearnedClauses << ",";
				sprintf(sta, "%s\nUNSAT learned clauses : %g\n", sta,
						UNSATLearnedClauses);

				final_result << unknownLearnedClauses << ",";
				sprintf(sta, "%s\nUNKOWN learned clauses: %g\n", sta,
						unknownLearnedClauses);

				//cout<<sta;
				if (finalRes == -2) {
					//cout <<"\nTIMEOUT";
					final_result << "Timeout\n"; // output the result to final compact result.
					sprintf(sta, "%sResult                : Timeout\n\n", sta);
					cout << sta;
					fprintf(res, sta);
					fclose(res);
				}

				else if (finalRes == 0) {
					//cout <<"\nUNKNOWN";
					final_result << "UNKNOWN\n"; // output the result to final compact result.
					sprintf(sta, "%sResult                : UNKNOWN\n\n", sta);
					cout << sta;
					fprintf(res, sta);
					fclose(res);
				} else if (finalRes == -1) {
					//cout <<"\nUNSAT";
					final_result << "UNSAT\n"; // output the result to final compact result.
					sprintf(sta,
							"%sResult                : UNSAT			(in the searching bound [%g, %g])\n\n",
							sta, lb, ub);
					cout << sta;
					fprintf(res, sta);
					fclose(res);
				} else if (finalRes == 1) {
					//cout <<"\nSAT\n";	    
					final_result << "SAT\n"; // output the result to final compact result.
					sprintf(sta, "%sResult                : SAT\n\n", sta);
					cout << sta;
					char * logContent = new char[logResult.size() + 1];
					strcpy(logContent, logResult.c_str());
					cout
							<< "=================================[ SAT instances ]================================="
							<< endl;
					cout << endl << logContent;
					fprintf(res, sta);
					fprintf(res, logContent);
					fclose(res);
				}
				cout << endl;
				final_result.close();
			}
			// End screen information

#ifdef NDEBUG
			exit(ret == l_True ? 10 : ret == l_False ? 20 : 0); // (faster than "return", which will invoke the destructor for 'Solver')
#else
			return (ret == l_True ? 10 : ret == l_False ? 20 : 0);
#endif
		} catch (OutOfMemoryException&) {
			printf(
					"===============================================================================\n");
			printf("INDETERMINATE\n");
			exit(0);
		}
	} //End combination of SAT and IA
}
