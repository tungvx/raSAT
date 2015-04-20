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
#include <set>

#include "utils/System.h"
#include "utils/ParseUtils.h"
#include "utils/Options.h"
#include "solver/Dimacs.h"
#include "solver/Solver.h"
#include "exactRealArithmetics.h"

#include "iRRAM/lib.h"

//Caml interaction
#include "modwrap.h"
#include "file.h"

extern "C" {
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
}

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
	//cout << bump_vars << "\n";
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
		int var = abs(a[i]) - 1;
		solv.varBumpActivity(var);
	}
}

// add a list of lit into a vector:
void addLitListToVector(vec<Lit> &litVector, string litList) {
	//cout << "Start\n";
	string s = remove_space(litList);
	//cout << "End Remove space\n";
	int l = s.length();
	int size = 0;
	for (int i = 0; i < l; i++)
		if (s[i] == ' ')
			size++;
	int *a = new int[size + 1];
	int pos = 0, prev = 0;
	//cout << "StartAtoi\n";
	for (int i = 0; i < l; i++) {
		if (s[i] == ' ') {
			a[pos] = atoi(s.substr(prev, i - prev).c_str());
			prev = i;
			pos++;
		}
	}
	//cout << "End Atoi\n";
	//cout << atoi(s.substr(prev, l - prev).c_str()) << " converted\n";
	a[pos] = atoi(s.substr(prev, l - prev).c_str());
	//cout << litVector.size() << "\n";
	//cout << "assumption: ";
	for (int i = 0; i <= pos; i++) {
		int var = abs(a[i]) - 1;
		//cout << var << " ";
		litVector.push((a[i] > 0) ? mkLit(var) : ~mkLit(var));
	}
	delete[] a;
	//cout << "\n";
}

double miniSATVars = 0;
double clauses = 0;
// add some clauses to solver which stores information in reason
int solver_addClause(Solver& solv, string reason) {
	//solv.model.clear(true);
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
				//printf ("adding %d\n", var);	
				while (var >= solv.nVars())
					solv.newVar();
				if (miniSATVars < var + 1)
					miniSATVars = var + 1;
				bool sign = (a[i] > 0) ? false : true;
				cl.push(mkLit(var, sign));
			}
			solv.addClause_(cl);
			clauses++;
			addedClauses++;
			prevPos = endPos + 1;
		}
		endPos++;
	}
	delete[] a;
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
extern char* clausesList;
extern char* assignments; 
extern bool checkingResult;

int main(int argc, char* argv[]) {
  iRRAM_initialize(argc,argv);
  //iRRAM::iRRAM_exec(checkSAT,"");
  //testIRRAM(0);
  //iRRAM::iRRAM_exec(testIRRAM, 0);
  //return 0;
	////cout << "Run" << endl;
	bool debug = true;
	double initial_time = cpuTime();
	/* Initialize Caml code */
	//caml_main(argv);
	caml_startup(argv); //for bytecode compilation in ocaml

	//Preprocess for parameter and generate MiniSat form here
	if (argc < 3) {
		if (debug)
			cout << "Missing arguments...";
		return 0;
	}

	//generate ebg format form from SMT2 format (raSAT input form)
	////cout << "Run1" << endl;	
	double lb = stod(getLoBound(argv[2]));
	double ub = stod(getUpBound(argv[2]));
	//cout << "Lower Bound: " << lb << endl;
	//cout << "Upper Bound: " << ub << endl;

	//cout << "Run2" << endl;	
	char *smtfile = argv[1];
	//cout << "SMT file: " << smtfile << endl;
	int nvar = num_var(smtfile);
	//cout << "number of variables: "<<nvar<<endl;
  string logic = get_logic(smtfile);
  //cout << "Logic:" << logic << "t" << endl;

//	cout << "Run6" << endl;

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
	int maxVarsNum = 0;
	int isEquation = 0;
	int isNotEquation = 0;
	//char *sta = new char[2048];
	//char *typeIA = new char[5];

	//interval arithmetic type
	//Jan 14, 2014: force ia = "af2"
	int ia = 2;

	//bound for dynamic interval decomposition
//	cout << "start getting esl" << endl;
	double esl = 0.1;	//default searching box is 0.1
	if (argc >= 4)
		//esl = atof(argv[3]);
		esl = getSearchingBox(argv[3]);
//	cout <<"esl = "<<esl<<endl;
  if (logic == "QF_NIA")
    esl = ceil(esl);
	//get the timeout, Jan 14, 2014
	double timeout = 60.0; //default timeout = 60 seconds
	if (argc >= 5)
		timeout = getTimeout(argv[4]);

	//cout <<endl <<"time out" <<timeout<<endl;

	//get information for polynomial constraints: number of variables, constraints

  double parsingStart = cpuTime();
	//ebg -> form of minisat in string 
	CAMLlocal3(satInfo, intvInfo, miniSATCodesConstraintsMap);
	caml_register_global_root (&satInfo);
	caml_register_global_root (&intvInfo);
	caml_register_generational_global_root (&miniSATCodesConstraintsMap);
//	cout << sIntv << endl;
  //cout << "smtfile: " << smtfile << endl;
  //cout << "lb: " << lb << endl;
  //cout << "ub: " << ub << endl;
  //cout << "logic: " << logic.c_str() << endl;
	satInfo = caml_genSatForm(smtfile, lb, ub, logic.c_str());
	//return 0;
	//cout << "finish genSATForm" << endl;
	int nVars = Int_val(Field(satInfo, 0)); //nVars store the number variables for SAT content
	string satContent = String_val(Field(satInfo, 1));
	intvInfo = Field(satInfo, 2);
	miniSATCodesConstraintsMap = Field(satInfo, 3);
	int nCons = Int_val(Field(satInfo, 4));
	maxVarsNum = Int_val(Field(satInfo, 5));
	isEquation = Int_val(Field(satInfo, 6));
	isNotEquation = Int_val(Field(satInfo, 7));
	//cout << "maxVarsNum: " << maxVarsNum << endl;
//	cout << satContent << endl;
//	cout << "IsEquation: " << isEquation << endl;
//	cout << "isNotEquation: " << isNotEquation << endl;
	caml_remove_global_root(&satInfo);

  parsingTime = cpuTime() - parsingStart;
//	cout << "Run8" << endl;
	string sfile = toFilein(argv[1]);
	char * inFile = new char[sfile.size() + 1];
	strcpy(inFile, sfile.c_str());

	if (writeFile(inFile, satContent) != 1) {
		if (debug)
			cout << "Can not create " << inFile << " file!";
		return 0;
	}

	//cout << "Run9" << endl;
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
			//cout << "Run10" << endl;
			setUsageHelp(
					"USAGE: %s [options] <input-file> <result-output-file>\n\n  where input may be either in plain or gzipped DIMACS.\n");
			// printf("This is MiniSat 2.0 beta\n");

#if defined(__linux__)
			//cout << "Run11" << endl;
			fpu_control_t oldcw, newcw;
			//cout << "Run12" << endl;
			_FPU_GETCW(oldcw); newcw = (oldcw & ~_FPU_EXTENDED) | _FPU_DOUBLE; _FPU_SETCW(newcw);
			//cout << "Run13" << endl;
			if (debug) printf("WARNING: for repeatability, setting FPU to use double precision\n");
#endif 

			// Extra options:
			//
			//cout << "Run14" << endl;	
			IntOption verb("MAIN", "verb",
					"Verbosity level (0=silent, 1=some, 2=more).", 1,
					IntRange(0, 2));
			//cout << "Run15" << endl;
			IntOption cpu_lim("MAIN", "cpu-lim",
					"Limit on CPU time allowed in seconds.\n", INT32_MAX,
					IntRange(0, INT32_MAX));
			//cout << "Run16" << endl;
			IntOption mem_lim("MAIN", "mem-lim",
					"Limit on memory usage in megabytes.\n", INT32_MAX,
					IntRange(0, INT32_MAX));

			//cout << "Run17" << endl;
			parseOptions(argc, argv, true);

			//cout << "Run18" << endl;
			Solver S;
			//double initial_time = cpuTime();

			S.verbosity = verb;

			solver = &S;
			// Use signal handlers that forcibly quit until the solver will be able to respond to
			// interrupts:
			//cout << "Run19" << endl;
			signal(SIGINT, SIGINT_exit);
			//cout << "Run20" << endl;	
			signal(SIGXCPU, SIGINT_exit);

			// Set limit on CPU-time:
			//cout << "Run21" << endl;
			if (cpu_lim != INT32_MAX) {
				rlimit rl;
				getrlimit(RLIMIT_CPU, &rl);
				if (rl.rlim_max == RLIM_INFINITY
						|| (rlim_t) cpu_lim < rl.rlim_max) {
					rl.rlim_cur = cpu_lim;
					if (setrlimit(RLIMIT_CPU, &rl) == -1)
						if (debug)
							printf(
									"WARNING! Could not set resource limit: CPU-time.\n");
				}
			}

			// Set limit on virtual memory:
			//cout << "Run22" << endl;	
			if (mem_lim != INT32_MAX) {
				rlim_t new_mem_lim = (rlim_t) mem_lim * 1024 * 1024;
				rlimit rl;
				getrlimit(RLIMIT_AS, &rl);
				if (rl.rlim_max == RLIM_INFINITY || new_mem_lim < rl.rlim_max) {
					rl.rlim_cur = new_mem_lim;
					if (setrlimit(RLIMIT_AS, &rl) == -1)
						if (debug)
							printf(
									"WARNING! Could not set resource limit: Virtual memory.\n");
				}
			}

			//cout << "Run23" << endl;
			if (argc == 1)
				if (debug)
					printf(
							"Reading from standard input... Use '--help' for help.\n");
			//cout << "Run24" << endl;
			gzFile in = (argc == 1) ? gzdopen(0, "rb") : gzopen(inFile, "rb");
			//cout << "Run25" << endl;
			if (in == NULL)
				if (debug)
					printf("ERROR! Could not open file: %s\n",
							argc == 1 ? "<stdin>" : inFile), exit(1);
			/*
			 if (S.verbosity > 0){
			 printf("\n============================[ Problem Statistics ]=============================\n");
			 printf("|                                                                             |\n"); }
			 */
			//cout << "Run26" << endl;
			parse_DIMACS(in, S);
			//cout << "Run27" << endl;
			gzclose(in);
			delete[] inFile;
			//cout << "Run28" << endl;

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
			//cout << "Run29" << endl;
			signal(SIGINT, SIGINT_interrupt);
			//cout << "Run30" << endl;
			signal(SIGXCPU, SIGINT_interrupt);

			//cout << "Run30" << endl;
			if (!S.simplify()) {
				//cout << "Run31" << endl;
				//if (res != NULL) {
				//	if (finalRes == 0)
				//		fprintf(res, "UNKNOWN\n");
				//	else
				///		fprintf(res, "UNSAT\n");
				//	fclose(res);
				//}
				//cout << "Run32" << endl;
				check = false;
				if (S.verbosity > 0 && debug) {
					//cout << "Run33" << endl;	
					printf(
							"===============================================================================\n");
					printf("Solved by unit propagation\n");
					printStats(S);
					//cout << "Run34" << endl;



					printf("\n");

				}
				if (debug)
					printf("UNSATISFIABLE\n");
				exit(20);
			}

			//cout << "Run35" << endl;
			vec < Lit > dummy;
			//int nLearn=0;
			lbool ret = l_False;
			string str_dIntv = "";
			string bump_vars = "";
			string tmp = "";
			//cout << "Run36" << endl;	
			CAMLlocal1 (theoCheck);
			//cout << "Run37" << endl;
			caml_register_global_root(&theoCheck);
			//cout << "Run38" << endl;

			if (debug)
				cout << "\nStart searching, ";
			if (debug)
				cout << "please wait....\n";
			//cout << "Run39" << endl;
			while (check) {
			  //if (!isEquation) break;
				//cout << "Run40" << endl;	
				miniSATCalls++;
				if (maxClauses < S.nClauses())
					maxClauses = S.nClauses();
				double miniSATStart = cpuTime();
//				cout << "Run41" << endl;
				S.model.clear(true);
//				cout << "Run42" << endl;
				ret = S.solveLimited(dummy);
//				cout << "Run43" << endl;	
				if (ret != l_True) {
					check = false;
				}
        
				miniSATTime += cpuTime() - miniSATStart;

				if (ret == l_True) {
//					cout << "Run44" << endl;
					int varsNum = S.nVars();
//					cout << "varsNum: " << varsNum << endl;
//					cout << "Model size: " << S.model.size() << endl;
//					cout << "start init sSAT of size: " << 2 * varsNum + 1 << endl;
					string sSAT;
//					cout << "Run45" << endl;
					bool firstSolution = true;
					for (int i = 0; i < S.nVars(); i++) {
						if (S.model[i] == l_True) {
							char numstr[21];
							sprintf(numstr, "%s%d", firstSolution ? "" : " ", i+1);
							sSAT.append(numstr);
//							ostringstream ss;
//							ss << i+1;
//							if (!firstSolution) sSAT.append(" ");
//							sSAT.append(ss.str());
							firstSolution = false;
						}
					}
//					cout << "Run46" << endl;
//					cout << "clauses: " << S.nClauses() << endl;
					//theoCheck = caml_doTest (sIntv, sAss, sSAT, ia);
					//cout << "dyndecom size: " << str_dIntv.size() << endl;
					char *c_dIntv = new char[str_dIntv.size() + 1];
					strcpy(c_dIntv, str_dIntv.c_str());

					char *c_strTestUS = ""; //sua ngay 10/01/2014
					//cout << "Run47" << endl;
					bool needDeleted = false;
					if (strTestUS == "") {
						strcpy(c_strTestUS, "");
					} else {
						needDeleted = true;
						c_strTestUS = new char[strTestUS.size() + 1];
						strcpy(c_strTestUS, strTestUS.c_str());
					}
					//cout << "Run48" << endl;

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
					//cout << "Run49" << endl;
					theoCheck = caml_dynTest(&intvInfo,
							&miniSATCodesConstraintsMap, nCons, sSAT.c_str(),
							ia, esl, c_strTestUS, iaTime, testingTime, usTime,
							parsingTime, decompositionTime,
							timeout - (cpuTime() - initial_time));
					delete[] c_dIntv;
					if (needDeleted)
						delete[] c_strTestUS;
					//cout << "Run50" << endl;
					dummy.clear(true);
//					cout << "Searched \n\n";
					ocamlTime += cpuTime() - startCheck;

//					cout << "Check: " << cpuTime() - startCheck << endl;
					//cout << "Run51" << endl;
					int sat = Int_val(Field(theoCheck, 0));
//					cout << sat << endl;
					logResult = String_val(Field(theoCheck, 3));
					//cout << "Run52" << endl;	

					strTestUS = String_val(Field(theoCheck, 5));
					//cout <<endl<<"strTestUS: "<<strTestUS<<endl;

					//get list of variables for bumping
					bump_vars = String_val(Field(theoCheck, 6));
//					cout << "bump: " << bump_vars << endl;

					tmp = String_val(Field(theoCheck, 7));

//					cout << "sat = " << sat << endl;
//					cout << endl << "logResult: " << logResult << endl;

					//dynamic interval decomposition
					intvInfo = Field(theoCheck, 4);
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
//					cout << "Run53" << endl;

					//check timeout occurs
					if ((cpuTime() - initial_time) >= timeout) {
						check = false;
						finalRes = -2;
					} else if (sat == 1) {
#ifdef iRRAM_INC
						// get the assignments of testing.
						assignments = String_val(Field(theoCheck, 1));
						// get the tested expresions.
						clausesList = String_val(Field(theoCheck, 2));
						// get the clauses which are SAT by IA and the variables ranges.
						char* satClausesAndVarsIntervals = String_val(
								Field(theoCheck, 8));
						iRRAM::iRRAM_exec(checkSAT, 0);
						if (checkingResult) {
							check = false;
							finalRes = sat;
						} else {
							if (debug) {
								cout
								<< "iRRAM detects round off error:.....................\n";
								cout << logResult << endl;
								cout
								<< "............................................. end iRRAM\n";
							}
						}
#else 
						check = false;
						finalRes = sat;
#endif
					} else if (sat == 0) { //adding unknown reason to solver
						finalRes = sat;
						string cl_uk = String_val(Field(theoCheck, 2));
						//cout << endl << "Add unknown clause:" << cl_uk << endl;
						unknownLearnedClauses += solver_addClause(S, cl_uk);
					} else if (sat == -2) { //adding decomposition clauses to solver
//						cout << "start adding clauses" << endl;
						string cl_uk = String_val(Field(theoCheck, 2));
					  //cout << endl << "Add decomposition clauses:" << cl_uk << "|" << endl;
						nDecompositions += solver_addClause(S, cl_uk);
						//cout << S.nVars() << endl;	
						//cout << "finish adding clauses" << endl;	
						//cout << endl << "Bumping activity of variables: "
						//		<< bump_vars << endl;
						//solver_bumping(S, bump_vars);
//						cout << "finish bump" << endl;
						//cout << S.nVars() << endl;
						if (!bump_vars.empty()) addLitListToVector(dummy, bump_vars);
					} else { // adding unsat reason to solver
							 //finalRes = finalRes*sat;		  
						string cl_us = String_val(Field(theoCheck, 1));
						//cout << endl << "Add unsat clause:" << cl_us << "|"	<< endl;
						UNSATLearnedClauses += solver_addClause(S, cl_us);
						//cout << "finish adding unsat clauses" << endl;
					}

				} else if (ret == l_False) {
//					cout << "All Searched" << endl;
					check = false;
				} else {
					check = false;
					fprintf(res, "INDET\n");
					fclose(res);
				}

				//break; // force the loop to be executed once only
			}							 // End while loop

			//cout << "finish while" << endl;

			caml_remove_global_root(&theoCheck);

			//================================================================================================
			// Screen information
			if (!check) {
				// file for output compact result
				ofstream final_result;
				final_result.open((string(smtfile) + ".tmp").c_str());

				// For theory result
				double totalTime = cpuTime() - initial_time;
				if (totalTime - iaTime - testingTime - usTime - parsingTime - decompositionTime - miniSATTime > 0)
				  miniSATTime += totalTime - iaTime - testingTime - usTime - parsingTime - decompositionTime - miniSATTime;
				char *sta = new char[1024];
				if (debug)
					printf(
							"\n===========================[ Problem Statistic ]===================================\n");

				final_result << argv[1] << ","; //output problem name to the final compact result file:
				sprintf(sta, "\nInput problem         : %s ", argv[1]);

				final_result << nVars << ","; // output the number of variables to final compact result.
				sprintf(sta, "%s\nNumber of variables   : %d ", sta, nVars);

				final_result << maxVarsNum << ","; // output the max number of variables in one api to final compact result.

				final_result << nCons << ","; // output the number of apis to final compact result.
				sprintf(sta, "%s\nNumber of constraints : %d ", sta, nCons);
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

				final_result << isEquation << ",";
				final_result << isNotEquation << ",";
				
//				cout<<sta;
				if (finalRes == -2) {
					//cout <<"\nTIMEOUT";
					final_result << "Timeout\n"; // output the result to final compact result.
					sprintf(sta, "%sResult                : Timeout\n\n", sta);
					if (debug)
						cout << sta;
					fprintf(res, sta);
					fclose(res);
				}

				else if (finalRes == 0) {
					//cout <<"\nUNKNOWN";
					//cout << "unknown";
					final_result << "unknown\n"; // output the result to final compact result.
					sprintf(sta, "%sResult                : UNKNOWN\n\n", sta);
					if (debug)
						cout << sta;
					fprintf(res, sta);
					fclose(res);
				} else if (finalRes == -1) {
					//cout <<"\nUNSAT";
					//cout << "unsat";
					final_result << "unsat\n"; // output the result to final compact result.
					sprintf(sta, "%sResult                : UNSAT			\n\n", sta);
					if (debug)
						cout << sta;
					fprintf(res, sta);
					fclose(res);
				} else if (finalRes == 1) {
					//cout <<"\nSAT\n";
					//cout << "sat";
					final_result << "sat\n"; // output the result to final compact result.
					sprintf(sta, "%sResult                : SAT\n\n", sta);
					if (debug)
						cout << sta;
					char * logContent = new char[logResult.size() + 1];
					strcpy(logContent, logResult.c_str());
					if (debug)
						cout
								<< "=================================[ SAT instances ]================================="
								<< endl;
					if (debug)
						cout << endl << logContent;
					fprintf(res, sta);
					fprintf(res, logContent);
					fclose(res);
				}
				if (debug)
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
			if (debug)
				printf(
						"===============================================================================\n");
			if (debug)
				printf("INDETERMINATE\n");
			exit(0);
		}
	} //End combination of SAT and IA
}
