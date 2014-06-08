extern "C" {
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
}
#include <iostream>
using namespace std;

value caml_genSmtForm(char * sIntv, char *sAssert, double lb, double ub);
value caml_genSatForm(char * sIntv, double esl);
value caml_isTheoConsis(char *sIntv, char *sCons, char *sCheck, int ia);
char* caml_logResult(char * sIntv, char *sCons, char *sCheck, int ia);
value caml_doTest(char *sIntv, char *sCons, char *sCheck, int ia);

/* =========================================================================================================================|
 * sIntv		: the string format of initial intervals. For example: "(ic (x_1 in -1000. 1000.) (x_0 in -1000. 1000.))"	|
 * dIntv		: the string format of decomposed intervals. 																|	
 * 					For example: "(ic (or (x_1 in 0. 1000.) (x_1 in -1000. 0.)) (or (x_0 in 0. 1000.) (x_0 in -1000. 0.)))"	|
 * sCons		: the string format of assertions. For example:																|
 *  				"(assert (and (> (* (* 1. x_0) x_1) 1.) (< (+ (* (* 1. x_1) x_1) (* (* 1. x_0) x_0)) 1.)))"				|
 * sCheck		: the string of selected minisat selected literals. For example: 											|
 * 					"1 2"																									|
 * ia			: type of Interval Arithmetics.																				|
 * esl			: epsilon the stoppable distance of decomposition.															|
 * sTestUS		: UNSAT tested clauses. For example: 																		|
 * 					"(< (+ (* (* 1. x_1) x_1) (* (* 1. x_0) x_0)) 1.)"														|
 * iaTime		: the time in which IA operations have been executed. This number is used for statistics. 			 		|
 * testingTime	: The current total time for testing.																		|
 * USTime		: the current total time for finding UnSAT core 															|
 * =========================================================================================================================|*/
value caml_dynTest(char *sIntv, char* dIntv, char *sCons, char *sCheck, int ia,
		double esl, char *sTestUS, double iaTime, double testingTime,
		double USTime, double parsingTime, double decompositionTime, double remainingTime);

int caml_getNumCons(char * sAss);
int caml_InfCheck(char * sIntv, char* sAss);
//double caml_divide(int a, int b);

