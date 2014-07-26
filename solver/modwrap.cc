#include <stdio.h>
#include <string.h>
#include "modwrap.h"

extern "C" {
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/fail.h>
}

value caml_genSmtForm(char * sIntv, char *sAssert, string ub) {
	CAMLparam0();
	CAMLlocalN(ml_args, 3);
	ml_args[0] = caml_copy_string(sIntv);
	ml_args[1] = caml_copy_string(sAssert);
	ml_args[2] = caml_copy_string(ub.c_str());

	static value * caml_gen_closure = NULL;
	if (caml_gen_closure == NULL)
		caml_gen_closure = caml_named_value("caml_genSmtForm");
	//return strdup(String_val(caml_callbackN(*caml_gen_closure, 4, ml_args)));
	CAMLreturn(caml_callbackN(*caml_gen_closure, 3, ml_args));
}

value caml_genSatForm(char* sAss, char * sIntv, double esl) {
	CAMLparam0();
	CAMLlocalN(ml_args, 3);
	ml_args[0] = caml_copy_string(sAss);
	ml_args[1] = caml_copy_string(sIntv);
	ml_args[2] = caml_copy_double(esl);

	static value * caml_gen_closure = NULL;
	if (caml_gen_closure == NULL)
		caml_gen_closure = caml_named_value("caml_genSatForm");
	CAMLreturn(caml_callbackN(*caml_gen_closure, 3, ml_args));
	/* We copy the C string returned by String_val to the C heap
	 so that it remains valid after garbage collection. */
}

value caml_isTheoConsis(char *sIntv, char *sCons, char *sCheck, int ia) {
	CAMLparam0();
	CAMLlocalN(ml_args, 4);
	//CAMLlocal1 (result);
	ml_args[0] = caml_copy_string(sIntv);
	ml_args[1] = caml_copy_string(sCons);
	ml_args[2] = caml_copy_string(sCheck);
	ml_args[3] = Val_int(ia);

	static value *caml_is_closure = NULL;
	if (caml_is_closure == NULL)
		caml_is_closure = caml_named_value("caml_isTheoConsis");
	//result = caml_alloc (2, 0);
	//result = caml_callbackN(*caml_is_closure, 4, ml_args);
	//CAMLreturn (result);
	CAMLreturn(caml_callbackN(*caml_is_closure, 4, ml_args));
}

char* caml_logResult(char * sIntv, char *sCons, char *sCheck, int ia) {
	//CAMLparam0 ();
	CAMLlocalN(ml_args, 4);
	ml_args[0] = caml_copy_string(sIntv);
	ml_args[1] = caml_copy_string(sCons);
	ml_args[2] = caml_copy_string(sCheck);
	ml_args[3] = Val_int(ia);

	static value * caml_gen_closure = NULL;
	if (caml_gen_closure == NULL)
		caml_gen_closure = caml_named_value("caml_logResult");
	return strdup(String_val(caml_callbackN(*caml_gen_closure, 4, ml_args)));
	//CAMLreturn(caml_callbackN(*caml_gen_closure, 4, ml_args));
}

value caml_doTest(char *sIntv, char *sCons, char *sCheck, int ia) {
	CAMLparam0();
	CAMLlocalN(ml_args, 4);
	//CAMLlocal1 (result);
	ml_args[0] = caml_copy_string(sIntv);
	ml_args[1] = caml_copy_string(sCons);
	ml_args[2] = caml_copy_string(sCheck);
	ml_args[3] = Val_int(ia);

	static value *caml_is_closure = NULL;
	if (caml_is_closure == NULL)
		caml_is_closure = caml_named_value("caml_doTest");
	//result = caml_alloc (2, 0);
	//result = caml_callbackN(*caml_is_closure, 4, ml_args);
	//CAMLreturn (result);
	CAMLreturn(caml_callbackN(*caml_is_closure, 4, ml_args));
}

//including dynamic interval decomposition and testing
value caml_dynTest(value *intvInfo, value *miniSATCodesConstraintsMap, int nCons, char *sCheck, int ia,
		double esl, char *sTestUS, double iaTime, double testingTime,
		double USTime, double parsingTime, double decompositionTime,
		double remainingTime) {
//	cout << "dynTest\n";
	CAMLparam0();
	//cout << "dynTest1\n";
	int const ARGS_NUM = 13;
	int index = 0;
	//cout << "dynTest2\n";
	CAMLlocalN(ml_args, ARGS_NUM);
	//cout << "dynTest3\n";
	CAMLlocal1 (result);
	//cout << "dynTest4\n";
	ml_args[index++] = *intvInfo;
	//cout << "dynTest5\n";
	//cout << "dynTest6\n";
	ml_args[index++] = *miniSATCodesConstraintsMap;
	//cout << "dynTest7\n";
	ml_args[index++] = Val_int(nCons);
	ml_args[index++] = caml_copy_string(sCheck);
	//cout << "dynTest8\n";
	ml_args[index++] = Val_int(ia);
	//cout << "dynTest9\n";
	ml_args[index++] = caml_copy_double(esl);
	//cout << "dynTest10\n";
	ml_args[index++] = caml_copy_string(sTestUS);
	//cout << "dynTest11\n";
	ml_args[index++] = caml_copy_double(iaTime);
	//cout << "dynTest12\n";
	ml_args[index++] = caml_copy_double(testingTime);
	//cout << "dynTest13\n";
	ml_args[index++] = caml_copy_double(USTime);
	//cout << "dynTest14\n";
	ml_args[index++] = caml_copy_double(parsingTime);
	//cout << "dynTest15\n";
	ml_args[index++] = caml_copy_double(decompositionTime);
	//cout << "dynTest16\n";
	ml_args[index++] = caml_copy_double(remainingTime);
//	cout << "dynTest17\n";

	static value *caml_is_closure = NULL;
	//cout << "dynTest18\n";
	if (caml_is_closure == NULL)
		caml_is_closure = caml_named_value("caml_dynTest");
//	cout << "dynTest19\n";
	//result = caml_alloc (2, 0);
	//result = caml_callbackN(*caml_is_closure, 4, ml_args);
	result = caml_callbackN_exn(*caml_is_closure, ARGS_NUM, ml_args);
//	cout << "dynTest20\n";
	if (Is_exception_result(result)) {
		printf("Catched ocaml exception!\n");
		printf("%s", String_val(Field(result, 0)));
		printf("\n");
		caml_raise(Extract_exception(result));
	}
	//cout << "dynTest21\n";
	CAMLreturn(result);
}

int caml_InfCheck(char * sIntv, char* sAss) {
	CAMLparam0();
	static value * caml_gen_closure = NULL;
	if (caml_gen_closure == NULL)
		caml_gen_closure = caml_named_value("caml_InfCheck");
	return Int_val(
			caml_callback2(*caml_gen_closure, caml_copy_string(sIntv),
					caml_copy_string(sAss)));
}

