#include <iostream>
#include <sstream>
#include <fstream>
#include <cstring>
#include <string>
#include <stdio.h>
#include <cstdlib>

using namespace std;

const string keyVar = "declare-var";
const string keyAssert = "assert";

//Predefined type of Interval Arithmetic
const int CI = 0;              //Classical Interval
const int AF1 = 1;              //Affine Form of interval 1
const int AF2 = 2;              //Affine Form of interval 2
const int CAI1 = 3;              //Chebyshev Affine Form of interval 
const int CAI2 = 4;              //Extend Chebyshev Affine Form of interval 
const int CAI3 = 5;     //Otptimized Extended Chebyshev Affine Form of interval 

//remove space at the begining and ending of a string
string remove_space(string s) {
	size_t l = s.length();
	size_t i, j;
	for (i = 0; i < l; i++) {
		if (s.at(i) != ' ')
			break;
	}
	for (j = l - 1; j >= 0; j--) {
		if (s.at(j) != ' ')
			break;
	}
	return s.substr(i, j + 1 - i);
}

//get the first word in a string
string first_word(string s) {
	string str = remove_space(s);
	size_t i;
	for (i = 0; i < str.length(); i++) {
		if (str.at(i) == ' ')
			return str.substr(0, i);
	}
	return str;
}

//get the last word in a string
string last_word(string s) {
	string str = remove_space(s);
	size_t i;
	size_t l = str.length();
	for (i = l - 1; i >= 0; i--) {
		if (str.at(i) == ' ')
			return str.substr(i, l);
	}
	return str;
}

//get the variables by keyword "declare-var"
string get_var(string sIn) {
	string s = remove_space(sIn);
	string s1 = keyVar;
	string str = s.substr(s.find(s1) + s1.length(), s.length());
	return first_word(str);
}

//get low bound of interval variable
string get_lbound(string s) {
	string s1 = "in";
	string str = s.substr(s.find(s1) + s1.length(), s.length());
	return first_word(str);
}

//get upper bound of interval variable
string get_ubound(string s) {
	string s1 = get_lbound(s);
	size_t tmp = s.find(s1) + s1.length();
	string str = s.substr(tmp, s.length() - tmp);
	string s2 = first_word(str);

	//remove the last character ')'
	size_t pos = s2.rfind(')');
	return s2.substr(0, pos);
}

//generate interval constraints
string gen_intv(string s) {
	string var, lb, ub;
	var = get_var(s);
	lb = get_lbound(s);
	ub = get_ubound(s);
	string res = "(" + var + ' ' + lb + ' ' + ub + ")";
	return res;
}

//generate assertion constraints
string gen_constr(string s) {
	string s1 = remove_space(s);
	string s2 = keyAssert;
	size_t l = s1.length();
	size_t tmp = s1.find(s2) + s2.length();
	s1 = s1.substr(tmp, l - tmp);
	s1 = remove_space(s1);

	//remove the last character ')'
	size_t pos = s1.rfind(')');
	string res = s1.substr(0, pos);
	res = remove_space(res);
	return res;
}

// test.smt -> test
string getFileNameWithoutExt(char *filename) {
	string str = string(filename);
	int pos = str.find_last_of('.');
	return str.substr(0, pos);
}

//change SMT2 file to raSAT file (*.rs) (with raSAT input form)
string toFileRs(char * filename) {
	string str = string(filename);
	int pos = str.find_last_of('.');
	string sub = str.substr(0, pos);
	return sub + ".rs";
}

//change file name to file in of MiniSAT
string toFilein(char * filename) {
	string str = string(filename);
	int pos = str.find_last_of('.');
	string sub = str.substr(0, pos);
	return sub + ".in";
}

//change file name to file out of MiniSAT
string toFileout(char * filename) {
	string str = string(filename);
	int pos = str.find_last_of('.');
	string sub = str.substr(0, pos);
	return sub + ".out";
}

//change filename from SMT benchmark file to ebg file
string toFile_ebg(char * filename) {
	string str = string(filename);
	int pos = str.find_last_of('.');
	string sub = str.substr(0, pos);
	return sub + ".ebg";
}

//get interval constraints from ebg files
string getIntv(char *filename) {
	string line, intvCons = "";
	size_t pos;
	ifstream myfile(filename);

	if (myfile.is_open()) {
		while (myfile.good()) {
			getline(myfile, line);
			pos = line.find(keyVar);
			if (pos != string::npos) {
				if (intvCons != "")
					intvCons = "and " + intvCons + ' ';
				intvCons = intvCons + gen_intv(line);
			}
		}
		myfile.close();
	} else
		cout << "Unable to open " << filename << " file!";
	return intvCons;
}

//get assertion constraints from ebg files
string getAssert(char *filename) {
	string line, sCons = "";
	size_t pos;
	ifstream myfile(filename);

	if (myfile.is_open()) {
		while (myfile.good()) {
			getline(myfile, line);
			pos = line.find(keyAssert);
			if (pos != string::npos) {
				if (sCons != "")
					sCons = "and " + sCons + ' ';
				sCons = sCons + gen_constr(line);
			}
		}
		myfile.close();
	} else
		cout << "Unable to open " << filename << " file!";
	return sCons;
}

//write content to file
int writeFile(char *filename, string content) {
	ofstream myfile(filename);
	if (myfile.is_open()) {
		myfile << content;
		myfile.close();
	} else {
		cout << "Unable to create " << filename << " file!";
		return 0;
	}
	return 1;
}

//read content to from file
string readFile(char *filename) {
	string content = "", line;
	ifstream myfile(filename);
	if (myfile.is_open()) {
		while (!myfile.eof()) // To get you all the lines.
		{
			getline(myfile, line);
			content = (content == "") ? "" : content + "\n";
			content = content + line;
		}
		myfile.close();
	} else {
		cout << "Unable to open file";
		return "";
	}
	return content;
}

//update number of clause in MiniSat file
string incClause(string s) {
	string s1 = "cnf";
	size_t i, pos1 = 0, pos2 = 0;

	size_t pos = s.find(s1) + s1.length();

	//decide position of number clauses
	for (i = pos; i < s.length(); i++) {
		if (s.at(i) == ' ')
			pos1 = i;
		if (s.at(i) == '\n') {
			pos2 = i;
			break;
		}
	}

	//string number of clauses
	string num_of_clauses = s.substr(pos1 + 1, pos2 - pos1);

	int nClauses;
	stringstream convert(num_of_clauses); // stringstream used for the conversion initialized with the contents of Text

	if (!(convert >> nClauses))  // convert a string to int
		nClauses = 0;                // if convert fails set nClauses to 0

	nClauses += 1;

	ostringstream osstream;
	osstream << nClauses;
	string string_clause = osstream.str();

	string start = s.substr(0, pos1 + 1);
	string remain = s.substr(pos2, s.length() - pos2);
	//res.append (string_clause);
	string res = start + string_clause + remain;
	return res;
}

//increase clause and update new clause to filename
int updateClause(char *filename, string newClause) {
	string fileContent = readFile(filename);

	string change = incClause(fileContent);

	change = change + "\n" + newClause + "0";

	return writeFile(filename, change);
}

//parse argument to decide which type of Interval Arithmetic in (ci, af1, af2...)
int getIA(char *argIA) {
	string s = string(argIA);
	string s1 = "=";
	size_t pos = s.find(s1);
	string str = s.substr(pos + s1.length(), s.length() - pos - s1.length());
	str = remove_space(str);
	if ((str == "ci") || (str == "CI") || (str == "Ci"))
		return CI;
	else if ((str == "af1") || (str == "AF1") || (str == "Af1"))
		return AF1;
	else if ((str == "af2") || (str == "AF2") || (str == "Af2"))
		return AF2;
	else if ((str == "cai1") || (str == "CAI1") || (str == "Cai1"))
		return CAI1;
	else if ((str == "cai2") || (str == "CAI2") || (str == "Cai2"))
		return CAI2;
	else if ((str == "cai3") || (str == "CAI3") || (str == "Cai3"))
		return CAI3;
	else
		return CI;
}

//parse argument to get the minimum searching interval box, init of searching in an interval box
double getSearchingBox(char *sBox) {
	string s = string(sBox);
	string s1 = "=";
	size_t pos = s.find(s1);
	string str = s.substr(pos + s1.length(), s.length() - pos - s1.length());
	str = remove_space(str);

	//cout <<endl<<cstr<<endl;
	double sbox;
	sbox = atof(str.c_str());
	//sbox = strtod(str.c_str(), 0);
	//cout <<endl<<"sbox="<<sbox<<endl;

	return sbox;
}

//parse argument to get the time out
double getTimeout(char *tout) {
	string s = string(tout);
	string s1 = "=";
	size_t pos = s.find(s1);
	string str = s.substr(pos + s1.length(), s.length() - pos - s1.length());
	str = remove_space(str);

	double timeout = atof(str.c_str());
	return timeout;
}

//parse argument to get the lower bound
double getLoBound(char* bound) {
	string s = string(bound);
	string s1 = "=";
	string str = s.substr(s.find(s1) + s1.length(), s.length());
	//cout <<"str: "<<str<<endl;
	string strLo = first_word(str);
	strLo = remove_space(strLo);

	double lo = atof(strLo.c_str());
	return lo;
}

//parse argument to get the upper bound
double getUpBound(char* bound) {
	string s = string(bound);

	string strUp = last_word(s);
	strUp = remove_space(strUp);

	double up = atof(strUp.c_str());
	return up;
}

//This part works on conf.ini file: config parameters

string get_mb(string s) {
	string s1 = "max_bound";
	string str = s.substr(s.find(s1) + s1.length(), s.length());
	return first_word(str);
}
string get_bs(string s) {
	string s1 = "bound_step";
	string str = s.substr(s.find(s1) + s1.length(), s.length());
	return first_word(str);
}

//get max_bound from conf.ini files
double max_bound(char *filename) {
	string line, mb = "";
	size_t pos;
	ifstream myfile(filename);

	if (myfile.is_open()) {
		while (myfile.good()) {
			getline(myfile, line);
			pos = line.find("max_bound");
			if (pos != string::npos) {
				mb = get_mb(line);
			}
		}
		myfile.close();
	} else {
		cout << "Unable to open " << filename << " file!";
		return 0.0;
	}

	char *char_mb = new char[mb.size() + 1];
	strcpy(char_mb, mb.c_str());

	double value;
	sscanf(char_mb, "%lf", &value);
	return value;
}

//get bound_step from conf.ini files
double bound_step(char *filename) {
	string line, bs = "";
	size_t pos;
	ifstream myfile(filename);

	if (myfile.is_open()) {
		while (myfile.good()) {
			getline(myfile, line);
			pos = line.find("bound_step");
			if (pos != string::npos) {
				bs = get_bs(line);
			}
		}
		myfile.close();
	} else {
		cout << "Unable to open " << filename << " file!";
		return 0.0;
	}

	char *char_bs = new char[bs.size() + 1];
	strcpy(char_bs, bs.c_str());

	double value;
	sscanf(char_bs, "%lf", &value);
	return value;
}

//get number of variables in a smt2 file
int num_var(char *filename) {
	string line;
	size_t pos;
	int nvar = 0;
	ifstream myfile(filename);

	if (myfile.is_open()) {
		while (myfile.good()) {
			getline(myfile, line);
			pos = line.find("declare-fun");
			if (pos != string::npos)
				nvar++;
		}
		myfile.close();
	} else {
		cout << "Unable to open " << filename << " file!";
		return -1;
	}
	return nvar;
}

//get >= constraints for variables
string intv_var(string s) {
	string s1 = "assert";
	string str = s.substr(s.find(s1) + s1.length() + 1, s.length());
	int pos = str.length();
	for (int i = str.length() - 1; i > 0; i--)
		if (str.at(i) == ')')
			pos = i;
	return str.substr(0, pos + 1);
}

//get bounds for variables from smt2 files
string var_bound(char *filename, int nvar) {
	string line, svar = "";
	size_t pos;
	int ivar = nvar;
	ifstream myfile(filename);

	if (myfile.is_open()) {
		while (myfile.good()) {
			getline(myfile, line);
			pos = line.find("assert");
			if ((pos != string::npos) && (ivar > 0)) {
				ivar--;
				if (svar == "")
					svar = intv_var(line);
				else
					svar = "(and " + svar + " " + intv_var(line) + ")";
			}
		}
		myfile.close();
	} else
		cout << "Unable to open " << filename << " file!";
	return "(assert " + svar + ")";
}

//get interval constraints from ebg files of SMT benchmark
string smt_intv(char *filename) {
	string line, sIntv = "";
	size_t pos;
	ifstream myfile(filename);

	if (myfile.is_open()) {
		while (myfile.good()) {
			getline(myfile, line);
			pos = line.find("in");
			if (pos != string::npos) {
				if (sIntv == "")
					sIntv = line;
				else
					sIntv = "(ic " + sIntv + " " + line + ")";
			}
		}
		myfile.close();
	} else
		cout << "Unable to open " << filename << " file!";
	return sIntv;
}

//new version: get interval constraints from ebg files of SMT benchmark
string smt_getintv(char *filename) {
	string sIntv = "";
	char line[1024];
	size_t pos;
	FILE* myfile = fopen(filename, "rt");

	if (myfile) {
		while (fgets(line, sizeof(line), myfile)) {
			string s = line;
			pos = s.find("in");
			if (pos != string::npos) {
				int n = s.length();
				//int n=0;
				for (int i = s.length() - 1; i >= 0; i--)
					if (s[i] = '\n') {
						n = i;
						break;
					}
				string tmp = s.substr(0, n);
				if (sIntv == "")
					sIntv = tmp;
				else
					sIntv = "(ic " + sIntv + " " + tmp + ")";
			}
		}
		fclose(myfile);
	} else
		cout << "Unable to open " << filename << " file!";
	return sIntv;
}

//get assertion constraints from ebg files of SMT benchmark
string smt_ass(char *filename) {
	string line, sAss = "";
	size_t pos;
	ifstream myfile(filename);

	if (myfile.is_open()) {
		while (myfile.good()) {
			getline(myfile, line);
			pos = line.find("assert");
			if (pos != string::npos)
				sAss = line;
		}
		myfile.close();
	} else
		cout << "Unable to open " << filename << " file!";
	return sAss;
}

//get the number of literal in a clause represented by a string
int num_of_lit(string cl) {
	string s = remove_space(cl);
	if (s == "")
		return 0;
	int l = s.length();
	int n = 0;
	for (int i = 0; i < l; i++)
		if (s[i] == ' ')
			n++;
	return n + 1;
}

//get the int array from a string
int* getLit(string cl) {
	string s = remove_space(cl);
	int l = s.length();
	int *a = new int[l];
	int pos = 0, prev = 0;
	for (int i = 0; i < l; i++) {
		if (s[i] == ' ') {
			a[pos] = atoi(s.substr(prev, i - prev).c_str());
			prev = i;
			pos++;
		}
	}
	a[pos] = atoi(s.substr(prev, l - prev).c_str());
	return a;
}

//SMT2 format to EBG format (raSAT format form)

//get name of variables
string get_namevar(string s) {
	string s1 = "declare-fun";
	string str = s.substr(s.find(s1) + s1.length() + 1, s.length());
	int pos = str.length();
	for (int i = 0; i < pos; i++)
		if (str.at(i) == ' ') {
			pos = i;
			break;
		}
	return str.substr(0, pos + 1);
}

//get list of name of variables and assign lower bound for them
string get_listvars(char *filename, int nvar) {
	string line, svar = "";
	size_t pos;
	int ivar = nvar;
	ifstream myfile(filename);

	char str_lb[20] = "";
	sprintf(str_lb, "%s", "-inf");

	if (myfile.is_open()) {
		while (myfile.good()) {
			getline(myfile, line);
			pos = line.find("declare-fun");
			if ((pos != string::npos) && (ivar > 0)) {
				ivar--;
				if (svar == "")
					svar = "(>= " + get_namevar(line) + str_lb + ")";
				else
					svar = "(and " + svar + " " + "(>= " + get_namevar(line)
							+ str_lb + "))";
			}
		}
		myfile.close();
	} else
		cout << "Unable to open " << filename << " file!";
	return "(assert " + svar + ")";
}

//get assertion constraints (remove keyword "assert")
string get_assert(string s) {
	string s1 = "assert";
	string str = s.substr(s.find(s1) + s1.length() + 1, s.length());
	int pos = str.length();
	for (int i = str.length() - 1; i > 0; i--) {
		if (str.at(i) == ')') {
			pos = i;
			break;
		}
	}
	return str.substr(0, pos);
}

//get assertion constraints from smt2 files
string get_cons(char *filename, int nvar) {
	string line, svar = "";
	size_t pos;
	int ivar = nvar;
	ifstream myfile(filename);

	if (myfile.is_open()) {
		while (myfile.good()) {
			getline(myfile, line);
			pos = line.find("assert");
			if (pos != string::npos) {
				//if (ivar >0)
				//  ivar --;
				//else
				//svar = line;

				//cout <<endl<<get_assert(line)<<endl;

				if (svar == "")
					svar = get_assert(line);
				else
					svar = "(& " + svar + " " + get_assert(line) + ")";

			}
		}
		svar = "(assert " + svar + ")";
		myfile.close();
	} else
		cout << "Unable to open " << filename << " file!";
	return svar;
}
