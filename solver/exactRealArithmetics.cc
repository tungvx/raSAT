#ifdef iRRAM_INC
#include <map>
#include <vector>
#include <algorithm>
#include <stack> 
#include "iRRAM/lib.h"
#include "iRRAM/core.h"
#include "exactRealArithmetics.h"

using namespace iRRAM;

// function for extracting tokens (words) from a string. Tokens are seperated by delimiter in the string.
// returns a vector of tokens.
std::vector<std::string> string_split(std::string s, const char delimiter) {
	size_t start = 0; // start at the beginning of the string.
	size_t length = s.length(); // store the length of the string for termination of algorithm.
	size_t end = s.find_first_of(delimiter); // find the position of first occurence of the delimiter. 

	std::vector < std::string > output; // we stores the tokens in a vector
	while (start < length) { // while we did not proccess all the string.
		if (end > start) { // if the next occurence of the delimiter is after the current considering position start,
			output.emplace_back(s.substr(start, end - start)); // then we push the token from start to end into the result vector
		}

		if (end == std::string::npos) // This case occurs when the final token has been extracted.
			break;

		start = end + 1; // increase the current position
		end = s.find_first_of(delimiter, start); // find the next occurence of delimiter in the string.
	}

	return output;
}

/*
 * Function for checking equality of two reals numbers.
 */
bool equal(REAL r1, REAL r2) {
	try {
		return r1 == r2; // if r1 != r2
	} catch (iRRAM::Iteration it) { // if r1 == r2
		return true;
	}
}

/*
 * check whether an expression is SAT or not when the assignments of variables are provided.
 */
bool check(string expression, std::map<string, REAL> ass) {
// First, extract the tokens from the input string of expression
	std::vector < std::string > expTokens = string_split(expression, ' ');
// In real computer, we have one stack.
// But here, we use two stack for calculating the final result:
	std::stack < REAL > stack; // the first stack is for storing values of arithmetics operations.
	std::stack<bool> boolStack;	// the other stack stores the value for boolean operations.
// the expresion is in the post fix form, using stacks are approriate.
	unsigned i = 0;
	unsigned length = expTokens.size();
	while (i < length) {
		string tmp = expTokens[i++];
		if (tmp.compare("real") == 0) {
			// expects the value of this real in the next token
			if (i >= length)
				return false;
			// simply push the value into the stack.
			stack.push(atoREAL(expTokens[i++].c_str()));
		} else if (tmp.compare("var") == 0) {
			// we need the name of the variable in the next token.
			if (i >= length)
				return false;
			// extract the variable name
			string varName = expTokens[i++];
			// try to find the variable in the assignments map.
			std::map<string, REAL>::iterator it = ass.find(varName);
			// if the variable was not assgined any value, return UNSAT.
			if (it == ass.end())
				return false;
			// otherwise, push the value to the stack.
			stack.push(it->second);
		} else if (tmp.compare("+") == 0) {
			// we need the stack to have more than 1 value.
			if (stack.size() < 2)
				return false;
			// get the first value
			REAL r1 = stack.top();
			stack.pop();
			// get the second value
			REAL r2 = stack.top();
			stack.pop();
			// add above two values and push into the stack.
			stack.push(r1 + r2);
		} else if (tmp.compare("-") == 0) {
			// we need at least two values on the stack for minus operator.
			if (stack.size() < 2)
				return false;
			// get two values at the top of the stack.
			REAL r1 = stack.top();
			stack.pop();
			REAL r2 = stack.top();
			stack.pop();
			// subtract them, and push back into stack.
			stack.push(r2 - r1);
		} else if (tmp.compare("*") == 0) {
			// we need at least two values on the stack for multiplication operator.
			if (stack.size() < 2)
				return false;
			// pop two values and multiply them, push the result into the stack.
			REAL r1 = stack.top();
			stack.pop();
			REAL r2 = stack.top();
			stack.pop();
			stack.push(r1 * r2);
		} else if (tmp.compare("^") == 0) {
			// we need at least two values on the stack for power operator: a^b or power(a, b)
			if (stack.size() < 2)
				return false;
			// pop two top values on the stack and calculate the power operator. 
			// The result is pushed back into the stack.
			REAL r1 = stack.top();
			stack.pop();
			REAL r2 = stack.top();
			stack.pop();
			stack.push(power(r2, r1));
			/*
			 * The next operators are boolean ones.
			 * For each one, we need two values on the top of the stack.
			 * The result of the operator are stored in the boolStack.
			 */
		} else if (tmp.compare("=") == 0) {
			// we need two values on the stack for comparison
			if (stack.size() < 2)
				return false;
			// get two values on the top of stack.
			REAL r1 = stack.top();
			stack.pop();
			REAL r2 = stack.top();
			stack.pop();
			try {
				// using iRRAM to check whether they are equal.
				// Unfortunately, if r1 and r2 are actually equal, iRRAM will enter an infinite loop.
				// In such case, iRRAM throw an iteration object.
				if (r1 == r2)
					boolStack.push(true);// this line is never executed because the Iteration object will be thrown.
				else
					boolStack.push(false);// iRRAM can detect if two real numbers are different.
			} catch (iRRAM::Iteration it) {
				boolStack.push(true); // not exactly two numbers are the same!
									  // iRRAM throws this exception when the number of iterations is large enough.
									  // Though we do not know how many iterations is large.
			}

		} else if (tmp.compare("<") == 0) {
			// we need two values on the stack for comparison
			if (stack.size() < 2)
				return false;
			// pop two top values on the stack.
			REAL r1 = stack.top();
			stack.pop();
			REAL r2 = stack.top();
			stack.pop();
			// use iRRAM to compare them.
			try {
				if (r2 < r1)
					boolStack.push(true); // if r2 is really less than r1, iRRAM can detect the result.
				else
					boolStack.push(false); // iRRAM can also detect the false result if r1 is less than r2.
			} catch (iRRAM::Iteration it) {
				boolStack.push(false); // the exception will be thrown if r1 and r2 are exactly the same.
			}
		} else if (tmp.compare("<=") == 0) {
			// we need two values on the stack for comparison
			if (stack.size() < 2)
				return false;
			// pop two top values on the stack.
			REAL r1 = stack.top();
			stack.pop();
			REAL r2 = stack.top();
			stack.pop();
			try {
				if (r2 <= r1)
					boolStack.push(true); // This case when r2 < r1. If r1 = r2, exception will be thrown
				else
					boolStack.push(false);
			} catch (iRRAM::Iteration it) { // when r1 = r2
				boolStack.push(true);
			}
		} else if (tmp.compare(">") == 0) {
			// we need two values on the stack for comparison
			if (stack.size() < 2)
				return false;
			// pop two top values on the stack
			REAL r1 = stack.top();
			stack.pop();
			REAL r2 = stack.top();
			stack.pop();
			try {
				if (r2 > r1)
					boolStack.push(true); // if r2 > r1
				else
					boolStack.push(false); // if r2 < r1
			} catch (iRRAM::Iteration it) { // if r2 = r1
				boolStack.push(false);
			}
		} else if (tmp.compare(">=") == 0) {
			// we need two values on the stack for comparison
			if (stack.size() < 2)
				return false;
			REAL r1 = stack.top();
			stack.pop();
			REAL r2 = stack.top();
			stack.pop();
			try {
				if (r2 >= r1)
					boolStack.push(true); // if r2 > r1
				else
					boolStack.push(false); // if r2 < r1
			} catch (iRRAM::Iteration it) { // if r2 == r1
				boolStack.push(true);
			}
		} else if (tmp.compare("and") == 0) {
			// we need two values on the boolean stack.
			if (boolStack.size() < 2)
				return false;
			// pop two boolean values on top of boolStack.
			bool b1 = boolStack.top();
			boolStack.pop();
			bool b2 = boolStack.top();
			boolStack.pop();
			if (b1 && b2)
				boolStack.push(true);
			else
				boolStack.push(false);
		}
	}
// at the end of evaluating each expression, the arithmetics stack should be empty.
// and the boolean stack should contain 1 boolean value.
	if (stack.size() != 0 && boolStack.size() != 1)
		return false;
	return boolStack.top();
}

/*
 * check SAT of list of clauses (expressions) provided the assignments of variables.
 */
bool checkSAT(char* clausesList, char* assignments) {
	cout << "ClausesList: " << clausesList << "\n";
	cout << "Assignments: " << assignments << "\n";
// first, try to parse the assignments from the string and store them into a map for later uses.
	std::map < string, REAL > ass;
	std::vector < std::string > tokens = string_split(assignments, ' '); // extract list of tokens.
	unsigned i = 0;
	unsigned length = tokens.size();
	while (i < length) {
		string var = tokens[i++]; // name of the variable.
		if (i >= length) // we need one more Real number in the list.
			break;
		ass[var] = atoREAL(tokens[i++].c_str()); // store the value of of the variable into the map.
	}

// parse and check the list of SAT clauses.
	std::vector < std::string > clauses = string_split(clausesList, ','); // get the vector of expressions.
	for (std::vector<string>::iterator it = clauses.begin();
			it != clauses.end(); ++it)
		if (!check(*it, ass)) { // if one exression is UNSAT, reuturn false.
			cout << "iRRAM detects error at: " << *it << std::endl;
			return false;
		}
	return true; // all the expressions are checked to be SAT.
}

/*
 * We use iRRAM for checking the SAT of expressions over the intervals of variables.
 */
bool checkRangeSAT(string expression, std::map<string, INTERVAL> intervals,
		bool checingkSAT) {
//	cout << expression << " : ";
// convert the string format of the expression to the list of tokens:
	std::vector < std::string > expTokens = string_split(expression, ' ');
// In real computer, we have one stack.
// But here, we use two stack for calculating the final result:
	std::stack < INTERVAL > stack; // the first stack is for storing values of arithmetics operations.
	std::stack<bool> boolStack;	// the other stack stores the value for boolean operations.
// the expresion is in the post fix form, using stacks are approriate.
	unsigned i = 0;
	unsigned length = expTokens.size();
	while (i < length) {
		string tmp = expTokens[i++];
		if (tmp.compare("real") == 0) {
			// expects the value of this real in the next token
			if (i >= length)
				return false;
			// simply push the value as an interval into the stack.
			stack.push(INTERVAL(atoREAL(expTokens[i++].c_str())));
		} else if (tmp.compare("var") == 0) {
			// we need the name of the variable in the next token.
			if (i >= length)
				return false;
			// extract the variable name
			string varName = expTokens[i++];
			// try to find the variable in the assignments map.
			std::map<string, INTERVAL>::iterator it = intervals.find(varName);
			// if the variable was not assgined to any intervals, return UNSAT.
			if (it == intervals.end())
				return false;
			// otherwise, push the value to the stack.
			stack.push(it->second);
		} else if (tmp.compare("+") == 0) {
			// we need the stack to have more than 1 interval.
			if (stack.size() < 2)
				return false;
			// get the first interval
			INTERVAL i1 = stack.top();
			stack.pop();
			// get the second interval
			INTERVAL i2 = stack.top();
			stack.pop();
			// add above two intervals and push into the stack.
			stack.push(i1 + i2);
		} else if (tmp.compare("-") == 0) {
			// we need at least two intervals on the stack for minus operator.
			if (stack.size() < 2)
				return false;
			// get two intervals at the top of the stack.
			INTERVAL i1 = stack.top();
			stack.pop();
			INTERVAL i2 = stack.top();
			stack.pop();
			// subtract them, and push back into stack.
			stack.push(i2 - i1);
		} else if (tmp.compare("*") == 0) {
			// we need at least two intervals on the stack for multiplication operator.
			if (stack.size() < 2)
				return false;
			// pop two values and multiply them, push the result into the stack.
			INTERVAL i1 = stack.top();
			stack.pop();
			INTERVAL i2 = stack.top();
			stack.pop();
			stack.push(i1 * i2);
		} else if (tmp.compare("^") == 0) {
			// we need at least two intervals on the stack for power operator: a^b or power(a, b)
			if (stack.size() < 2)
				return false;
			// pop two top intervals on the stack
			INTERVAL i1 = stack.top();
			stack.pop();
			INTERVAL i2 = stack.top();
			stack.pop();
			long pw = round(i1.low);// take the round of the lower bound of i1 as the power number
			// first, i1 must have two bounds as the same natural numbers because we only consider the polynomials.
			if (pw != round(i1.upp))
				return false;
			// calculate the power operator by multiplying i2 iteratively i1 times:
			INTERVAL result = i2;
			for (int j = 1; j < pw; j++)
				result = result * i2;
			// The result is pushed back into the stack.
			stack.push(result);

			/*
			 * The next operators are boolean ones.
			 * For each one, we need two values on the top of the stack.
			 * The result of the operator are stored in the boolStack.
			 */
		} else if (tmp.compare("=") == 0) {
			// we need two intervals on the stack for comparison
			if (stack.size() < 2)
				return false;
			// get two intervals on the top of stack.
			INTERVAL i1 = stack.top();
			stack.pop();
			INTERVAL i2 = stack.top();
			stack.pop();
			// [a, b] = [c, d] <==> a = d && b = c
			boolStack.push(equal(i1.low, i2.upp) && equal(i1.upp, i2.low));
		} else if (tmp.compare("<") == 0) {
			// we need two intervals on the stack for comparison
			if (stack.size() < 2)
				return false;
			// pop two top intervals on the stack.
			INTERVAL i1 = stack.top();
			stack.pop();
			INTERVAL i2 = stack.top();
			stack.pop();
			// use iRRAM to compare them.
			// [a, b] < [c, d] <==> b < c
			try {
				if (i2.upp < i1.low)
					boolStack.push(true); // if i2.upp < i1.low, iRRAM can detect the result.
				else
					boolStack.push(false); // iRRAM can also detect the false result if i2.upp > i1.low.
			} catch (iRRAM::Iteration it) {
				boolStack.push(false); // the exception will be thrown if i2.upp = i1.low.
			}
		} else if (tmp.compare("<=") == 0) {
			// we need two intervals on the stack for comparison
			if (stack.size() < 2)
				return false;
			// pop two top intervals on the stack.
			INTERVAL i1 = stack.top();
			stack.pop();
			INTERVAL i2 = stack.top();
			stack.pop();

			try {
				if (i2.upp <= i1.low) // i2.upp < i1.low ==> i2 <= i1
					boolStack.push(true);
				else
					// i2.upp > i1.low, we can not conclude that i2 <= i1
					boolStack.push(false);
			} catch (iRRAM::Iteration it) { // i2.upp = i1.low => i2 <= i1
				boolStack.push(true);
			}
		} else if (tmp.compare(">") == 0) {
			// we need two intervals on the stack for comparison
			if (stack.size() < 2)
				return false;
			// pop two top intervals on the stack
			INTERVAL i1 = stack.top();
			stack.pop();
			INTERVAL i2 = stack.top();
			stack.pop();
			try {
				if (i2.low > i1.upp) // i2.low > i1.upp ==> i2 > i1 
					boolStack.push(true);
				else
					boolStack.push(false); // if i2.low < i1.upp, we can not conclude i2 > i1
			} catch (iRRAM::Iteration it) { // if i2.low = i1.upp
				boolStack.push(false);
			}
		} else if (tmp.compare(">=") == 0) {
			// we need two intervals on the stack for comparison
			if (stack.size() < 2)
				return false;
			INTERVAL i1 = stack.top();
			stack.pop();
			INTERVAL i2 = stack.top();
			stack.pop();
//			cout << "[" << i2.low << ", " << i2.upp << "] >= [" << i1.low
//					<< ", " << i1.upp << "]\n";
			try {
				if (i2.low >= i1.upp)
					boolStack.push(true); // if i2.low > i1.upp, i2 >= i1
				else
					boolStack.push(false); // if i2.low < i1.upp, we can not conclude that i2 >= i1.
			} catch (iRRAM::Iteration it) { // if i2.low = i1.upp ==> i2 >= i1.
				boolStack.push(true);
			}
		} else if (tmp.compare("and") == 0) {
			// we need two values on the boolean stack.
			if (boolStack.size() < 2)
				return false;
			// pop two boolean values on top of boolStack.
			bool b1 = boolStack.top();
			boolStack.pop();
			bool b2 = boolStack.top();
			boolStack.pop();
			if (b1 && b2)
				boolStack.push(true);
			else
				boolStack.push(false);
		}
	}
	// at the end of evaluating each expression, the arithmetics stack should be empty.
	// and the boolean stack should contain 1 boolean value.
	if (stack.size() != 0 && boolStack.size() != 1)
		return false;
	return boolStack.top();
}

bool checkRange(char* clausesAndVarsIntervals, bool checkingSAT) {
// extract clauses and intervals
	std::vector < std::string > clausesAndIntervals = string_split(
			clausesAndVarsIntervals, ';');
	if (clausesAndIntervals.size() != 2) // when there is nothing to test, just return true.
		return true;
	string clausesString = clausesAndIntervals[0];
	string intervalsString = clausesAndIntervals[1];
//	cout << intervalsString << std::endl;

// first, parse list of intervals into a map.
	std::map < string, INTERVAL > intervals;
	std::vector < std::string > intervalsTokens = string_split(intervalsString,
			' ');
	unsigned i = 0;
	unsigned length = intervalsTokens.size();
	while (i < length) {
		string var = intervalsTokens[i++];
		if (i >= length - 1) // we need two more numbers in the list
			return false;
		intervals[var] = INTERVAL(atoREAL(intervalsTokens[i++].c_str()),
				atoREAL(intervalsTokens[i++].c_str()));
	}

// Then we parse and check the SAT list:
	std::vector < std::string > clauses = string_split(clausesString, ',');
	for (std::vector<string>::iterator it = clauses.begin();
			it != clauses.end(); ++it)
		if (!checkRangeSAT(*it, intervals, checkingSAT))
			return false;

	return true;
}

#endif
