/*

iRRAM_FUNCTION.h -- header file for the FUNCTION part of the iRRAM library

Copyright (C) 2001-2013 Norbert Mueller

This file is part of the iRRAM Library.

The iRRAM Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Library General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

The iRRAM Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
License for more details.

You should have received a copy of the GNU Library General Public License
along with the iRRAM Library; see the file COPYING.LIB.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA. 
*/

#ifndef iRRAM_FUNCTION
#define iRRAM_FUNCTION
#include <utility>
#include <vector>

namespace iRRAM {


template<class PARAM,class RESULT>
class FUNCTION;

template<class PARAM,class RESULT>
class FUNCTIONAL_object
{
public:
	int ref_count;

	FUNCTIONAL_object(){
		ref_count=1;
		};

	bool release_check(){
		ref_count--;
		return ref_count != 0;
		}


	FUNCTIONAL_object* clone()
		{
		FUNCTIONAL_object* that = const_cast<FUNCTIONAL_object*>(this);
		that->ref_count++;
		return that;
		}
	virtual ~FUNCTIONAL_object(){};

	virtual void clear()
		{
		if (release_check()) return;
		delete this; 
		}

	virtual RESULT eval(const PARAM &z){RESULT r; return r;};
//	returns the default value from datatype RESULT 
};

template<class PARAM,class RESULT>
class FUNCTIONAL_algorithm :public FUNCTIONAL_object<PARAM,RESULT>
{
public:
	RESULT (*_eval)(const PARAM &z);

	FUNCTIONAL_algorithm(
		RESULT (evalp)(const PARAM &z)
		){_eval=evalp;}

	virtual void clear()
		{
		if (this->release_check()) return;
		delete this; 
		}

	RESULT eval(const PARAM &z){return _eval(z);}; 
};


template<class PARAM,class RESULT>
class FUNCTIONAL_value :public FUNCTIONAL_object<PARAM,RESULT>
{
public:
	RESULT _value;

	FUNCTIONAL_value(
		const RESULT &value
		){_value=value;}

	virtual void clear()
		{
		if (this->release_check()) return;
		delete this; 
		}

	RESULT eval(const PARAM &z){return _value;}; 
};


template<class PARAM, class INTERMEDIATE, class RESULT>
class FUNCTIONAL_composition :public FUNCTIONAL_object<PARAM,RESULT>
{
public:
	FUNCTIONAL_object<PARAM,INTERMEDIATE> *_inner;
	FUNCTIONAL_object<INTERMEDIATE,RESULT> *_outer;

	FUNCTIONAL_composition(
		FUNCTIONAL_object<INTERMEDIATE,RESULT>* outer, //outer function
		FUNCTIONAL_object<PARAM,INTERMEDIATE>* inner //inner function
		){
		_outer = outer->clone();
		_inner = inner->clone();
		}

	virtual void clear()
		{
		if (this->release_check()) return;
		_inner->clear();
		_outer->clear();
		delete this; 
		}

	RESULT eval(const PARAM &z){return _outer->eval(_inner->eval(z));}; 
};


template<class PARAM1, class RESULT1, class PARAM2, class RESULT2>
class FUNCTIONAL_product :public 
	FUNCTIONAL_object< std::pair<PARAM1,PARAM2>, std::pair<RESULT1,RESULT2> > 
{
public:
	FUNCTIONAL_object<PARAM1,RESULT1> * _f_1;
	FUNCTIONAL_object<PARAM2,RESULT2> * _f_2;

	FUNCTIONAL_product(
		FUNCTIONAL_object<PARAM1,RESULT1>* f_1,
		FUNCTIONAL_object<PARAM2,RESULT2>* f_2
		){
		_f_1 = f_1->clone();
		_f_2 = f_2->clone();
		}

	virtual void clear()
		{
		if (this->release_check()) return;
		_f_1->clear();
		_f_2->clear();
		delete this; 

		}

	std::pair<RESULT1,RESULT2> eval(
		const std::pair<PARAM1,PARAM2> &z
		){
		std::pair<RESULT1,RESULT2> r;
		r.first  = _f_1->eval(z.first);
		r.second = _f_2->eval(z.second);
		return r;
		}; 
};


template<class PARAM, class RESULT1, class RESULT2>
class FUNCTIONAL_juxtaposition :public 
	FUNCTIONAL_object< PARAM, std::pair<RESULT1,RESULT2> > 
{
public:
	FUNCTIONAL_object<PARAM,RESULT1> * _f_1;
	FUNCTIONAL_object<PARAM,RESULT2> * _f_2;

	FUNCTIONAL_juxtaposition(
		FUNCTIONAL_object<PARAM,RESULT1>* f_1, 
		FUNCTIONAL_object<PARAM,RESULT2>* f_2 
		){
		_f_1 = f_1->clone();
		_f_2 = f_2->clone();
		}

	virtual void clear()
		{
		if (this->release_check()) return;
		_f_1->clear();
		_f_2->clear();
		delete this; 
		}

	std::pair<RESULT1,RESULT2> eval(const PARAM &z)
		{
		std::pair<RESULT1,RESULT2> r;
		r.first  = _f_1->eval(z);
		r.second = _f_2->eval(z);
		return r;
		}; 

};

template<class PARAM, class RESULT1, class RESULT2>
class FUNCTIONAL_first :public FUNCTIONAL_object< PARAM, RESULT1 > 
{
public:
	FUNCTIONAL_object<PARAM,std::pair<RESULT1,RESULT2> > * _f;

	FUNCTIONAL_first(FUNCTIONAL_object<PARAM,std::pair<RESULT1,RESULT2> >* f
		){_f = f->clone();}

	virtual void clear()
		{
		if (this->release_check()) return;
		_f->clear();
		delete this; 
		}

	RESULT1 eval(const PARAM &z) {return (_f->eval(z)).first;}; 
};

template<class PARAM, class RESULT1, class RESULT2>
class FUNCTIONAL_second :public FUNCTIONAL_object< PARAM, RESULT2 > 
{
public:
	FUNCTIONAL_object<PARAM,std::pair<RESULT1,RESULT2> > * _f;

	FUNCTIONAL_second(FUNCTIONAL_object<PARAM,std::pair<RESULT1,RESULT2> >* f
		){_f = f->clone();}

	virtual void clear()
		{
		if (this->release_check()) return;
		_f->clear();
		delete this; 
		}

	RESULT2 eval(const PARAM &z) {return (_f->eval(z)).second;}; 
};

template<class PARAM1, class PARAM2, class RESULT>
class FUNCTIONAL_bind_first :public FUNCTIONAL_object< PARAM2, RESULT > 
{
public:
	FUNCTIONAL_object<std::pair<PARAM1,PARAM2>,RESULT> * _f;
	PARAM1 _x;

	FUNCTIONAL_bind_first(
		FUNCTIONAL_object<std::pair<PARAM1,PARAM2>,RESULT >* f,
		const PARAM1& x
		){_f = f->clone();_x=x;}

	virtual void clear()
		{
		if (this->release_check()) return;
		_f->clear();
		delete this; 
		}

	RESULT eval(const PARAM2 &y) {return _f->eval(std::make_pair<PARAM1,PARAM2>(_x,y));}; 
};

template<class PARAM1, class PARAM2, class RESULT>
class FUNCTIONAL_bind_second :public FUNCTIONAL_object< PARAM1, RESULT > 
{
public:
	FUNCTIONAL_object<std::pair<PARAM1,PARAM2>,RESULT> * _f;
	PARAM2 _y;

	FUNCTIONAL_bind_second(
		FUNCTIONAL_object<std::pair<PARAM1,PARAM2>,RESULT >* f,
		const PARAM2& y
		){_f = f->clone();_y=y;}

	virtual void clear(){
		if (this->release_check()) return;
		_f->clear();
		delete this; 
		}

	RESULT eval(const PARAM1 &x) {return _f->eval(std::make_pair<PARAM1,PARAM2>(x,_y));}; 
};


template<class PARAM,class RESULT>
class FUNCTIONAL_projection :public FUNCTIONAL_object<PARAM,RESULT>
{
public:
	FUNCTIONAL_object<PARAM,std::vector<RESULT>  > *_f;
	int _i;

	FUNCTIONAL_projection(
		FUNCTIONAL_object<PARAM,std::vector<RESULT> > *f,
//std::vector<RESULT> (evalp)(const PARAM &z),
		const int i 
		){_f = f->clone();_i=i;}

	virtual void clear()
		{
		if (this->release_check()) return;
		_f->clear();
		delete this; 
		}

	RESULT eval(const PARAM &z){return (_f->eval(z))[_i];}; 
};


//********************************************************************************


template<class PARAM,class RESULT>
class FUNCTION {
public: 
	FUNCTIONAL_object<PARAM,RESULT> * _fobject;

	FUNCTION(
		FUNCTIONAL_object<PARAM,RESULT> * fobject
		){_fobject = fobject;};

	FUNCTION(){_fobject= new FUNCTIONAL_object<PARAM,RESULT>;};

	FUNCTION(RESULT (evalp)(const PARAM &z)){_fobject= new FUNCTIONAL_algorithm<PARAM,RESULT>(evalp);};

	
	FUNCTION(
		const FUNCTION &z
		){_fobject = z._fobject->clone();};

	~FUNCTION(){_fobject->clear();};

	inline FUNCTION& operator = (
		const FUNCTION &z
		){ 
        	FUNCTIONAL_object<PARAM,RESULT> * copy = (*z._fobject).clone();
		(*_fobject).clear();
		_fobject = copy;
		return (*this);
		}; 

	inline RESULT operator()(
		const PARAM &z
		){return _fobject->eval(z);}; 

	template<class PRIMARY> inline FUNCTION<PRIMARY,RESULT> operator () (
	const FUNCTION<PRIMARY,PARAM> &inner
	){
		return new FUNCTIONAL_composition<PRIMARY,PARAM,RESULT>(_fobject,inner._fobject);
	};

};

//********************************************************************************

template<class PARAM,class RESULT>	inline FUNCTION<PARAM,RESULT> from_algorithm (
		RESULT (evalp)(const PARAM &z)
		){return new FUNCTIONAL_algorithm<PARAM,RESULT>(evalp);};

template<class PARAM,class RESULT>	inline FUNCTION<PARAM,RESULT> from_value (
		const RESULT &value
		){return new FUNCTIONAL_value<PARAM,RESULT>(value);};

template<class PARAM,class INTERMEDIATE,class RESULT>
inline FUNCTION<PARAM,RESULT> compose (
	const FUNCTION<INTERMEDIATE,RESULT>&outer,
	const FUNCTION<PARAM,INTERMEDIATE> &inner
	){
		return new FUNCTIONAL_composition<PARAM,INTERMEDIATE,RESULT>(outer._fobject,inner._fobject);
	};


template<class PARAM1, class RESULT1, class PARAM2, class RESULT2>
inline FUNCTION<std::pair<PARAM1,PARAM2>,std::pair<RESULT1,RESULT2> > product ( 
	const FUNCTION<PARAM1,RESULT1> &f_1, 
	const FUNCTION<PARAM2,RESULT2> &f_2
	){
		return new FUNCTIONAL_product<PARAM1,RESULT1,PARAM2,RESULT2>
						(f_1._fobject,f_2._fobject);
	}

template<class PARAM, class RESULT1, class RESULT2>
inline FUNCTION<PARAM,std::pair<RESULT1,RESULT2> > juxtaposition ( 
	const FUNCTION<PARAM,RESULT1> &f_1, 
	const FUNCTION<PARAM,RESULT2> &f_2
	){
		return new FUNCTIONAL_juxtaposition<PARAM,RESULT1,RESULT2>
						(f_1._fobject,f_2._fobject);
	}

template<class PARAM, class RESULT1, class RESULT2>
inline FUNCTION<PARAM,RESULT1 > first ( 
	const FUNCTION<PARAM,std::pair<RESULT1,RESULT2> > &f
	){
		return new FUNCTIONAL_first<PARAM,RESULT1,RESULT2>(f._fobject);
	}

template<class PARAM, class RESULT1, class RESULT2>
inline FUNCTION<PARAM,RESULT2 > second ( 
	const FUNCTION<PARAM,std::pair<RESULT1,RESULT2> > &f
	){
		return new FUNCTIONAL_second<PARAM,RESULT1,RESULT2> (f._fobject);
	}

template<class PARAM1, class PARAM2, class RESULT>
inline FUNCTION<PARAM2,RESULT> bind_first ( 
	const FUNCTION<std::pair<PARAM1,PARAM2>,RESULT > &f,
	const PARAM1& x
	){
		return new FUNCTIONAL_bind_first<PARAM1,PARAM2,RESULT>(f._fobject,x);
	}

template<class PARAM1, class PARAM2, class RESULT>
inline FUNCTION<PARAM1,RESULT> bind_second ( 
	const FUNCTION<std::pair<PARAM1,PARAM2>,RESULT > &f,
	const PARAM2& y
	){
		return new FUNCTIONAL_bind_second<PARAM1,PARAM2,RESULT>(f._fobject,y);
	}

template<class PARAM,class RESULT>	
inline FUNCTION<PARAM,RESULT> projection (
		const FUNCTION<PARAM,std::vector<RESULT> > &f,
		const int i 
		){return new FUNCTIONAL_projection<PARAM,RESULT> (f._fobject,i);};

//********************************************************************************


template<class PARAM>
class FUNCTIONAL_polynomial :public FUNCTIONAL_object< PARAM,PARAM> 
{
public:
	std::vector<PARAM> _coeff;

	FUNCTIONAL_polynomial(const std::vector<PARAM> &coeff){_coeff = coeff;}

	virtual void clear()
		{
		if (this->release_check()) return;
		delete this; 
		}

	PARAM eval(const PARAM &z) {
		PARAM sum=0;
		PARAM factor=1;
		for (unsigned int i=0; i<_coeff.size();i++){
			sum=sum+factor*_coeff[i];
			factor=factor*z;
			}
		return sum;
	}; 
};


template<class PARAM>
inline FUNCTION<PARAM,PARAM> polynomial ( 
	const std::vector<PARAM> &coeff
	){
		return new FUNCTIONAL_polynomial<PARAM> (coeff);
	}


//********************************************************************************


template<class PARAM,class RESULT>
class FUNCTIONAL_addition :public FUNCTIONAL_object<PARAM,RESULT>
{
public:
	FUNCTIONAL_object<PARAM,std::pair<RESULT,RESULT> > *_f;

	FUNCTIONAL_addition(
		FUNCTIONAL_object<PARAM,std::pair<RESULT,RESULT> > *f
		){
		_f = f->clone();
		}

	virtual void clear()
		{
		if (this->release_check()) return;
		_f->clear();
		delete this; 
		}

	RESULT eval(const PARAM &z){
		std::pair<RESULT,RESULT> r = _f->eval(z);
		return r.first+r.second;
		}; 
};

template<class PARAM,class RESULT>
inline FUNCTION<PARAM,RESULT> addition ( 
	const FUNCTION<PARAM,std::pair<RESULT,RESULT> > &a
	){
		return new FUNCTIONAL_addition<PARAM,RESULT>(a._fobject);
	};

template<class PARAM,class RESULT>
inline FUNCTION<PARAM,RESULT> operator + ( 
	const FUNCTION<PARAM,RESULT> &a,
	const FUNCTION<PARAM,RESULT> &b
	){
		return addition(juxtaposition(a,b));
	};


//********************************************************************************


template<class PARAM,class RESULT>
class FUNCTIONAL_subtraction :public FUNCTIONAL_object<PARAM,RESULT>
{
public:
	FUNCTIONAL_object<PARAM,std::pair<RESULT,RESULT> > *_f;

	FUNCTIONAL_subtraction(
		FUNCTIONAL_object<PARAM,std::pair<RESULT,RESULT> > *f
		){
		_f = f->clone();
		}

	virtual void clear()
		{
		if (this->release_check()) return;
		_f->clear();
		delete this; 
		}

	RESULT eval(const PARAM &z){
		std::pair<RESULT,RESULT> r = _f->eval(z);
		return r.first - r.second;
		}; 
};

template<class PARAM,class RESULT>
inline FUNCTION<PARAM,RESULT> subtraction ( 
	const FUNCTION<PARAM,std::pair<RESULT,RESULT> > &a
	){
		return new FUNCTIONAL_subtraction<PARAM,RESULT>(a._fobject);
	};

template<class PARAM,class RESULT>
inline FUNCTION<PARAM,RESULT> operator - ( 
	const FUNCTION<PARAM,RESULT> &a,
	const FUNCTION<PARAM,RESULT> &b
	){
		return subtraction(juxtaposition(a,b));
	};


//********************************************************************************


template<class PARAM,class RESULT>
class FUNCTIONAL_multiplication :public FUNCTIONAL_object<PARAM,RESULT>
{
public:
	FUNCTIONAL_object<PARAM,std::pair<RESULT,RESULT> > *_f;

	FUNCTIONAL_multiplication(
		FUNCTIONAL_object<PARAM,std::pair<RESULT,RESULT> > *f
		){
		_f = f->clone();
		}

	virtual void clear()
		{
		if (this->release_check()) return;
		_f->clear();
		delete this; 
		}

	RESULT eval(const PARAM &z){
		std::pair<RESULT,RESULT> r = _f->eval(z);
		return r.first * r.second;
		}; 
};

template<class PARAM,class RESULT>
inline FUNCTION<PARAM,RESULT> multiplication ( 
	const FUNCTION<PARAM,std::pair<RESULT,RESULT> > &a
	){
		return new FUNCTIONAL_multiplication<PARAM,RESULT>(a._fobject);
	};

template<class PARAM,class RESULT>
inline FUNCTION<PARAM,RESULT> operator * ( 
	const FUNCTION<PARAM,RESULT> &a,
	const FUNCTION<PARAM,RESULT> &b
	){
		return multiplication(juxtaposition(a,b));
	};


//********************************************************************************


template<class PARAM,class RESULT>
class FUNCTIONAL_division :public FUNCTIONAL_object<PARAM,RESULT>
{
public:
	FUNCTIONAL_object<PARAM,std::pair<RESULT,RESULT> > *_f;

	FUNCTIONAL_division(
		FUNCTIONAL_object<PARAM,std::pair<RESULT,RESULT> > *f
		){
		_f = f->clone();
		}

	virtual void clear()
		{
		if (this->release_check()) return;
		_f->clear();
		delete this; 

		}

	RESULT eval(const PARAM &z){
		std::pair<RESULT,RESULT> r = _f->eval(z);
		return r.first / r.second;
		}; 
};

template<class PARAM,class RESULT>
inline FUNCTION<PARAM,RESULT> division ( 
	const FUNCTION<PARAM,std::pair<RESULT,RESULT> > &a
	){
		return new FUNCTIONAL_division<PARAM,RESULT>(a._fobject);
	};

template<class PARAM,class RESULT>
inline FUNCTION<PARAM,RESULT> operator / ( 
	const FUNCTION<PARAM,RESULT> &a,
	const FUNCTION<PARAM,RESULT> &b
	){
		return division(juxtaposition(a,b));
	};


//********************************************************************************


template<class PARAM,class RESULT>
inline FUNCTION<PARAM,RESULT> operator + ( 
	const FUNCTION<PARAM,RESULT> &a,
	const RESULT &b
	){
		return addition(juxtaposition(a,from_value<PARAM,RESULT> (b)));
	};

template<class PARAM,class RESULT>
inline FUNCTION<PARAM,RESULT> operator + ( 
	const RESULT &a,
	const FUNCTION<PARAM,RESULT> &b
	){
		return addition(juxtaposition(from_value<PARAM,RESULT> (a) ,b));
	};


//********************************************************************************


template<class PARAM,class RESULT>
inline FUNCTION<PARAM,RESULT> operator * ( 
	const FUNCTION<PARAM,RESULT> &a,
	const RESULT &b
	){
		return multiplication(juxtaposition(a,from_value<PARAM,RESULT> (b)));
	};

template<class PARAM,class RESULT>
inline FUNCTION<PARAM,RESULT> operator * ( 
	const RESULT &a,
	const FUNCTION<PARAM,RESULT> &b
	){
		return multiplication(juxtaposition(from_value<PARAM,RESULT> (a) ,b));
	};


//********************************************************************************


template<class PARAM,class RESULT>
inline FUNCTION<PARAM,RESULT> operator - ( 
	const FUNCTION<PARAM,RESULT> &a,
	const RESULT &b
	){
		return subtraction(juxtaposition(a,from_value<PARAM,RESULT> (b)));
	};

template<class PARAM,class RESULT>
inline FUNCTION<PARAM,RESULT> operator - ( 
	const RESULT &a,
	const FUNCTION<PARAM,RESULT> &b
	){
		return subtraction(juxtaposition(from_value<PARAM,RESULT> (a) ,b));
	};


//********************************************************************************


template<class PARAM,class RESULT>
inline FUNCTION<PARAM,RESULT> operator / ( 
	const FUNCTION<PARAM,RESULT> &a,
	const RESULT &b
	){
		return division(juxtaposition(a,from_value<PARAM,RESULT> (b)));
	};

template<class PARAM,class RESULT>
inline FUNCTION<PARAM,RESULT> operator / ( 
	const RESULT &a,
	const FUNCTION<PARAM,RESULT> &b
	){
		return division(juxtaposition(from_value<PARAM,RESULT> (a) ,b));
	};


//********************************************************************************


// Be careful: The following operator "f<g" yields a function h,
// such that h(x)=true <=> f(x)<g(x)   (pointwise!)

template<class PARAM,class RESULT>
class FUNCTIONAL_less_than :public FUNCTIONAL_object<PARAM,LAZY_BOOLEAN>
{
public:
	FUNCTIONAL_object<PARAM,std::pair<RESULT,RESULT> > *_f;

	FUNCTIONAL_less_than(
		FUNCTIONAL_object<PARAM,std::pair<RESULT,RESULT> > *f
		){
		_f = f->clone();
		}

	virtual void clear()
		{
		if (this->release_check()) return;
		_f->clear();
		delete this; 
		}

	LAZY_BOOLEAN eval(const PARAM &z){
		std::pair<RESULT,RESULT> r = _f->eval(z);
		return (r.first) < (r.second);
		}; 
};

template<class PARAM,class RESULT>
inline FUNCTION<PARAM,LAZY_BOOLEAN> less_than ( 
	const FUNCTION<PARAM,std::pair<RESULT,RESULT> > &a
	){
		return new FUNCTIONAL_less_than<PARAM,RESULT>(a._fobject);
	};

template<class PARAM,class RESULT>
inline FUNCTION<PARAM,LAZY_BOOLEAN> operator < ( 
	const FUNCTION<PARAM,RESULT> &a,
	const FUNCTION<PARAM,RESULT> &b
	){
		return less_than(juxtaposition(a,b));
	};

template<class PARAM,class RESULT>
inline FUNCTION<PARAM,LAZY_BOOLEAN> operator > ( 
	const FUNCTION<PARAM,RESULT> &a,
	const FUNCTION<PARAM,RESULT> &b
	){
		return less_than(juxtaposition(b,a));
	};


//********************************************************************************


template<class PARAM,class RESULT>
inline FUNCTION<PARAM,LAZY_BOOLEAN> operator < ( 
	const FUNCTION<PARAM,RESULT> &a,
	const RESULT &b
	){
		return less_than(juxtaposition(a,from_value<PARAM,RESULT> (b)));
	};

template<class PARAM,class RESULT>
inline FUNCTION<PARAM,LAZY_BOOLEAN> operator < ( 
	const RESULT &a,
	const FUNCTION<PARAM,RESULT> &b
	){
		return less_than(juxtaposition(from_value<PARAM,RESULT> (a) ,b));
	};

template<class PARAM,class RESULT>
inline FUNCTION<PARAM,LAZY_BOOLEAN> operator > ( 
	const FUNCTION<PARAM,RESULT> &a,
	const RESULT &b
	){
		return less_than(juxtaposition(from_value<PARAM,RESULT> (b),a));
	};

template<class PARAM,class RESULT>
inline FUNCTION<PARAM,LAZY_BOOLEAN> operator > ( 
	const RESULT &a,
	const FUNCTION<PARAM,RESULT> &b
	){
		return less_than(juxtaposition(b,from_value<PARAM,RESULT> (a)));
	};

//********************************************************************************


template<class PARAM>
class FUNCTIONAL_and :public FUNCTIONAL_object<PARAM,LAZY_BOOLEAN>
{
public:
	FUNCTIONAL_object<PARAM,std::pair<LAZY_BOOLEAN,LAZY_BOOLEAN> > *_f;

	FUNCTIONAL_and(
		FUNCTIONAL_object<PARAM,std::pair<LAZY_BOOLEAN,LAZY_BOOLEAN> > *f
		){
		_f = f->clone();
		}

	virtual void clear()
		{
		if (this->release_check()) return;
		_f->clear();
		delete this; 
		}

	LAZY_BOOLEAN eval(const PARAM &z){
		std::pair<LAZY_BOOLEAN,LAZY_BOOLEAN> r = _f->eval(z);
		return (r.first) && (r.second);
		}; 
};

template<class PARAM>
inline FUNCTION<PARAM,LAZY_BOOLEAN> LAZY_BOOLEAN_FUNCTION_and ( 
	const FUNCTION<PARAM,std::pair<LAZY_BOOLEAN,LAZY_BOOLEAN> > &a
	){
		return new FUNCTIONAL_and<PARAM>(a._fobject);
	};

template<class PARAM>
inline FUNCTION<PARAM,LAZY_BOOLEAN> operator && ( 
	const FUNCTION<PARAM,LAZY_BOOLEAN> &a,
	const FUNCTION<PARAM,LAZY_BOOLEAN> &b
	){
		return LAZY_BOOLEAN_FUNCTION_and(juxtaposition(a,b));
	};


template<class PARAM>
class FUNCTIONAL_or :public FUNCTIONAL_object<PARAM,LAZY_BOOLEAN>
{
public:
	FUNCTIONAL_object<PARAM,std::pair<LAZY_BOOLEAN,LAZY_BOOLEAN> > *_f;

	FUNCTIONAL_or(
		FUNCTIONAL_object<PARAM,std::pair<LAZY_BOOLEAN,LAZY_BOOLEAN> > *f
		){
		_f = f->clone();
		}

	virtual void clear()
		{
		if (this->release_check()) return;
		_f->clear();
		delete this; 
		}

	LAZY_BOOLEAN eval(const PARAM &z){
		std::pair<LAZY_BOOLEAN,LAZY_BOOLEAN> r = _f->eval(z);
		return (r.first) || (r.second);
		}; 
};

template<class PARAM>
inline FUNCTION<PARAM,LAZY_BOOLEAN> LAZY_BOOLEAN_FUNCTION_or ( 
	const FUNCTION<PARAM,std::pair<LAZY_BOOLEAN,LAZY_BOOLEAN> > &a
	){
		return new FUNCTIONAL_or<PARAM>(a._fobject);
	};

template<class PARAM>
inline FUNCTION<PARAM,LAZY_BOOLEAN> operator || ( 
	const FUNCTION<PARAM,LAZY_BOOLEAN> &a,
	const FUNCTION<PARAM,LAZY_BOOLEAN> &b
	){
		return LAZY_BOOLEAN_FUNCTION_or(juxtaposition(a,b));
	};

template<class PARAM>
class FUNCTIONAL_not :public FUNCTIONAL_object<PARAM,LAZY_BOOLEAN>
{
public:
	FUNCTIONAL_object<PARAM,LAZY_BOOLEAN> *_f;

	FUNCTIONAL_not(
		FUNCTIONAL_object<PARAM,LAZY_BOOLEAN> *f
		){
		_f = f->clone();
		}

	virtual void clear()
		{
		if (this->release_check()) return;
		_f->clear();
		delete this; 
		}

	LAZY_BOOLEAN eval(const PARAM &z){
		LAZY_BOOLEAN r = _f->eval(z);
		return !r;
		}; 
};

template<class PARAM>
inline FUNCTION<PARAM,LAZY_BOOLEAN> LAZY_BOOLEAN_FUNCTION_not ( 
	const FUNCTION<PARAM,LAZY_BOOLEAN> &a
	){
		return new FUNCTIONAL_not<PARAM>(a._fobject);
	};

template<class PARAM>
inline FUNCTION<PARAM,LAZY_BOOLEAN> operator ! ( 
	const FUNCTION<PARAM,LAZY_BOOLEAN> &a
	){
		return LAZY_BOOLEAN_FUNCTION_not(a);
	};

	
//********************************************************************************
// Special additional operators for functions
//********************************************************************************


//********************************************************************************
// general limit operator for FUNCTION objects on REAL numbers
//
// REAL limit ( FUNCTION<int,REAL> f )
// if FUNCTION f defines a normed Cauchy sequence, i.e. |f(i)-x|<= 2^{-i}
// then limit(f) returns x
//********************************************************************************

REAL limit ( FUNCTION<int,REAL> f );


} //namespace iRRAM
#endif

