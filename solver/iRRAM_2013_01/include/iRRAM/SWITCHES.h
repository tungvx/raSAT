/*

iRRAM/SWITCHES.h -- header file for SWITCHES class of the iRRAM library
 
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
/****************************************************************************/
// Definition of switches
//
// The iRRAM uses "switches" that start with a default behaviour.
// Their value can be changed by declaring a variable of the corresponding type.
// This change is effective until an other variable of the type is declared or the
// variable is destroyed.
// With the end of the lifetime of a variable, the previous behaviour will be restored.
//
// Such switches exist for:
//   implicit change of the behaviour of LAZY_BOOLEAN comparisons (BOTTOM <-> UNCERTAIN)
//   temporary increase/decrease of the working precision
//   explicit declaration of continuous behaviour of a code section
//   implicit declaration of the working precision of DYADIC operators
//
// The switches are thread-specific.
//
/****************************************************************************/

namespace iRRAM {


class continuous_block
{
int saved;
public:
  inline continuous_block(){ cerr << "Entering continuous section\n"; saved=ACTUAL_STACK.inlimit++; }
  inline ~continuous_block(){ cerr << "Leaving continuous section\n";  ACTUAL_STACK.inlimit=saved; }
}



extern __thread  bool iRRAM_COMPARE_exact;
extern __thread  int  iRRAM_COMPARE_precision;



class comparison_uncertainty
{
public:
 comparison_uncertainty(int p){
   precision=iRRAM_COMPARE_precision;
   iRRAM_COMPARE_precision=p;
   exact=iRRAM_COMPARE_exact;
   iRRAM_COMPARE_exact=false;
  }
  
 comparison_uncertainty(){
   precision=iRRAM_COMPARE_precision;
   exact=iRRAM_COMPARE_exact;
   iRRAM_COMPARE_exact=true;
  }
  
 ~comparison_uncertainty(){
   iRRAM_COMPARE_precision=precision;
   iRRAM_COMPARE_exact=exact;
  };
private:
int precision;
bool exact;
};



inline bool comparison_is_exact(){return iRRAM_COMPARE_exact;};

inline int compare_precision(){return iRRAM_COMPARE_precision;};




} // namespace iRRAM
