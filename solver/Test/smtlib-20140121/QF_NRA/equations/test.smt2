(set-logic QF_NRA)
(set-info :source |

Vu Xuan Tung <tungvx@jaist.ac.jp>

|)
(set-info :smt-lib-version 2.0)
(set-info :category "crafted")
(set-info :status sat)
(declare-fun a () Real)
(declare-fun b () Real)
(assert (= (+ (* a a) (* b b)) 2))
(assert (= (* a b) 1))
(check-sat)
(exit)
