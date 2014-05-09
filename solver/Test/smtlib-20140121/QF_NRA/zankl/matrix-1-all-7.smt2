(set-logic QF_NRA)
(set-info :source |
From termination analysis of term rewriting.

Submitted by Harald Roman Zankl <Harald.Zankl@uibk.ac.at>

|)
(set-info :smt-lib-version 2.0)
(set-info :category "industrial")
(set-info :status sat)
(declare-fun x6 () Real)
(declare-fun x13 () Real)
(declare-fun x3 () Real)
(declare-fun x20 () Real)
(declare-fun x10 () Real)
(declare-fun x0 () Real)
(declare-fun x17 () Real)
(declare-fun x7 () Real)
(declare-fun x14 () Real)
(declare-fun x4 () Real)
(declare-fun x21 () Real)
(declare-fun x11 () Real)
(declare-fun x1 () Real)
(declare-fun x18 () Real)
(declare-fun x8 () Real)
(declare-fun x15 () Real)
(declare-fun x5 () Real)
(declare-fun x22 () Real)
(declare-fun x12 () Real)
(declare-fun x2 () Real)
(declare-fun x19 () Real)
(declare-fun x9 () Real)
(declare-fun x16 () Real)
(assert (>= x6 0))
(assert (>= x13 0))
(assert (>= x3 0))
(assert (>= x20 0))
(assert (>= x10 0))
(assert (>= x0 0))
(assert (>= x17 0))
(assert (>= x7 0))
(assert (>= x14 0))
(assert (>= x4 0))
(assert (>= x21 0))
(assert (>= x11 0))
(assert (>= x1 0))
(assert (>= x18 0))
(assert (>= x8 0))
(assert (>= x15 0))
(assert (>= x5 0))
(assert (>= x22 0))
(assert (>= x12 0))
(assert (>= x2 0))
(assert (>= x19 0))
(assert (>= x9 0))
(assert (>= x16 0))
(assert (let ((?v_0 (+ x0 (* x1 x2))) (?v_1 (+ (+ x0 (* x1 x10)) (* x3 x13))) (?v_2 (+ (+ x7 (* x8 x15)) (* x9 x15)))) (let ((?v_11 (and (and (and (and (and (> ?v_0 x5) (>= ?v_0 x5)) (>= x3 x6)) (and (and (and (> ?v_0 x7) (>= ?v_0 x7)) (>= x3 x8)) (>= x4 x9))) (and (and (and (> ?v_0 ?v_1) (>= ?v_0 ?v_1)) (>= x3 (+ (* x1 x11) (* x3 x14)))) (>= x4 (+ (* x1 x12) x4)))) (and (and (and (> ?v_2 x7) (>= ?v_2 x7)) (>= (* x8 x16) x8)) (>= (* x9 x16) x9)))) (?v_4 (+ (+ x17 (* x18 x10)) (* x19 x13))) (?v_3 (+ x17 (* x18 x2))) (?v_5 (+ x10 (* x11 x21))) (?v_7 (+ x10 (* x11 x15)))) (let ((?v_6 (+ ?v_7 (* x12 x21))) (?v_8 (+ ?v_7 (* x12 x15))) (?v_9 (+ x13 (* x14 x21))) (?v_10 (+ x13 (* x14 x15)))) (and (and (and (and (and (and (and ?v_11 (and (and (and (> ?v_3 ?v_4) (>= ?v_3 ?v_4)) (>= x19 (+ (* x18 x11) (* x19 x14)))) (>= x20 (+ (* x18 x12) x20)))) (and (> ?v_5 x22) (>= ?v_5 x22))) (and (> ?v_6 x2) (>= ?v_6 x2))) (and (and (and (> ?v_8 x10) (>= ?v_8 x10)) (>= (* x11 x16) x11)) (>= (* x12 x16) x12))) (and (> ?v_9 x21) (>= ?v_9 x21))) (and (and (> ?v_10 0) (>= ?v_10 0)) (>= (* x14 x16) 1))) ?v_11)))))
(check-sat)
(exit)
