(set-logic QF_NRA)
(set-info :source |
From termination analysis of term rewriting.

Submitted by Harald Roman Zankl <Harald.Zankl@uibk.ac.at>

|)
(set-info :smt-lib-version 2.0)
(set-info :category "industrial")
(set-info :status unknown)
(declare-fun x6 () Real)
(declare-fun x23 () Real)
(declare-fun x13 () Real)
(declare-fun x30 () Real)
(declare-fun x3 () Real)
(declare-fun x20 () Real)
(declare-fun x10 () Real)
(declare-fun x27 () Real)
(declare-fun x0 () Real)
(declare-fun x17 () Real)
(declare-fun x7 () Real)
(declare-fun x24 () Real)
(declare-fun x14 () Real)
(declare-fun x31 () Real)
(declare-fun x4 () Real)
(declare-fun x21 () Real)
(declare-fun x11 () Real)
(declare-fun x28 () Real)
(declare-fun x1 () Real)
(declare-fun x18 () Real)
(declare-fun x8 () Real)
(declare-fun x25 () Real)
(declare-fun x15 () Real)
(declare-fun x5 () Real)
(declare-fun x22 () Real)
(declare-fun x12 () Real)
(declare-fun x29 () Real)
(declare-fun x2 () Real)
(declare-fun x19 () Real)
(declare-fun x9 () Real)
(declare-fun x26 () Real)
(declare-fun x16 () Real)
(assert (>= x6 0))
(assert (>= x23 0))
(assert (>= x13 0))
(assert (>= x30 0))
(assert (>= x3 0))
(assert (>= x20 0))
(assert (>= x10 0))
(assert (>= x27 0))
(assert (>= x0 0))
(assert (>= x17 0))
(assert (>= x7 0))
(assert (>= x24 0))
(assert (>= x14 0))
(assert (>= x31 0))
(assert (>= x4 0))
(assert (>= x21 0))
(assert (>= x11 0))
(assert (>= x28 0))
(assert (>= x1 0))
(assert (>= x18 0))
(assert (>= x8 0))
(assert (>= x25 0))
(assert (>= x15 0))
(assert (>= x5 0))
(assert (>= x22 0))
(assert (>= x12 0))
(assert (>= x29 0))
(assert (>= x2 0))
(assert (>= x19 0))
(assert (>= x9 0))
(assert (>= x26 0))
(assert (>= x16 0))
(assert (let ((?v_0 (+ x0 (+ (* x1 x3) (* x2 x4)))) (?v_3 (+ x14 (+ (* x16 x3) (* x17 x4)))) (?v_4 (+ x15 (+ (* x18 x3) (* x19 x4))))) (let ((?v_1 (+ x9 (+ (* x12 ?v_3) (* x13 ?v_4)))) (?v_6 (+ (* x16 x5) (* x17 x7))) (?v_7 (+ (* x18 x5) (* x19 x7))) (?v_8 (+ (* x16 x6) (* x17 x8))) (?v_9 (+ (* x18 x6) (* x19 x8)))) (let ((?v_10 (and (and (and (> x0 ?v_0) (>= x0 ?v_0)) (and (>= x1 (+ (* x1 x5) (* x2 x7))) (>= x2 (+ (* x1 x6) (* x2 x8))))) (and (and (> x0 ?v_1) (>= x0 ?v_1)) (and (>= x1 (+ x10 (+ (* x12 ?v_6) (* x13 ?v_7)))) (>= x2 (+ x11 (+ (* x12 ?v_8) (* x13 ?v_9)))))))) (?v_2 (+ x20 (+ (* x26 x20) (* x27 x21)))) (?v_5 (+ x20 (+ (* x26 ?v_3) (* x27 ?v_4))))) (and (and (and ?v_10 (and (> ?v_2 x30) (and (>= ?v_2 x30) (>= (+ x21 (+ (* x28 x20) (* x29 x21))) x31)))) (and (and (> x14 ?v_5) (and (>= x14 ?v_5) (>= x15 (+ x21 (+ (* x28 ?v_3) (* x29 ?v_4)))))) (and (and (and (>= x16 (+ x22 (+ (* x26 ?v_6) (* x27 ?v_7)))) (>= x17 (+ x23 (+ (* x26 ?v_8) (* x27 ?v_9))))) (>= x18 (+ x24 (+ (* x28 ?v_6) (* x29 ?v_7))))) (>= x19 (+ x25 (+ (* x28 ?v_8) (* x29 ?v_9))))))) ?v_10)))))
(check-sat)
(exit)
