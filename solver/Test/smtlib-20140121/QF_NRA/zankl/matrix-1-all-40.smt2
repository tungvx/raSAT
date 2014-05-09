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
(declare-fun x40 () Real)
(declare-fun x13 () Real)
(declare-fun x30 () Real)
(declare-fun x3 () Real)
(declare-fun x20 () Real)
(declare-fun x37 () Real)
(declare-fun x10 () Real)
(declare-fun x27 () Real)
(declare-fun x0 () Real)
(declare-fun x17 () Real)
(declare-fun x34 () Real)
(declare-fun x7 () Real)
(declare-fun x24 () Real)
(declare-fun x41 () Real)
(declare-fun x14 () Real)
(declare-fun x31 () Real)
(declare-fun x4 () Real)
(declare-fun x21 () Real)
(declare-fun x38 () Real)
(declare-fun x11 () Real)
(declare-fun x28 () Real)
(declare-fun x1 () Real)
(declare-fun x18 () Real)
(declare-fun x35 () Real)
(declare-fun x8 () Real)
(declare-fun x25 () Real)
(declare-fun x15 () Real)
(declare-fun x32 () Real)
(declare-fun x5 () Real)
(declare-fun x22 () Real)
(declare-fun x39 () Real)
(declare-fun x12 () Real)
(declare-fun x29 () Real)
(declare-fun x2 () Real)
(declare-fun x19 () Real)
(declare-fun x36 () Real)
(declare-fun x9 () Real)
(declare-fun x26 () Real)
(declare-fun x16 () Real)
(declare-fun x33 () Real)
(assert (>= x6 0))
(assert (>= x23 0))
(assert (>= x40 0))
(assert (>= x13 0))
(assert (>= x30 0))
(assert (>= x3 0))
(assert (>= x20 0))
(assert (>= x37 0))
(assert (>= x10 0))
(assert (>= x27 0))
(assert (>= x0 0))
(assert (>= x17 0))
(assert (>= x34 0))
(assert (>= x7 0))
(assert (>= x24 0))
(assert (>= x41 0))
(assert (>= x14 0))
(assert (>= x31 0))
(assert (>= x4 0))
(assert (>= x21 0))
(assert (>= x38 0))
(assert (>= x11 0))
(assert (>= x28 0))
(assert (>= x1 0))
(assert (>= x18 0))
(assert (>= x35 0))
(assert (>= x8 0))
(assert (>= x25 0))
(assert (>= x15 0))
(assert (>= x32 0))
(assert (>= x5 0))
(assert (>= x22 0))
(assert (>= x39 0))
(assert (>= x12 0))
(assert (>= x29 0))
(assert (>= x2 0))
(assert (>= x19 0))
(assert (>= x36 0))
(assert (>= x9 0))
(assert (>= x26 0))
(assert (>= x16 0))
(assert (>= x33 0))
(assert (let ((?v_0 (+ x0 (* x1 x2))) (?v_1 (+ x6 (* x7 x8))) (?v_33 (* x9 x14)) (?v_2 (* x12 x13))) (let ((?v_4 (+ (+ ?v_1 ?v_33) ?v_2)) (?v_34 (* x10 x11))) (let ((?v_3 (+ (+ ?v_1 ?v_34) ?v_2)) (?v_5 (+ x4 (* x5 x15))) (?v_7 (* x5 x16)) (?v_49 (+ x15 (* x16 x2)))) (let ((?v_6 (+ x0 (* x1 ?v_49))) (?v_52 (* x16 x3)) (?v_8 (+ x6 (* x7 x19))) (?v_40 (* x10 x21))) (let ((?v_10 (+ (+ ?v_8 ?v_40) ?v_2)) (?v_39 (* x9 x20))) (let ((?v_9 (+ (+ ?v_8 ?v_39) ?v_2)) (?v_11 (* x25 x13))) (let ((?v_13 (+ (+ x22 (* x23 x8)) ?v_11)) (?v_12 (+ (+ x22 (* x24 x14)) ?v_11)) (?v_14 (+ x6 ?v_2)) (?v_16 (+ (+ x7 x9) x10)) (?v_19 (+ x6 (* x7 x26)))) (let ((?v_15 (+ (+ (+ ?v_19 (* x9 x28)) (* x10 x28)) (* x12 x28))) (?v_22 (* x7 x27)) (?v_18 (+ (+ x22 (* x24 x20)) ?v_11)) (?v_17 (+ (+ x22 (* x23 x19)) ?v_11)) (?v_21 (+ x6 (* x12 x30))) (?v_20 (+ (+ (+ ?v_19 (* x9 x29)) (* x10 x29)) (* x12 x29))) (?v_23 (+ x22 ?v_11)) (?v_25 (+ x23 x24)) (?v_27 (+ x22 (* x23 x31)))) (let ((?v_24 (+ (+ ?v_27 (* x24 x28)) (* x25 x28))) (?v_30 (* x23 x32)) (?v_36 (+ x2 (* x3 x2)))) (let ((?v_26 (+ x17 (* x18 ?v_36))) (?v_32 (+ ?v_25 x25)) (?v_38 (* x3 x3)) (?v_29 (+ x22 (* x25 x30))) (?v_28 (+ (+ ?v_27 (* x24 x29)) (* x25 x29))) (?v_31 (+ x0 (* x1 x31))) (?v_42 (* x1 x32)) (?v_35 (+ (+ (+ x6 ?v_33) ?v_34) ?v_2)) (?v_70 (+ x2 (* x3 ?v_36)))) (let ((?v_37 (+ x17 (* x18 ?v_70))) (?v_43 (+ ?v_16 x12)) (?v_71 (* x3 ?v_38)) (?v_41 (+ (+ (+ x6 ?v_39) ?v_40) ?v_2))) (let ((?v_88 (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (> ?v_0 x4) (>= ?v_0 x4)) (>= (* x1 x3) x5)) (and (and (> ?v_3 ?v_4) (>= ?v_3 ?v_4)) (>= x9 x10))) (and (and (> ?v_5 x17) (>= ?v_5 x17)) (>= ?v_7 x18))) (and (and (> ?v_5 ?v_6) (>= ?v_5 ?v_6)) (>= ?v_7 (* x1 ?v_52)))) (and (and (> ?v_9 ?v_10) (>= ?v_9 ?v_10)) (>= x10 x9))) (and (and (> ?v_12 ?v_13) (>= ?v_12 ?v_13)) (>= x23 x24))) (and (and (> ?v_14 x4) (>= ?v_14 x4)) (>= ?v_16 x5))) (and (and (> ?v_14 ?v_15) (>= ?v_14 ?v_15)) (>= ?v_16 ?v_22))) (and (and (> ?v_17 ?v_18) (>= ?v_17 ?v_18)) (>= x24 x23))) (and (and (> ?v_20 ?v_21) (>= ?v_20 ?v_21)) (>= ?v_22 ?v_16))) (and (and (> ?v_23 x0) (>= ?v_23 x0)) (>= ?v_25 x1))) (and (and (> ?v_23 ?v_24) (>= ?v_23 ?v_24)) (>= ?v_25 ?v_30))) (and (and (> ?v_26 x22) (>= ?v_26 x22)) (>= (* x18 ?v_38) ?v_32))) (and (and (> ?v_28 ?v_29) (>= ?v_28 ?v_29)) (>= ?v_30 ?v_25))) (and (and (> ?v_31 x22) (>= ?v_31 x22)) (>= ?v_42 ?v_32))) (and (and (> ?v_35 ?v_3) (>= ?v_35 ?v_3)) (>= x7 x9))) (and (and (> ?v_37 x6) (>= ?v_37 x6)) (>= (* x18 ?v_71) ?v_43))) (and (and (> ?v_10 ?v_41) (>= ?v_10 ?v_41)) (>= x9 x7))) (and (and (> ?v_31 x6) (>= ?v_31 x6)) (>= ?v_42 ?v_43)))) (?v_44 (+ x31 (* x32 x2))) (?v_45 (+ x33 (* x34 x8))) (?v_79 (* x35 x14)) (?v_46 (* x37 x13))) (let ((?v_48 (+ (+ ?v_45 ?v_79) ?v_46)) (?v_80 (* x36 x11))) (let ((?v_47 (+ (+ ?v_45 ?v_80) ?v_46)) (?v_51 (+ x31 (* x32 ?v_49))) (?v_50 (+ x26 (* x27 x15))) (?v_53 (+ x33 (* x34 x19))) (?v_84 (* x36 x21))) (let ((?v_55 (+ (+ ?v_53 ?v_84) ?v_46)) (?v_83 (* x35 x20))) (let ((?v_54 (+ (+ ?v_53 ?v_83) ?v_46)) (?v_56 (* x41 x13))) (let ((?v_58 (+ (+ x38 (* x39 x8)) ?v_56)) (?v_57 (+ (+ x38 (* x40 x14)) ?v_56)) (?v_63 (+ x33 (* x34 x26)))) (let ((?v_60 (+ (+ (+ ?v_63 (* x35 x28)) (* x36 x28)) (* x37 x28))) (?v_59 (+ x33 ?v_46)) (?v_66 (* x34 x27)) (?v_67 (+ (+ x34 x35) x36)) (?v_62 (+ (+ x38 (* x40 x20)) ?v_56)) (?v_61 (+ (+ x38 (* x39 x19)) ?v_56)) (?v_65 (+ x33 (* x37 x30))) (?v_64 (+ (+ (+ ?v_63 (* x35 x29)) (* x36 x29)) (* x37 x29))) (?v_73 (+ x38 (* x39 x31)))) (let ((?v_69 (+ (+ ?v_73 (* x40 x28)) (* x41 x28))) (?v_68 (+ x38 ?v_56)) (?v_76 (* x39 x32)) (?v_72 (+ x39 x40))) (let ((?v_78 (+ ?v_72 x41)) (?v_75 (+ x38 (* x41 x30))) (?v_74 (+ (+ ?v_73 (* x40 x29)) (* x41 x29))) (?v_77 (+ x31 (* x32 x31))) (?v_86 (* x32 x32)) (?v_81 (+ (+ (+ x33 ?v_79) ?v_80) ?v_46)) (?v_82 (+ x2 (* x3 ?v_70))) (?v_87 (+ ?v_67 x37)) (?v_85 (+ (+ (+ x33 ?v_83) ?v_84) ?v_46))) (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and ?v_88 (and (and (> ?v_44 x26) (>= ?v_44 x26)) (>= (* x32 x3) x27))) (and (and (> ?v_47 ?v_48) (>= ?v_47 ?v_48)) (>= x35 x36))) (and (and (> ?v_50 ?v_51) (>= ?v_50 ?v_51)) (>= (* x27 x16) (* x32 ?v_52)))) (and (and (> ?v_54 ?v_55) (>= ?v_54 ?v_55)) (>= x36 x35))) (and (and (> ?v_57 ?v_58) (>= ?v_57 ?v_58)) (>= x39 x40))) (and (and (> ?v_59 ?v_60) (>= ?v_59 ?v_60)) (>= ?v_67 ?v_66))) (and (and (> ?v_61 ?v_62) (>= ?v_61 ?v_62)) (>= x40 x39))) (and (and (> ?v_64 ?v_65) (>= ?v_64 ?v_65)) (>= ?v_66 ?v_67))) (and (and (> ?v_68 ?v_69) (>= ?v_68 ?v_69)) (>= ?v_72 ?v_76))) (and (and (> ?v_70 x38) (>= ?v_70 x38)) (>= ?v_71 ?v_78))) (and (and (> ?v_74 ?v_75) (>= ?v_74 ?v_75)) (>= ?v_76 ?v_72))) (and (and (> ?v_77 x38) (>= ?v_77 x38)) (>= ?v_86 ?v_78))) (and (and (> ?v_81 ?v_47) (>= ?v_81 ?v_47)) (>= x34 x35))) (and (and (> ?v_82 x33) (>= ?v_82 x33)) (>= (* x3 ?v_71) ?v_87))) (and (and (> ?v_55 ?v_85) (>= ?v_55 ?v_85)) (>= x35 x34))) (and (and (> ?v_77 x33) (>= ?v_77 x33)) (>= ?v_86 ?v_87))) ?v_88))))))))))))))))))))))
(check-sat)
(exit)
