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
(declare-fun x57 () Real)
(declare-fun x13 () Real)
(declare-fun x30 () Real)
(declare-fun x47 () Real)
(declare-fun x64 () Real)
(declare-fun x3 () Real)
(declare-fun x20 () Real)
(declare-fun x37 () Real)
(declare-fun x54 () Real)
(declare-fun x71 () Real)
(declare-fun x10 () Real)
(declare-fun x27 () Real)
(declare-fun x44 () Real)
(declare-fun x61 () Real)
(declare-fun x0 () Real)
(declare-fun x17 () Real)
(declare-fun x34 () Real)
(declare-fun x51 () Real)
(declare-fun x68 () Real)
(declare-fun x7 () Real)
(declare-fun x24 () Real)
(declare-fun x41 () Real)
(declare-fun x58 () Real)
(declare-fun x14 () Real)
(declare-fun x31 () Real)
(declare-fun x48 () Real)
(declare-fun x65 () Real)
(declare-fun x4 () Real)
(declare-fun x21 () Real)
(declare-fun x38 () Real)
(declare-fun x55 () Real)
(declare-fun x72 () Real)
(declare-fun x11 () Real)
(declare-fun x28 () Real)
(declare-fun x45 () Real)
(declare-fun x62 () Real)
(declare-fun x1 () Real)
(declare-fun x18 () Real)
(declare-fun x35 () Real)
(declare-fun x52 () Real)
(declare-fun x69 () Real)
(declare-fun x8 () Real)
(declare-fun x25 () Real)
(declare-fun x42 () Real)
(declare-fun x59 () Real)
(declare-fun x15 () Real)
(declare-fun x32 () Real)
(declare-fun x49 () Real)
(declare-fun x66 () Real)
(declare-fun x5 () Real)
(declare-fun x22 () Real)
(declare-fun x39 () Real)
(declare-fun x56 () Real)
(declare-fun x73 () Real)
(declare-fun x12 () Real)
(declare-fun x29 () Real)
(declare-fun x46 () Real)
(declare-fun x63 () Real)
(declare-fun x2 () Real)
(declare-fun x19 () Real)
(declare-fun x36 () Real)
(declare-fun x53 () Real)
(declare-fun x70 () Real)
(declare-fun x9 () Real)
(declare-fun x26 () Real)
(declare-fun x43 () Real)
(declare-fun x60 () Real)
(declare-fun x16 () Real)
(declare-fun x33 () Real)
(declare-fun x50 () Real)
(declare-fun x67 () Real)
(assert (>= x6 0))
(assert (>= x23 0))
(assert (>= x40 0))
(assert (>= x57 0))
(assert (>= x13 0))
(assert (>= x30 0))
(assert (>= x47 0))
(assert (>= x64 0))
(assert (>= x3 0))
(assert (>= x20 0))
(assert (>= x37 0))
(assert (>= x54 0))
(assert (>= x71 0))
(assert (>= x10 0))
(assert (>= x27 0))
(assert (>= x44 0))
(assert (>= x61 0))
(assert (>= x0 0))
(assert (>= x17 0))
(assert (>= x34 0))
(assert (>= x51 0))
(assert (>= x68 0))
(assert (>= x7 0))
(assert (>= x24 0))
(assert (>= x41 0))
(assert (>= x58 0))
(assert (>= x14 0))
(assert (>= x31 0))
(assert (>= x48 0))
(assert (>= x65 0))
(assert (>= x4 0))
(assert (>= x21 0))
(assert (>= x38 0))
(assert (>= x55 0))
(assert (>= x72 0))
(assert (>= x11 0))
(assert (>= x28 0))
(assert (>= x45 0))
(assert (>= x62 0))
(assert (>= x1 0))
(assert (>= x18 0))
(assert (>= x35 0))
(assert (>= x52 0))
(assert (>= x69 0))
(assert (>= x8 0))
(assert (>= x25 0))
(assert (>= x42 0))
(assert (>= x59 0))
(assert (>= x15 0))
(assert (>= x32 0))
(assert (>= x49 0))
(assert (>= x66 0))
(assert (>= x5 0))
(assert (>= x22 0))
(assert (>= x39 0))
(assert (>= x56 0))
(assert (>= x73 0))
(assert (>= x12 0))
(assert (>= x29 0))
(assert (>= x46 0))
(assert (>= x63 0))
(assert (>= x2 0))
(assert (>= x19 0))
(assert (>= x36 0))
(assert (>= x53 0))
(assert (>= x70 0))
(assert (>= x9 0))
(assert (>= x26 0))
(assert (>= x43 0))
(assert (>= x60 0))
(assert (>= x16 0))
(assert (>= x33 0))
(assert (>= x50 0))
(assert (>= x67 0))
(assert (let ((?v_14 (+ x5 (+ (+ (+ (* x9 x5) (* x10 x6)) (* x11 x7)) (* x12 x8)))) (?v_15 (+ x6 (+ (+ (+ (* x13 x5) (* x14 x6)) (* x15 x7)) (* x16 x8)))) (?v_16 (+ x7 (+ (+ (+ (* x17 x5) (* x18 x6)) (* x19 x7)) (* x20 x8)))) (?v_17 (+ x8 (+ (+ (+ (* x21 x5) (* x22 x6)) (* x23 x7)) (* x24 x8))))) (let ((?v_0 (+ x0 (+ (+ (+ (* x1 ?v_14) (* x2 ?v_15)) (* x3 ?v_16)) (* x4 ?v_17)))) (?v_20 (+ (+ (+ (* x9 x9) (* x10 x13)) (* x11 x17)) (* x12 x21))) (?v_21 (+ (+ (+ (* x13 x9) (* x14 x13)) (* x15 x17)) (* x16 x21))) (?v_22 (+ (+ (+ (* x17 x9) (* x18 x13)) (* x19 x17)) (* x20 x21))) (?v_23 (+ (+ (+ (* x21 x9) (* x22 x13)) (* x23 x17)) (* x24 x21))) (?v_24 (+ (+ (+ (* x9 x10) (* x10 x14)) (* x11 x18)) (* x12 x22))) (?v_25 (+ (+ (+ (* x13 x10) (* x14 x14)) (* x15 x18)) (* x16 x22))) (?v_26 (+ (+ (+ (* x17 x10) (* x18 x14)) (* x19 x18)) (* x20 x22))) (?v_27 (+ (+ (+ (* x21 x10) (* x22 x14)) (* x23 x18)) (* x24 x22))) (?v_28 (+ (+ (+ (* x9 x11) (* x10 x15)) (* x11 x19)) (* x12 x23))) (?v_29 (+ (+ (+ (* x13 x11) (* x14 x15)) (* x15 x19)) (* x16 x23))) (?v_30 (+ (+ (+ (* x17 x11) (* x18 x15)) (* x19 x19)) (* x20 x23))) (?v_31 (+ (+ (+ (* x21 x11) (* x22 x15)) (* x23 x19)) (* x24 x23))) (?v_32 (+ (+ (+ (* x9 x12) (* x10 x16)) (* x11 x20)) (* x12 x24))) (?v_33 (+ (+ (+ (* x13 x12) (* x14 x16)) (* x15 x20)) (* x16 x24))) (?v_34 (+ (+ (+ (* x17 x12) (* x18 x16)) (* x19 x20)) (* x20 x24))) (?v_35 (+ (+ (+ (* x21 x12) (* x22 x16)) (* x23 x20)) (* x24 x24))) (?v_2 (+ x0 (+ (+ (+ (* x1 x5) (* x2 x6)) (* x3 x7)) (* x4 x8)))) (?v_1 (+ x25 (+ (+ (+ (* x26 x5) (* x27 x6)) (* x28 x7)) (* x29 x8)))) (?v_4 (+ (+ (+ (* x26 x9) (* x27 x13)) (* x28 x17)) (* x29 x21))) (?v_5 (+ (+ (+ (* x26 x10) (* x27 x14)) (* x28 x18)) (* x29 x22))) (?v_6 (+ (+ (+ (* x26 x11) (* x27 x15)) (* x28 x19)) (* x29 x23))) (?v_7 (+ (+ (+ (* x26 x12) (* x27 x16)) (* x28 x20)) (* x29 x24))) (?v_37 (+ x30 (+ (+ (+ (* x34 x5) (* x35 x6)) (* x36 x7)) (* x37 x8)))) (?v_38 (+ x31 (+ (+ (+ (* x38 x5) (* x39 x6)) (* x40 x7)) (* x41 x8)))) (?v_39 (+ x32 (+ (+ (+ (* x42 x5) (* x43 x6)) (* x44 x7)) (* x45 x8)))) (?v_40 (+ x33 (+ (+ (+ (* x46 x5) (* x47 x6)) (* x48 x7)) (* x49 x8))))) (let ((?v_3 (+ x25 (+ (+ (+ (* x26 ?v_37) (* x27 ?v_38)) (* x28 ?v_39)) (* x29 ?v_40)))) (?v_47 (+ (+ (+ (* x34 x9) (* x35 x13)) (* x36 x17)) (* x37 x21))) (?v_48 (+ (+ (+ (* x38 x9) (* x39 x13)) (* x40 x17)) (* x41 x21))) (?v_49 (+ (+ (+ (* x42 x9) (* x43 x13)) (* x44 x17)) (* x45 x21))) (?v_50 (+ (+ (+ (* x46 x9) (* x47 x13)) (* x48 x17)) (* x49 x21))) (?v_51 (+ (+ (+ (* x34 x10) (* x35 x14)) (* x36 x18)) (* x37 x22))) (?v_52 (+ (+ (+ (* x38 x10) (* x39 x14)) (* x40 x18)) (* x41 x22))) (?v_53 (+ (+ (+ (* x42 x10) (* x43 x14)) (* x44 x18)) (* x45 x22))) (?v_54 (+ (+ (+ (* x46 x10) (* x47 x14)) (* x48 x18)) (* x49 x22))) (?v_55 (+ (+ (+ (* x34 x11) (* x35 x15)) (* x36 x19)) (* x37 x23))) (?v_56 (+ (+ (+ (* x38 x11) (* x39 x15)) (* x40 x19)) (* x41 x23))) (?v_57 (+ (+ (+ (* x42 x11) (* x43 x15)) (* x44 x19)) (* x45 x23))) (?v_58 (+ (+ (+ (* x46 x11) (* x47 x15)) (* x48 x19)) (* x49 x23))) (?v_59 (+ (+ (+ (* x34 x12) (* x35 x16)) (* x36 x20)) (* x37 x24))) (?v_60 (+ (+ (+ (* x38 x12) (* x39 x16)) (* x40 x20)) (* x41 x24))) (?v_61 (+ (+ (+ (* x42 x12) (* x43 x16)) (* x44 x20)) (* x45 x24))) (?v_62 (+ (+ (+ (* x46 x12) (* x47 x16)) (* x48 x20)) (* x49 x24)))) (let ((?v_79 (and (and (and (and (> ?v_0 x0) (>= ?v_0 x0)) (and (and (and (>= (+ (+ (+ (* x1 ?v_20) (* x2 ?v_21)) (* x3 ?v_22)) (* x4 ?v_23)) x1) (>= (+ (+ (+ (* x1 ?v_24) (* x2 ?v_25)) (* x3 ?v_26)) (* x4 ?v_27)) x2)) (>= (+ (+ (+ (* x1 ?v_28) (* x2 ?v_29)) (* x3 ?v_30)) (* x4 ?v_31)) x3)) (>= (+ (+ (+ (* x1 ?v_32) (* x2 ?v_33)) (* x3 ?v_34)) (* x4 ?v_35)) x4))) (and (and (> ?v_1 ?v_2) (>= ?v_1 ?v_2)) (and (and (and (>= ?v_4 (+ (+ (+ (* x1 x9) (* x2 x13)) (* x3 x17)) (* x4 x21))) (>= ?v_5 (+ (+ (+ (* x1 x10) (* x2 x14)) (* x3 x18)) (* x4 x22)))) (>= ?v_6 (+ (+ (+ (* x1 x11) (* x2 x15)) (* x3 x19)) (* x4 x23)))) (>= ?v_7 (+ (+ (+ (* x1 x12) (* x2 x16)) (* x3 x20)) (* x4 x24)))))) (and (and (> ?v_1 ?v_3) (>= ?v_1 ?v_3)) (and (and (and (>= ?v_4 (+ (+ (+ (* x26 ?v_47) (* x27 ?v_48)) (* x28 ?v_49)) (* x29 ?v_50))) (>= ?v_5 (+ (+ (+ (* x26 ?v_51) (* x27 ?v_52)) (* x28 ?v_53)) (* x29 ?v_54)))) (>= ?v_6 (+ (+ (+ (* x26 ?v_55) (* x27 ?v_56)) (* x28 ?v_57)) (* x29 ?v_58)))) (>= ?v_7 (+ (+ (+ (* x26 ?v_59) (* x27 ?v_60)) (* x28 ?v_61)) (* x29 ?v_62))))))) (?v_8 (+ x30 (+ (+ (+ (* x34 x50) (* x35 x51)) (* x36 x52)) (* x37 x53)))) (?v_10 (+ x5 (+ (+ (+ (* x9 x50) (* x10 x51)) (* x11 x52)) (* x12 x53)))) (?v_11 (+ x6 (+ (+ (+ (* x13 x50) (* x14 x51)) (* x15 x52)) (* x16 x53)))) (?v_12 (+ x7 (+ (+ (+ (* x17 x50) (* x18 x51)) (* x19 x52)) (* x20 x53)))) (?v_13 (+ x8 (+ (+ (+ (* x21 x50) (* x22 x51)) (* x23 x52)) (* x24 x53))))) (let ((?v_9 (+ x30 (+ (+ (+ (* x34 ?v_10) (* x35 ?v_11)) (* x36 ?v_12)) (* x37 ?v_13)))) (?v_19 (+ x5 (+ (+ (+ (* x9 x30) (* x10 x31)) (* x11 x32)) (* x12 x33)))) (?v_18 (+ x30 (+ (+ (+ (* x34 ?v_14) (* x35 ?v_15)) (* x36 ?v_16)) (* x37 ?v_17)))) (?v_36 (+ x54 (+ (+ (+ (* x58 x50) (* x59 x51)) (* x60 x52)) (* x61 x53)))) (?v_43 (+ x54 (+ (+ (+ (* x58 ?v_37) (* x59 ?v_38)) (* x60 ?v_39)) (* x61 ?v_40)))) (?v_44 (+ x55 (+ (+ (+ (* x62 ?v_37) (* x63 ?v_38)) (* x64 ?v_39)) (* x65 ?v_40)))) (?v_45 (+ x56 (+ (+ (+ (* x66 ?v_37) (* x67 ?v_38)) (* x68 ?v_39)) (* x69 ?v_40)))) (?v_46 (+ x57 (+ (+ (+ (* x70 ?v_37) (* x71 ?v_38)) (* x72 ?v_39)) (* x73 ?v_40))))) (let ((?v_42 (+ x5 (+ (+ (+ (* x9 ?v_43) (* x10 ?v_44)) (* x11 ?v_45)) (* x12 ?v_46)))) (?v_41 (+ x54 (+ (+ (+ (* x58 x5) (* x59 x6)) (* x60 x7)) (* x61 x8)))) (?v_63 (+ (+ (+ (* x58 ?v_47) (* x59 ?v_48)) (* x60 ?v_49)) (* x61 ?v_50))) (?v_64 (+ (+ (+ (* x62 ?v_47) (* x63 ?v_48)) (* x64 ?v_49)) (* x65 ?v_50))) (?v_65 (+ (+ (+ (* x66 ?v_47) (* x67 ?v_48)) (* x68 ?v_49)) (* x69 ?v_50))) (?v_66 (+ (+ (+ (* x70 ?v_47) (* x71 ?v_48)) (* x72 ?v_49)) (* x73 ?v_50))) (?v_67 (+ (+ (+ (* x58 ?v_51) (* x59 ?v_52)) (* x60 ?v_53)) (* x61 ?v_54))) (?v_68 (+ (+ (+ (* x62 ?v_51) (* x63 ?v_52)) (* x64 ?v_53)) (* x65 ?v_54))) (?v_69 (+ (+ (+ (* x66 ?v_51) (* x67 ?v_52)) (* x68 ?v_53)) (* x69 ?v_54))) (?v_70 (+ (+ (+ (* x70 ?v_51) (* x71 ?v_52)) (* x72 ?v_53)) (* x73 ?v_54))) (?v_71 (+ (+ (+ (* x58 ?v_55) (* x59 ?v_56)) (* x60 ?v_57)) (* x61 ?v_58))) (?v_72 (+ (+ (+ (* x62 ?v_55) (* x63 ?v_56)) (* x64 ?v_57)) (* x65 ?v_58))) (?v_73 (+ (+ (+ (* x66 ?v_55) (* x67 ?v_56)) (* x68 ?v_57)) (* x69 ?v_58))) (?v_74 (+ (+ (+ (* x70 ?v_55) (* x71 ?v_56)) (* x72 ?v_57)) (* x73 ?v_58))) (?v_75 (+ (+ (+ (* x58 ?v_59) (* x59 ?v_60)) (* x60 ?v_61)) (* x61 ?v_62))) (?v_76 (+ (+ (+ (* x62 ?v_59) (* x63 ?v_60)) (* x64 ?v_61)) (* x65 ?v_62))) (?v_77 (+ (+ (+ (* x66 ?v_59) (* x67 ?v_60)) (* x68 ?v_61)) (* x69 ?v_62))) (?v_78 (+ (+ (+ (* x70 ?v_59) (* x71 ?v_60)) (* x72 ?v_61)) (* x73 ?v_62)))) (and (and (and (and (and (and ?v_79 (and (> ?v_8 x50) (and (and (and (>= ?v_8 x50) (>= (+ x31 (+ (+ (+ (* x38 x50) (* x39 x51)) (* x40 x52)) (* x41 x53))) x51)) (>= (+ x32 (+ (+ (+ (* x42 x50) (* x43 x51)) (* x44 x52)) (* x45 x53))) x52)) (>= (+ x33 (+ (+ (+ (* x46 x50) (* x47 x51)) (* x48 x52)) (* x49 x53))) x53)))) (and (> ?v_9 x50) (and (and (and (>= ?v_9 x50) (>= (+ x31 (+ (+ (+ (* x38 ?v_10) (* x39 ?v_11)) (* x40 ?v_12)) (* x41 ?v_13))) x51)) (>= (+ x32 (+ (+ (+ (* x42 ?v_10) (* x43 ?v_11)) (* x44 ?v_12)) (* x45 ?v_13))) x52)) (>= (+ x33 (+ (+ (+ (* x46 ?v_10) (* x47 ?v_11)) (* x48 ?v_12)) (* x49 ?v_13))) x53)))) (and (and (> ?v_18 ?v_19) (and (and (and (>= ?v_18 ?v_19) (>= (+ x31 (+ (+ (+ (* x38 ?v_14) (* x39 ?v_15)) (* x40 ?v_16)) (* x41 ?v_17))) (+ x6 (+ (+ (+ (* x13 x30) (* x14 x31)) (* x15 x32)) (* x16 x33))))) (>= (+ x32 (+ (+ (+ (* x42 ?v_14) (* x43 ?v_15)) (* x44 ?v_16)) (* x45 ?v_17))) (+ x7 (+ (+ (+ (* x17 x30) (* x18 x31)) (* x19 x32)) (* x20 x33))))) (>= (+ x33 (+ (+ (+ (* x46 ?v_14) (* x47 ?v_15)) (* x48 ?v_16)) (* x49 ?v_17))) (+ x8 (+ (+ (+ (* x21 x30) (* x22 x31)) (* x23 x32)) (* x24 x33)))))) (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (>= (+ (+ (+ (* x34 ?v_20) (* x35 ?v_21)) (* x36 ?v_22)) (* x37 ?v_23)) (+ (+ (+ (* x9 x34) (* x10 x38)) (* x11 x42)) (* x12 x46))) (>= (+ (+ (+ (* x34 ?v_24) (* x35 ?v_25)) (* x36 ?v_26)) (* x37 ?v_27)) (+ (+ (+ (* x9 x35) (* x10 x39)) (* x11 x43)) (* x12 x47)))) (>= (+ (+ (+ (* x34 ?v_28) (* x35 ?v_29)) (* x36 ?v_30)) (* x37 ?v_31)) (+ (+ (+ (* x9 x36) (* x10 x40)) (* x11 x44)) (* x12 x48)))) (>= (+ (+ (+ (* x34 ?v_32) (* x35 ?v_33)) (* x36 ?v_34)) (* x37 ?v_35)) (+ (+ (+ (* x9 x37) (* x10 x41)) (* x11 x45)) (* x12 x49)))) (>= (+ (+ (+ (* x38 ?v_20) (* x39 ?v_21)) (* x40 ?v_22)) (* x41 ?v_23)) (+ (+ (+ (* x13 x34) (* x14 x38)) (* x15 x42)) (* x16 x46)))) (>= (+ (+ (+ (* x38 ?v_24) (* x39 ?v_25)) (* x40 ?v_26)) (* x41 ?v_27)) (+ (+ (+ (* x13 x35) (* x14 x39)) (* x15 x43)) (* x16 x47)))) (>= (+ (+ (+ (* x38 ?v_28) (* x39 ?v_29)) (* x40 ?v_30)) (* x41 ?v_31)) (+ (+ (+ (* x13 x36) (* x14 x40)) (* x15 x44)) (* x16 x48)))) (>= (+ (+ (+ (* x38 ?v_32) (* x39 ?v_33)) (* x40 ?v_34)) (* x41 ?v_35)) (+ (+ (+ (* x13 x37) (* x14 x41)) (* x15 x45)) (* x16 x49)))) (>= (+ (+ (+ (* x42 ?v_20) (* x43 ?v_21)) (* x44 ?v_22)) (* x45 ?v_23)) (+ (+ (+ (* x17 x34) (* x18 x38)) (* x19 x42)) (* x20 x46)))) (>= (+ (+ (+ (* x42 ?v_24) (* x43 ?v_25)) (* x44 ?v_26)) (* x45 ?v_27)) (+ (+ (+ (* x17 x35) (* x18 x39)) (* x19 x43)) (* x20 x47)))) (>= (+ (+ (+ (* x42 ?v_28) (* x43 ?v_29)) (* x44 ?v_30)) (* x45 ?v_31)) (+ (+ (+ (* x17 x36) (* x18 x40)) (* x19 x44)) (* x20 x48)))) (>= (+ (+ (+ (* x42 ?v_32) (* x43 ?v_33)) (* x44 ?v_34)) (* x45 ?v_35)) (+ (+ (+ (* x17 x37) (* x18 x41)) (* x19 x45)) (* x20 x49)))) (>= (+ (+ (+ (* x46 ?v_20) (* x47 ?v_21)) (* x48 ?v_22)) (* x49 ?v_23)) (+ (+ (+ (* x21 x34) (* x22 x38)) (* x23 x42)) (* x24 x46)))) (>= (+ (+ (+ (* x46 ?v_24) (* x47 ?v_25)) (* x48 ?v_26)) (* x49 ?v_27)) (+ (+ (+ (* x21 x35) (* x22 x39)) (* x23 x43)) (* x24 x47)))) (>= (+ (+ (+ (* x46 ?v_28) (* x47 ?v_29)) (* x48 ?v_30)) (* x49 ?v_31)) (+ (+ (+ (* x21 x36) (* x22 x40)) (* x23 x44)) (* x24 x48)))) (>= (+ (+ (+ (* x46 ?v_32) (* x47 ?v_33)) (* x48 ?v_34)) (* x49 ?v_35)) (+ (+ (+ (* x21 x37) (* x22 x41)) (* x23 x45)) (* x24 x49)))))) (and (> ?v_36 x50) (and (and (and (>= ?v_36 x50) (>= (+ x55 (+ (+ (+ (* x62 x50) (* x63 x51)) (* x64 x52)) (* x65 x53))) x51)) (>= (+ x56 (+ (+ (+ (* x66 x50) (* x67 x51)) (* x68 x52)) (* x69 x53))) x52)) (>= (+ x57 (+ (+ (+ (* x70 x50) (* x71 x51)) (* x72 x52)) (* x73 x53))) x53)))) (and (and (> ?v_41 ?v_42) (and (and (and (>= ?v_41 ?v_42) (>= (+ x55 (+ (+ (+ (* x62 x5) (* x63 x6)) (* x64 x7)) (* x65 x8))) (+ x6 (+ (+ (+ (* x13 ?v_43) (* x14 ?v_44)) (* x15 ?v_45)) (* x16 ?v_46))))) (>= (+ x56 (+ (+ (+ (* x66 x5) (* x67 x6)) (* x68 x7)) (* x69 x8))) (+ x7 (+ (+ (+ (* x17 ?v_43) (* x18 ?v_44)) (* x19 ?v_45)) (* x20 ?v_46))))) (>= (+ x57 (+ (+ (+ (* x70 x5) (* x71 x6)) (* x72 x7)) (* x73 x8))) (+ x8 (+ (+ (+ (* x21 ?v_43) (* x22 ?v_44)) (* x23 ?v_45)) (* x24 ?v_46)))))) (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (>= (+ (+ (+ (* x58 x9) (* x59 x13)) (* x60 x17)) (* x61 x21)) (+ (+ (+ (* x9 ?v_63) (* x10 ?v_64)) (* x11 ?v_65)) (* x12 ?v_66))) (>= (+ (+ (+ (* x58 x10) (* x59 x14)) (* x60 x18)) (* x61 x22)) (+ (+ (+ (* x9 ?v_67) (* x10 ?v_68)) (* x11 ?v_69)) (* x12 ?v_70)))) (>= (+ (+ (+ (* x58 x11) (* x59 x15)) (* x60 x19)) (* x61 x23)) (+ (+ (+ (* x9 ?v_71) (* x10 ?v_72)) (* x11 ?v_73)) (* x12 ?v_74)))) (>= (+ (+ (+ (* x58 x12) (* x59 x16)) (* x60 x20)) (* x61 x24)) (+ (+ (+ (* x9 ?v_75) (* x10 ?v_76)) (* x11 ?v_77)) (* x12 ?v_78)))) (>= (+ (+ (+ (* x62 x9) (* x63 x13)) (* x64 x17)) (* x65 x21)) (+ (+ (+ (* x13 ?v_63) (* x14 ?v_64)) (* x15 ?v_65)) (* x16 ?v_66)))) (>= (+ (+ (+ (* x62 x10) (* x63 x14)) (* x64 x18)) (* x65 x22)) (+ (+ (+ (* x13 ?v_67) (* x14 ?v_68)) (* x15 ?v_69)) (* x16 ?v_70)))) (>= (+ (+ (+ (* x62 x11) (* x63 x15)) (* x64 x19)) (* x65 x23)) (+ (+ (+ (* x13 ?v_71) (* x14 ?v_72)) (* x15 ?v_73)) (* x16 ?v_74)))) (>= (+ (+ (+ (* x62 x12) (* x63 x16)) (* x64 x20)) (* x65 x24)) (+ (+ (+ (* x13 ?v_75) (* x14 ?v_76)) (* x15 ?v_77)) (* x16 ?v_78)))) (>= (+ (+ (+ (* x66 x9) (* x67 x13)) (* x68 x17)) (* x69 x21)) (+ (+ (+ (* x17 ?v_63) (* x18 ?v_64)) (* x19 ?v_65)) (* x20 ?v_66)))) (>= (+ (+ (+ (* x66 x10) (* x67 x14)) (* x68 x18)) (* x69 x22)) (+ (+ (+ (* x17 ?v_67) (* x18 ?v_68)) (* x19 ?v_69)) (* x20 ?v_70)))) (>= (+ (+ (+ (* x66 x11) (* x67 x15)) (* x68 x19)) (* x69 x23)) (+ (+ (+ (* x17 ?v_71) (* x18 ?v_72)) (* x19 ?v_73)) (* x20 ?v_74)))) (>= (+ (+ (+ (* x66 x12) (* x67 x16)) (* x68 x20)) (* x69 x24)) (+ (+ (+ (* x17 ?v_75) (* x18 ?v_76)) (* x19 ?v_77)) (* x20 ?v_78)))) (>= (+ (+ (+ (* x70 x9) (* x71 x13)) (* x72 x17)) (* x73 x21)) (+ (+ (+ (* x21 ?v_63) (* x22 ?v_64)) (* x23 ?v_65)) (* x24 ?v_66)))) (>= (+ (+ (+ (* x70 x10) (* x71 x14)) (* x72 x18)) (* x73 x22)) (+ (+ (+ (* x21 ?v_67) (* x22 ?v_68)) (* x23 ?v_69)) (* x24 ?v_70)))) (>= (+ (+ (+ (* x70 x11) (* x71 x15)) (* x72 x19)) (* x73 x23)) (+ (+ (+ (* x21 ?v_71) (* x22 ?v_72)) (* x23 ?v_73)) (* x24 ?v_74)))) (>= (+ (+ (+ (* x70 x12) (* x71 x16)) (* x72 x20)) (* x73 x24)) (+ (+ (+ (* x21 ?v_75) (* x22 ?v_76)) (* x23 ?v_77)) (* x24 ?v_78)))))) ?v_79))))))))
(check-sat)
(exit)
