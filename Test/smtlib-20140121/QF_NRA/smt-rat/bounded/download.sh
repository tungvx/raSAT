HPCC=s1310007@hpcc:/work/tungvx/smt-rat/QF_NRA
LOCAL=./
ABCD="toilatung90!2"
sshpass -p $ABCD scp $HPCC/0/QF_NRA_0_smt-rat_bounded_20160108.csv $LOCAL
sshpass -p $ABCD scp $HPCC/1/QF_NRA_1_smt-rat_bounded_20160108.csv $LOCAL
sshpass -p $ABCD scp $HPCC/2/QF_NRA_2_smt-rat_bounded_20160108.csv $LOCAL
python ../../check.py ../../../status/QF_NRA_status_20160115.csv smt-rat_unbounded.xls smt-rat
