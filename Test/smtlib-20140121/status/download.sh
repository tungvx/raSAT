HPCC=s1310007@hpcc:/work/tungvx/raSAT_2_1TCs_10_1DecomposedVar_13/
LOCAL=./
ABCD="toilatung90!2"
sshpass -p $ABCD scp $HPCC/QF_NRA/QF_NRA_status_20160115.csv $LOCAL
sshpass -p $ABCD scp $HPCC/QF_NIA/QF_NIA_status_20160115.csv $LOCAL
