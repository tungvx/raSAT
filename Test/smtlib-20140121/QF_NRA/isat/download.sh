HPCC=s1310007@hpcc:/work/tungvx/isat3-0.02-20140409-i686/bin/QF_NRA
LOCAL=./
ABCD="toilatung90!2"
sshpass -p $ABCD scp $HPCC/0/QF_NRA_0_20160108.csv $LOCAL
sshpass -p $ABCD scp $HPCC/1/QF_NRA_1_20160108.csv $LOCAL
sshpass -p $ABCD scp $HPCC/2/QF_NRA_2_20160108.csv $LOCAL
python check.py
