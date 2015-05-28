directory=s1310007@hpcc:/work/tungvx/raSAT_2_1TCs_10_1DecomposedVar_6/QF_NRA/hycomp
strategy=1-5-8-11
date=20150522
sshpass -p 'toilatung90!2' scp $directory/0/$strategy-$date.xls $strategy-$date.0.csv
sshpass -p 'toilatung90!2' scp $directory/1/$strategy-$date.xls $strategy-$date.1.csv
sshpass -p 'toilatung90!2' scp $directory/2/$strategy-$date.xls $strategy-$date.2.csv
sshpass -p 'toilatung90!2' scp $directory/3/$strategy-$date.xls $strategy-$date.3.csv
cp ../../check.py check.py
python check.py
rm check.py
