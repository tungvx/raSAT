directory=s1310007@hpcc:/work/tungvx/raSAT_2_1TCs_10_1DecomposedVar_2/AProVE
strategy=1-5-8-11
date=20150522
sshpass -p 'toilatung90!2' scp $directory/0/1-5-8-$date.csv $strategy-$date-0.csv
sshpass -p 'toilatung90!2' scp $directory/1/1-5-8-$date.csv $strategy-$date-1.csv
sshpass -p 'toilatung90!2' scp $directory/2/1-5-8-$date.csv $strategy-$date-2.csv
sshpass -p 'toilatung90!2' scp $directory/3/1-5-8-$date.csv $strategy-$date-3.csv
sshpass -p 'toilatung90!2' scp $directory/4/1-5-8-$date.csv $strategy-$date-4.csv
sshpass -p 'toilatung90!2' scp $directory/5/1-5-8-$date.csv $strategy-$date-5.csv
sshpass -p 'toilatung90!2' scp $directory/6/1-5-8-$date.csv $strategy-$date-6.csv
sshpass -p 'toilatung90!2' scp $directory/7/1-5-8-$date.csv $strategy-$date-7.csv
sshpass -p 'toilatung90!2' scp $directory/8/1-5-8-$date.csv $strategy-$date-8.csv
sshpass -p 'toilatung90!2' scp $directory/9/1-5-8-$date.csv $strategy-$date-9.csv
sshpass -p 'toilatung90!2' scp $directory/10/1-5-8-$date.csv $strategy-$date-10.csv
python check.py
