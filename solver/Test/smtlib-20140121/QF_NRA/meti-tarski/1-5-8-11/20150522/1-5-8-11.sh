directory=s1310007@hpcc:/work/tungvx/raSAT_2_1TCs_10_1DecomposedVar_46/QF_NRA/meti-tarski
strategy=1-5-8-11
date=20150522
sshpass -p 'toilatung90!2' scp $directory/Arthan/1-5-8-$date-Arthan.csv $strategy-$date-Arthan.csv
sshpass -p 'toilatung90!2' scp $directory/asin/1-5-8-$date-asin.csv $strategy-$date-asin.csv
sshpass -p 'toilatung90!2' scp $directory/atan/problem/1-5-8-$date-atan.problem.csv $strategy-$date-atan.problem.csv
sshpass -p 'toilatung90!2' scp $directory/atan/vega/3/1-5-8-$date-atan.vega.3.csv $strategy-$date-atan.vega.3.csv
sshpass -p 'toilatung90!2' scp $directory/atan/vega/3/weak/1-5-8-$date-atan.problem.csv $strategy-$date-atan.vega.3.weak.csv 
sshpass -p 'toilatung90!2' scp $directory/bottom-plate-mixer/1-5-8-$date-bottom-plate-mixer.csv $strategy-$date-bottom-plate-mixer.csv
sshpass -p 'toilatung90!2' scp $directory/cbrt/1-5-8-$date-cbrt.csv $strategy-$date-cbrt.csv
sshpass -p 'toilatung90!2' scp $directory/Chua/1/1-5-8-$date-Chua.1.csv $strategy-$date-Chua.1.csv
sshpass -p 'toilatung90!2' scp $directory/Chua/2/1-5-8-$date-Chua.2.csv $strategy-$date-Chua.2.csv
sshpass -p 'toilatung90!2' scp $directory/CMOS/1-5-8-$date-CMOS.csv $strategy-$date-CMOS.csv 
sshpass -p 'toilatung90!2' scp $directory/CONVOI2/1-5-8-$date-CMOS.csv $strategy-$date-CONVOI2.csv
sshpass -p 'toilatung90!2' scp $directory/cos/1-5-8-$date-cos.csv $strategy-$date-cos.csv
sshpass -p 'toilatung90!2' scp $directory/exp/1-5-8-$date-exp.csv $strategy-$date-exp.csv
sshpass -p 'toilatung90!2' scp $directory/heartdipole/1-5-8-$date-heartdipole.csv $strategy-$date-heartdipole.csv
sshpass -p 'toilatung90!2' scp $directory/log/1-5-8-$date-log.csv $strategy-$date-log.csv
sshpass -p 'toilatung90!2' scp $directory/Lyapunov/1-5-8-$date-Lyapunov.csv $strategy-$date-Lyapunov.csv 
sshpass -p 'toilatung90!2' scp $directory/Nichols-Plot/1-5-8-$date-Nichols-Plot.csv $strategy-$date-Nichols-Plot.csv
sshpass -p 'toilatung90!2' scp $directory/polypaver/1-5-8-$date-polypaver.csv $strategy-$date-polypaver.csv
sshpass -p 'toilatung90!2' scp $directory/RL-high-pass-circuit/1-5-8-$date-RL-high-pass-circuit.csv $strategy-$date-RL-high-pass-circuit.csv
sshpass -p 'toilatung90!2' scp $directory/sin/344/1-5-8-$date-sin.344.csv $strategy-$date-sin.344.csv
sshpass -p 'toilatung90!2' scp $directory/sin/cos/1-5-8-$date-sin.cos.csv $strategy-$date-sin.cos.csv
sshpass -p 'toilatung90!2' scp $directory/sin/problem/1-5-8-$date-sin.problem.csv $strategy-$date-sin.problem.csv
sshpass -p 'toilatung90!2' scp $directory/sqrt/1mcosq/7/1-5-8-$date-sqrt.1mcosq.7.csv $strategy-$date-sqrt.1mcosq.7.csv
sshpass -p 'toilatung90!2' scp $directory/sqrt/1mcosq/8/1-5-8-$date-sqrt.1mcosq.8.csv $strategy-$date-sqrt.1mcosq.8.csv
sshpass -p 'toilatung90!2' scp $directory/sqrt/circles/1-5-8-$date-sqrt.circles.csv $strategy-$date-sqrt.circles.csv
sshpass -p 'toilatung90!2' scp $directory/sqrt/problem/1-5-8-$date-sqrt.circles.csv $strategy-$date-sqrt.problem.csv
cp ../../check.py check.py
python check.py
rm check.py
