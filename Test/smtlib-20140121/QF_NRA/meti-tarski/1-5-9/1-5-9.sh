directory=s1310007@hpcc:/work/tungvx/raSAT_2_1TCs_10_1DecomposedVar_40/QF_NRA/meti-tarski
strategy=1-5-9
sshpass -p 'toilatung90!2' scp $directory/Arthan/1-5-8-20150422-Arthan.csv $strategy-20150422-Arthan.csv
sshpass -p 'toilatung90!2' scp $directory/asin/1-5-8-20150421-asin.csv $strategy-20150430-asin.csv
sshpass -p 'toilatung90!2' scp $directory/atan/problem/1-5-8-20150421-atan.problem.csv $strategy-20150430-atan.problem.csv
sshpass -p 'toilatung90!2' scp $directory/atan/vega/3/1-5-8-20150421-atan.vega.3.csv $strategy-20150421-atan.vega.3.csv
sshpass -p 'toilatung90!2' scp $directory/atan/vega/3/weak/1-5-8-20150421-atan.problem.csv $strategy-20150421-atan.vega.3.weak.csv 
sshpass -p 'toilatung90!2' scp $directory/bottom-plate-mixer/1-5-8-20150422-bottom-plate-mixer.csv $strategy-20150422-bottom-plate-mixer.csv
sshpass -p 'toilatung90!2' scp $directory/cbrt/1-5-8-20150422-cbrt.csv $strategy-20150422-cbrt.csv
sshpass -p 'toilatung90!2' scp $directory/Chua/1/1-5-8-20150422-Chua.1.csv $strategy-20150422-Chua.1.csv
sshpass -p 'toilatung90!2' scp $directory/Chua/2/1-5-8-20150422-Chua.2.csv $strategy-20150422-Chua.2.csv
sshpass -p 'toilatung90!2' scp $directory/CMOS/1-5-8-20150422-CMOS.csv $strategy-20150422-CMOS.csv 
sshpass -p 'toilatung90!2' scp $directory/CONVOI2/1-5-8-20150422-CMOS.csv $strategy-20150422-CONVOI2.csv
sshpass -p 'toilatung90!2' scp $directory/cos/1-5-8-20150422-cos.csv $strategy-20150422-cos.csv
sshpass -p 'toilatung90!2' scp $directory/exp/1-5-8-20150422-exp.csv $strategy-20150422-exp.csv
sshpass -p 'toilatung90!2' scp $directory/heartdipole/1-5-8-20150422-heartdipole.csv $strategy-20150422-heartdipole.csv
sshpass -p 'toilatung90!2' scp $directory/log/1-5-8-20150422-log.csv $strategy-20150422-log.csv
sshpass -p 'toilatung90!2' scp $directory/Lyapunov/1-5-8-20150422-Lyapunov.csv $strategy-20150422-Lyapunov.csv 
sshpass -p 'toilatung90!2' scp $directory/Nichols-Plot/1-5-8-20150422-Nichols-Plot.csv $strategy-20150422-Nichols-Plot.csv
sshpass -p 'toilatung90!2' scp $directory/polypaver/1-5-8-20150422-polypaver.csv $strategy-20150422-polypaver.csv
sshpass -p 'toilatung90!2' scp $directory/RL-high-pass-circuit/1-5-8-20150422-RL-high-pass-circuit.csv $strategy-20150422-RL-high-pass-circuit.csv
sshpass -p 'toilatung90!2' scp $directory/sin/344/1-5-8-20150422-sin.344.csv $strategy-20150422-sin.344.csv
sshpass -p 'toilatung90!2' scp $directory/sin/cos/1-5-8-20150422-sin.cos.csv $strategy-20150422-sin.cos.csv
sshpass -p 'toilatung90!2' scp $directory/sin/problem/1-5-8-20150422-sin.problem.csv $strategy-20150422-sin.problem.csv
sshpass -p 'toilatung90!2' scp $directory/sqrt/1mcosq/7/1-5-8-20150422-sqrt.1mcosq.7.csv $strategy-20150422-sqrt.1mcosq.7.csv
sshpass -p 'toilatung90!2' scp $directory/sqrt/1mcosq/8/1-5-8-20150422-sqrt.1mcosq.8.csv $strategy-20150422-sqrt.1mcosq.8.csv
sshpass -p 'toilatung90!2' scp $directory/sqrt/circles/1-5-8-20150422-sqrt.circles.csv $strategy-20150422-sqrt.circles.csv
sshpass -p 'toilatung90!2' scp $directory/sqrt/problem/1-5-8-20150422-sqrt.circles.csv $strategy-20150422-sqrt.problem.csv
cp ../check.py check.py
python check.py
rm check.py
