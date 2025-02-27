/* STAT 574 HW2 Problem 1 */

proc import out=hospital
datafile="C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/hospital_data.csv"
dbms=csv replace;
run;

/*SPLITTING DATA INTO 80% TRAINING AND 20% TESTING*/
proc surveyselect data=hospital rate=0.8 seed=233364
out=hospital outall method=srs; 
run;

/*BUILDING RANDOM FOREST REGRESSION*/
proc hpforest data=hospital seed=520530 
maxtrees=60 vars_to_try=4 trainfraction=0.7
maxdepth=50;
target surgery_cost/level=interval;
input gender/level=nominal;
input age BMI ASA surgery_duration_min/level=interval;
partition rolevar=selected(train='1');
save file='C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/random_forest.bin';
run;

/*COMPUTING PREDICTED VALUES FOR TESTING DATA*/
data test;
set hospital;
if(selected='0');
run;

proc hp4score data=test;
id surgery_cost;
score file='C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/random_forest.bin'
out=predicted;
run;

proc print;
run;

/*DETERMINING 10%, 15%, AND 20% ACCURACY*/
data accuracy;
set predicted;
if(abs(surgery_cost-P_surgery_cost)
<0.10*surgery_cost)
then ind10=1; else ind10=0;
if(abs(surgery_cost-P_surgery_cost)
<0.15*surgery_cost)
then ind15=1; else ind15=0; 
if(abs(surgery_cost-P_surgery_cost)
<0.20*surgery_cost)
then ind20=1; else ind20=0;
run;

proc sql;
 select sum(ind10)/count(*) as accuracy10, 
sum(ind15)/count(*) as accuracy15,
 sum(ind20)/count(*) as accuracy20
 from accuracy;
 quit;