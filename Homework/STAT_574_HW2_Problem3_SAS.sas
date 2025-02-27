/* STAT 574 HW2 Problem 3 */

proc import out=concussion_data 
datafile="C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/concussions_data.csv"
dbms=csv replace;
run;

/*SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS*/
proc surveyselect data=concussion_data rate=0.8 seed=224113
out=concussion_data outall method=srs; 
run;

/*BUILDING RANDOM FOREST MULTINOMIAL CLASSIFIER*/
proc hpforest data=concussion_data seed=177013
maxtrees=150 vars_to_try=3 trainfraction=0.9
maxdepth=50;
target concussion/level=ordinal;
input position prevconc/level=nominal;
input age nyearsplaying/level=interval;
partition rolevar=selected(train='1');
save file='C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/random_forest.bin';
run;

/*COMPUTING PREDICTED VALUES FOR TESTING DATA*/
data test;
set concussion_data;
if(selected='0');
run;

proc hp4score data=test;
id concussion;
score file='C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/random_forest.bin'
out=predicted;
run;

proc print;
run;

/*COMPUTING PREDICTION ACCURACY FOR TESTING DATA*/
data predicted;
set predicted;
match=(concussion=lowcase(I_concussion));
run;

proc sql;
select mean(match) as accuracy
from predicted;
quit;