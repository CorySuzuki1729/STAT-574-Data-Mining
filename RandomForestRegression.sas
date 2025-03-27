proc import out=housing
datafile="C:/Users/000110888/OneDrive - CSULB/Desktop/housing_data.csv"
dbms=csv replace;
run;

/*SPLITTING DATA INTO 80% TRAINING AND 20% TESTING*/
proc surveyselect data=housing rate=0.8 seed=502305
out=housing outall method=srs; 
run;

/*BUILDING RANDOM FOREST REGRESSION*/
proc hpforest data=housing seed=829743 
maxtrees=60 vars_to_try=4 trainfraction=0.7
maxdepth=50;
target median_house_value/level=interval;
input ocean_proximity/level=nominal;
input housing_median_age total_rooms total_bedrooms 
population households median_income/level=interval;
partition rolevar=selected(train='1');
save file='C:/Users/000110888/OneDrive - CSULB/Desktop/random_forest.bin';
run;

/*COMPUTING PREDICTED VALUES FOR TESTING DATA*/
data test;
set housing;
if(selected='0');
run;

proc hp4score data=test;
id median_house_value;
score file='C:/Users/000110888/OneDrive - CSULB/Desktop/random_forest.bin'
out=predicted;
run;

proc print;
run;

/*DETERMINING 10%, 15%, AND 20% ACCURACY*/
data accuracy;
set predicted;
if(abs(median_house_value-P_median_house_value)
<0.10*median_house_value)
then ind10=1; else ind10=0;
if(abs(median_house_value-P_median_house_value)
<0.15*median_house_value)
then ind15=1; else ind15=0; 
if(abs(median_house_value-P_median_house_value)
<0.20*median_house_value)
then ind20=1; else ind20=0;
run;

proc sql;
 select sum(ind10)/count(*) as accuracy10, 
sum(ind15)/count(*) as accuracy15,
 sum(ind20)/count(*) as accuracy20
 from accuracy;
 quit;
