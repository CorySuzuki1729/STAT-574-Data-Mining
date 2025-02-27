/* STAT 574 HW2 Problem 2 */

proc import out=card_data datafile="C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/card_transdata.csv"
dbms=csv replace;

/*SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS*/
proc surveyselect data=card_data rate=0.8 seed=328323
out=card_data outall method=srs; 
run;

/*BUILDING RANDOM FOREST BINARY CLASSIFIER*/
proc hpforest data=card_data seed=115113
maxtrees=60 vars_to_try=4 trainfraction=0.7
maxdepth=50;
target fraud/level=binary;
input repeat_retailer used_chip used_pin_number online_order/level=nominal;
input distance_from_home distance_from_last_transaction ratio_to_median_purchase_price/level=interval;
partition rolevar=selected(train='1');
save file='C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/random_forest.bin';
run;

/*COMPUTING PREDICTED VALUES FOR TESTING DATA*/
data test;
set card_data;
if(selected='0');
run;

proc hp4score data=test;
id fraud;
score file='C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/random_forest.bin'
out=predicted;
run;

/*COMPUTING PREDICTION ACCURACY FOR TESTING DATA*/
data predicted;
set predicted;
match=(fraud=I_fraud);
run;

proc sql;
select mean(match) as accuracy
from predicted;
quit;