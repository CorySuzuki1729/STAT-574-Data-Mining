proc import out=pneumonia datafile="C:/Users/000110888/OneDrive - CSULB/Desktop/pneumonia_data.csv"
dbms=csv replace;

/*SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS*/
proc surveyselect data=pneumonia rate=0.8 seed=6132208
out=pneumonia outall method=srs; 
run;

/*BUILDING RANDOM FOREST BINARY CLASSIFIER*/
proc hpforest data=pneumonia seed=115607
maxtrees=60 vars_to_try=4 trainfraction=0.7
maxdepth=50;
target pneumonia/level=binary;
input gender tobacco_use/level=nominal;
input age PM2_5/level=interval;
partition rolevar=selected(train='1');
save file='C:/Users/000110888/OneDrive - CSULB/Desktop/random_forest.bin';
run;

/*COMPUTING PREDICTED VALUES FOR TESTING DATA*/
data test;
set pneumonia;
if(selected='0');
run;

proc hp4score data=test;
id pneumonia;
score file='C:/Users/000110888/OneDrive - CSULB/Desktop/random_forest.bin'
out=predicted;
run;

/*COMPUTING PREDICTION ACCURACY FOR TESTING DATA*/
data predicted;
set predicted;
match=(pneumonia=lowcase(I_pneumonia));
run;

proc sql;
select mean(match) as accuracy
from predicted;
quit;
