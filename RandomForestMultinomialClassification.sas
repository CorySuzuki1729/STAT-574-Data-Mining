proc import out=movie 
datafile="C:/Users/000110888/OneDrive - CSULB/Desktop/movie_data.csv"
dbms=csv replace;
run;

/*SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS*/
proc surveyselect data=movie rate=0.8 seed=550040
out=movie outall method=srs; 
run;

/*BUILDING RANDOM FOREST MULTINOMIAL CLASSIFIER*/
proc hpforest data=movie seed=454545
maxtrees=150 vars_to_try=3 trainfraction=0.9
maxdepth=50;
target rating/level=ordinal;
input gender member/level=nominal;
input age nmovies/level=interval;
partition rolevar=selected(train='1');
save file='C:/Users/000110888/OneDrive - CSULB/Desktop/random_forest.bin';
run;

/*COMPUTING PREDICTED VALUES FOR TESTING DATA*/
data test;
set movie;
if(selected='0');
run;

proc hp4score data=test;
id rating;
score file='C:/Users/000110888/OneDrive - CSULB/Desktop/random_forest.bin'
out=predicted;
run;

proc print;
run;

/*COMPUTING PREDICTION ACCURACY FOR TESTING DATA*/
data predicted;
set predicted;
match=(rating=lowcase(I_rating));
run;

proc sql;
select mean(match) as accuracy
from predicted;
quit;
