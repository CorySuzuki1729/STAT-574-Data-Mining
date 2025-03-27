proc import out=sasuser.movies_ind
datafile="//vdi-fileshare02/UEMprofiles/000110888/Desktop/movie_data_ind.csv"
dbms=csv replace;
run;
/*************************************/
data rating;
set sasuser.movies;
_dataobs_=_n_;
keep _dataobs_ rating;
run;

proc sort;
by _dataobs_;
run;

data very_bad;
set tmp1.ann_class_verybad_logistic;
predprob_verybad=em_eventprobability;
keep _dataobs_ predprob_verybad;
run;

proc sort;
by _dataobs_;
run;

data bad;
set tmp1.ann_class_bad_logistic;
predprob_bad=em_eventprobability;
keep _dataobs_ predprob_bad;
run;

proc sort;
by _dataobs_;
run;

data okay;
set tmp1.ann_class_okay_logistic;;
predprob_okay=em_eventprobability;
keep _dataobs_ predprob_okay;
run;

proc sort;
by _dataobs_;
run;

data good;
set tmp1.ann_class_good_logistic;;
predprob_good=em_eventprobability;
keep _dataobs_ predprob_good;
run;

proc sort;
by _dataobs_;
run;

data very_good;
set tmp1.ann_class_verygood_logistic;
predprob_verygood=em_eventprobability;
keep _dataobs_ predprob_verygood;
run;

proc sort;
by _dataobs_;
run;

data all_data;
merge rating very_bad bad okay good very_good;
by _dataobs_;
if cmiss(predprob_verybad, predprob_bad,
predprob_okay, predprob_good, predprob_verygood)=0;
run;

data all_data;
set all_data;
predprob_max=max(predprob_very_bad, predprob_bad,
predprob_okay, predprob_good, predprob_very_good);
if (predprob_very_good=predprob_max) then pred_class='very good';
if (predprob_very_bad=predprob_max) then pred_class='very bad';
if (predprob_bad=predprob_max) then pred_class='bad';
if (predprob_okay=predprob_max) then pred_class='okay';
if (predprob_good=predprob_max) then pred_class='good';
keep rating pred_class;
run;

data all_data;
set all_data;
match=(rating=pred_class);
run;

proc sql;
select mean(match) as accuracy
from all_data;
quit;



