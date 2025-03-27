proc import out=movie datafile="C:/Users/000110888/OneDrive - CSULB/Desktop/movie_data.csv"
dbms=csv replace;

/*SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS*/
proc surveyselect data=movie rate=0.8 seed=121800
out=movie outall method=srs; 
run;

data train (drop=selected);
set movie;
if selected=1;
run;

data test (drop=selected);
set movie;
if selected=0;
run;

/*COMPUTING PRIOR PROBABILITIES*/
proc freq data=train noprint;
 table rating/out=priors;
run;

data priors;
set priors;
 percent=percent/100;
 if rating='very bad' then
 call symput('prior_very_bad', percent);
 if rating='bad' then
 call symput('prior_bad', percent);
 if rating='okay' then
 call symput('prior_okay', percent);
 if rating='good' then
 call symput('prior_good', percent);
 if rating='very good' then
 call symput('prior_very_good', percent);
run;

/*COMPUTING POSTERIOR PROBABILITIES FOR CATEGORICAL PREDICTORS*/
proc freq data=train noprint;
 table rating*gender/out=gender_perc 
   nocum list;
run;

data gender_perc;
set gender_perc;
 percent=percent/100;
if rating='very bad' and gender='F' then
 call symput('female_very_bad', percent);
if rating='very bad' and gender='M' then
 call symput('male_very_bad', percent);
if rating='bad' and gender='F' then
 call symput('female_bad', percent);
if rating='bad' and gender='M' then
 call symput('male_bad', percent);
if rating='okay' and gender='F' then
 call symput('female_okay', percent);
if rating='okay' and gender='M' then
 call symput('male_okay', percent);
if rating='good' and gender='F' then
 call symput('female_good', percent);
if rating='good' and gender='M' then
 call symput('male_good', percent);
if rating='very good' and gender='F' then
 call symput('female_very_good', percent);
if rating='very good' and gender='M' then
 call symput('male_very_good', percent);
run;

proc freq data=train noprint;
 table rating*member/out=member_perc
   nocum list;
run;

data member_perc;
set member_perc;
 percent=percent/100;
if rating='very bad' and member='no' then
 call symput('member_no_very_bad', percent);
if rating='very bad' and member='yes' then
 call symput('member_yes_very_bad', percent);
if rating='bad' and member='no' then
 call symput('member_no_bad', percent);
if rating='bad' and member='yes' then
 call symput('member_yes_bad', percent);
if rating='okay' and member='no' then
 call symput('member_no_okay', percent);
if rating='okay' and member='yes' then
 call symput('member_yes_okay', percent);
if rating='good' and member='no' then
 call symput('member_no_good', percent);
if rating='good' and member='yes' then
 call symput('member_yes_good', percent);
if rating='very good' and member='no' then
 call symput('member_no_very_good', percent);
if rating='very good' and member='yes' then
 call symput('member_yes_very_good', percent);
 run;

/*COMPUTING MEAN AND STANDARD DEVIATION FOR NUMERICAL PREDICTORS*/
proc means data=train mean std noprint;
 class rating;
  var age nmovies;
output out=stats;
run;

data stats;
 set stats;
if rating='very bad' and _stat_='MEAN' then
  do;
   call symput('age_mean_very_bad',age);
   call symput('nmovies_mean_very_bad',nmovies);
  end;
if rating='very bad' and _stat_='STD' then
  do;
   call symput('age_std_very_bad',age);
   call symput('nmovies_std_very_bad',nmovies);
  end;
if rating='bad' and _stat_='MEAN' then
  do;
   call symput('age_mean_bad',age);
   call symput('nmovies_mean_bad',nmovies);
  end;
if rating='bad' and _stat_='STD' then
  do; 
   call symput('age_std_bad',age);
   call symput('nmovies_std_bad',nmovies);
  end;
if rating='okay' and _stat_='MEAN' then
  do;
   call symput('age_mean_okay',age);
   call symput('nmovies_mean_okay',nmovies);
  end;
if rating='okay' and _stat_='STD' then
  do;
   call symput('age_std_okay',age);
   call symput('nmovies_std_okay',nmovies);
  end;
if rating='good' and _stat_='MEAN' then
  do;
   call symput('age_mean_good',age);
   call symput('nmovies_mean_good',nmovies);
  end;
if rating='good' and _stat_='STD' then
  do;
   call symput('age_std_good',age);
   call symput('nmovies_std_good',nmovies);
  end;
if rating='very good' and _stat_='MEAN' then
  do;
   call symput('age_mean_very_good',age);
   call symput('nmovies_mean_very_good',nmovies);
  end;
if rating='very good' and _stat_='STD' then
  do;
   call symput('age_std_very_good',age);
   call symput('nmovies_std_very_good',nmovies);
  end;
run;

/*COMPUTING POSTERIOR PROBABILITIES FOR TESTING DATA*/
data test;
set test;
if (gender='F' and member='no') then
do;
pred_prob_very_bad=&prior_very_bad*&female_very_bad*&member_no_very_bad*1/(2*3.14)*1/(&age_std_very_bad*&nmovies_std_very_bad)*exp(-(age-&age_mean_very_bad)**2/(2*&age_std_very_bad**2)-(nmovies-&nmovies_mean_very_bad)**2/(2*&nmovies_std_very_bad**2));
pred_prob_bad=&prior_bad*&female_bad*&member_no_bad*1/(2*3.14)*1/(&age_std_bad*&nmovies_std_bad)*exp(-(age-&age_mean_bad)**2/(2*&age_std_bad**2)-(nmovies-&nmovies_mean_bad)**2/(2*&nmovies_std_bad**2));
pred_prob_okay=&prior_okay*&female_okay*&member_no_okay*1/(2*3.14)*1/(&age_std_okay*&nmovies_std_okay)*exp(-(age-&age_mean_okay)**2/(2*&age_std_okay**2)-(nmovies-&nmovies_mean_okay)**2/(2*&nmovies_std_okay**2));
pred_prob_good=&prior_good*&female_good*&member_no_good*1/(2*3.14)*1/(&age_std_good*&nmovies_std_good)*exp(-(age-&age_mean_good)**2/(2*&age_std_good**2)-(nmovies-&nmovies_mean_good)**2/(2*&nmovies_std_good**2));
pred_prob_very_good=&prior_very_good*&female_very_good*&member_no_very_good*1/(2*3.14)*1/(&age_std_very_good*&nmovies_std_very_good)*exp(-(age-&age_mean_very_good)**2/(2*&age_std_very_good**2)-(nmovies-&nmovies_mean_very_good)**2/(2*&nmovies_std_very_good**2));
end;

if (gender='M' and member='no') then
do;
pred_prob_very_bad=&prior_very_bad*&male_very_bad*&member_no_very_bad*1/(2*3.14)*1/(&age_std_very_bad*&nmovies_std_very_bad)*exp(-(age-&age_mean_very_bad)**2/(2*&age_std_very_bad**2)-(nmovies-&nmovies_mean_very_bad)**2/(2*&nmovies_std_very_bad**2));
pred_prob_bad=&prior_bad*&male_bad*&member_no_bad*1/(2*3.14)*1/(&age_std_bad*&nmovies_std_bad)*exp(-(age-&age_mean_bad)**2/(2*&age_std_bad**2)-(nmovies-&nmovies_mean_bad)**2/(2*&nmovies_std_bad**2));
pred_prob_okay=&prior_okay*&male_okay*&member_no_okay*1/(2*3.14)*1/(&age_std_okay*&nmovies_std_okay)*exp(-(age-&age_mean_okay)**2/(2*&age_std_okay**2)-(nmovies-&nmovies_mean_okay)**2/(2*&nmovies_std_okay**2));
pred_prob_good=&prior_good*&male_good*&member_no_good*1/(2*3.14)*1/(&age_std_good*&nmovies_std_good)*exp(-(age-&age_mean_good)**2/(2*&age_std_good**2)-(nmovies-&nmovies_mean_good)**2/(2*&nmovies_std_good**2));
pred_prob_very_good=&prior_very_good*&male_very_good*&member_no_very_good*1/(2*3.14)*1/(&age_std_very_good*&nmovies_std_very_good)*exp(-(age-&age_mean_very_good)**2/(2*&age_std_very_good**2)-(nmovies-&nmovies_mean_very_good)**2/(2*&nmovies_std_very_good**2));
end;

if (gender='F' and member='yes') then
do;
pred_prob_very_bad=&prior_very_bad*&female_very_bad*&member_yes_very_bad*1/(2*3.14)*1/(&age_std_very_bad*&nmovies_std_very_bad)*exp(-(age-&age_mean_very_bad)**2/(2*&age_std_very_bad**2)-(nmovies-&nmovies_mean_very_bad)**2/(2*&nmovies_std_very_bad**2));
pred_prob_bad=&prior_bad*&female_bad*&member_yes_bad*1/(2*3.14)*1/(&age_std_bad*&nmovies_std_bad)*exp(-(age-&age_mean_bad)**2/(2*&age_std_bad**2)-(nmovies-&nmovies_mean_bad)**2/(2*&nmovies_std_bad**2));
pred_prob_okay=&prior_okay*&female_okay*&member_yes_okay*1/(2*3.14)*1/(&age_std_okay*&nmovies_std_okay)*exp(-(age-&age_mean_okay)**2/(2*&age_std_okay**2)-(nmovies-&nmovies_mean_okay)**2/(2*&nmovies_std_okay**2));
pred_prob_good=&prior_good*&female_good*&member_yes_good*1/(2*3.14)*1/(&age_std_good*&nmovies_std_good)*exp(-(age-&age_mean_good)**2/(2*&age_std_good**2)-(nmovies-&nmovies_mean_good)**2/(2*&nmovies_std_good**2));
pred_prob_very_good=&prior_very_good*&female_very_good*&member_yes_very_good*1/(2*3.14)*1/(&age_std_very_good*&nmovies_std_very_good)*exp(-(age-&age_mean_very_good)**2/(2*&age_std_very_good**2)-(nmovies-&nmovies_mean_very_good)**2/(2*&nmovies_std_very_good**2));
end;

if (gender='M' and member='yes') then
do;
pred_prob_very_bad=&prior_very_bad*&male_very_bad*&member_yes_very_bad*1/(2*3.14)*1/(&age_std_very_bad*&nmovies_std_very_bad)*exp(-(age-&age_mean_very_bad)**2/(2*&age_std_very_bad**2)-(nmovies-&nmovies_mean_very_bad)**2/(2*&nmovies_std_very_bad**2));
pred_prob_bad=&prior_bad*&male_bad*&member_yes_bad*1/(2*3.14)*1/(&age_std_bad*&nmovies_std_bad)*exp(-(age-&age_mean_bad)**2/(2*&age_std_bad**2)-(nmovies-&nmovies_mean_bad)**2/(2*&nmovies_std_bad**2));
pred_prob_okay=&prior_okay*&male_okay*&member_yes_okay*1/(2*3.14)*1/(&age_std_okay*&nmovies_std_okay)*exp(-(age-&age_mean_okay)**2/(2*&age_std_okay**2)-(nmovies-&nmovies_mean_okay)**2/(2*&nmovies_std_okay**2));
pred_prob_good=&prior_good*&male_good*&member_yes_good*1/(2*3.14)*1/(&age_std_good*&nmovies_std_good)*exp(-(age-&age_mean_good)**2/(2*&age_std_good**2)-(nmovies-&nmovies_mean_good)**2/(2*&nmovies_std_good**2));
pred_prob_very_good=&prior_very_good*&male_very_good*&member_yes_very_good*1/(2*3.14)*1/(&age_std_very_good*&nmovies_std_very_good)*exp(-(age-&age_mean_very_good)**2/(2*&age_std_very_good**2)-(nmovies-&nmovies_mean_very_good)**2/(2*&nmovies_std_very_good**2));
end;
run;

/*COMPUTING PREDICTION ACCURACY*/
data test;
 set test;
 max_prob=max(pred_prob_very_bad, pred_prob_bad,
 pred_prob_okay, pred_prob_good, pred_prob_very_good);
 if max_prob=pred_prob_very_good then pred_class='very good';
 if max_prob=pred_prob_very_bad then pred_class='very bad';
 if max_prob=pred_prob_bad then pred_class='bad';
 if max_prob=pred_prob_okay then pred_class='okay';
 if max_prob=pred_prob_good then pred_class='good';
 if pred_class=rating then pred=1; else pred=0;
run;

proc sql;
  select mean(pred) as accuracy
   from test;
quit;

