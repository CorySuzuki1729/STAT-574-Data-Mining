proc import out=pneumonia datafile="C:/Users/000110888/OneDrive - CSULB/Desktop/pneumonia_data.csv"
dbms=csv replace;

/*SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS*/
proc surveyselect data=pneumonia rate=0.8 seed=999454
out=pneumonia outall method=srs; 
run;

data train (drop=selected);
set pneumonia;
if selected=1;
run;

data test (drop=selected);
set pneumonia;
if selected=0;
run;

/*COMPUTING PRIOR PROBABILITIES*/
proc freq data=train noprint;
 table pneumonia/out=priors;
run;

data priors;
set priors;
 percent=percent/100;
 if pneumonia='no' then
 call symput('prior_no', percent);
 if pneumonia='yes' then
 call symput('prior_yes', percent);
run;

/*COMPUTING POSTERIOR PROBABILITIES FOR CATEGORICAL PREDICTORS*/
proc freq data=train noprint;
 table pneumonia*gender/out=gender_perc 
   nocum list;
run;

data gender_perc;
set gender_perc;
 percent=percent/100;
if pneumonia='no' and gender='F' then
 call symput('female_no', percent);
if pneumonia='no' and gender='M' then
 call symput('male_no', percent);
if pneumonia='yes' and gender='F' then
 call symput('female_yes', percent);
if pneumonia='yes' and gender='M' then
 call symput('male_yes', percent);
 run;

proc freq data=train noprint;
 table pneumonia*tobacco_use/out=tobacco_use_perc
   nocum list;
run;

data tobacco_use_perc;
set tobacco_use_perc;
 percent=percent/100;
if pneumonia='no' and tobacco_use='no' then
 call symput('tobacco_no_no', percent);
if pneumonia='no' and tobacco_use='yes' then
 call symput('tobacco_yes_no', percent);
if pneumonia='yes' and tobacco_use='no' then
 call symput('tobacco_no_yes', percent);
if pneumonia='yes' and tobacco_use='yes' then
 call symput('tobacco_yes_yes', percent);
 run;

/*COMPUTING MEAN AND STANDARD DEVIATION FOR NUMERICAL PREDICTORS*/
proc means data=train mean std noprint;
 class pneumonia;
  var age PM2_5;
output out=stats;
run;

data stats;
 set stats;
if pneumonia='no' and _stat_='MEAN' then
  do;
   call symput('age_mean_no',age);
   call symput('PM2_5_mean_no',PM2_5);
  end;
if pneumonia='no' and _stat_='STD' then
  do;
   call symput('age_std_no',age);
   call symput('PM2_5_std_no',PM2_5);
  end;
if pneumonia='yes' and _stat_='MEAN' then
  do;
   call symput('age_mean_yes',age);
   call symput('PM2_5_mean_yes',PM2_5);
  end;
if pneumonia='yes' and _stat_='STD' then
  do;
   call symput('age_std_yes',age);
   call symput('PM2_5_std_yes',PM2_5);
  end;
run;

/*COMPUTING POSTERIOR PROBABILITIES FOR TESTING DATA*/
data test;
set test;
if (gender='F' and tobacco_use='no') then
do;
pred_prob_no=&prior_no*&female_no*&tobacco_no_no*1/(2*3.14)*1/(&age_std_no*&PM2_5_std_no)
*exp(-(age-&age_mean_no)**2/(2*&age_std_no**2)-(PM2_5-&PM2_5_mean_no)**2/(2*&PM2_5_std_no**2));
pred_prob_yes=&prior_yes*&female_yes*&tobacco_no_yes*1/(2*3.14)*1/(&age_std_yes*&PM2_5_std_yes)
*exp(-(age-&age_mean_yes)**2/(2*&age_std_yes**2)-(PM2_5-&PM2_5_mean_yes)**2/(2*&PM2_5_std_yes**2));
end;
if (gender='M' and tobacco_use='no') then
do;
pred_prob_no=&prior_no*&male_no*&tobacco_no_no*1/(2*3.14)*1/(&age_std_no*&PM2_5_std_no)
*exp(-(age-&age_mean_no)**2/(2*&age_std_no**2)-(PM2_5-&PM2_5_mean_no)**2/(2*&PM2_5_std_no**2));
pred_prob_yes=&prior_yes*&male_yes*&tobacco_no_yes*1/(2*3.14)*1/(&age_std_yes*&PM2_5_std_yes)
*exp(-(age-&age_mean_yes)**2/(2*&age_std_yes**2)-(PM2_5-&PM2_5_mean_yes)**2/(2*&PM2_5_std_yes**2));
end;
if (gender='F' and tobacco_use='yes') then
do;
pred_prob_no=&prior_no*&female_no*&tobacco_yes_no*1/(2*3.14)*1/(&age_std_no*&PM2_5_std_no)
*exp(-(age-&age_mean_no)**2/(2*&age_std_no**2)-(PM2_5-&PM2_5_mean_no)**2/(2*&PM2_5_std_no**2));
pred_prob_yes=&prior_yes*&female_yes*&tobacco_yes_yes*1/(2*3.14)*1/(&age_std_yes*&PM2_5_std_yes)
*exp(-(age-&age_mean_yes)**2/(2*&age_std_yes**2)-(PM2_5-&PM2_5_mean_yes)**2/(2*&PM2_5_std_yes**2));
end;
if (gender='M' and tobacco_use='yes') then
do;
pred_prob_no=&prior_no*&male_no*&tobacco_yes_no*1/(2*3.14)*1/(&age_std_no*&PM2_5_std_no)
*exp(-(age-&age_mean_no)**2/(2*&age_std_no**2)-(PM2_5-&PM2_5_mean_no)**2/(2*&PM2_5_std_no**2));
pred_prob_yes=&prior_yes*&male_yes*&tobacco_yes_yes*1/(2*3.14)*1/(&age_std_yes*&PM2_5_std_yes)
*exp(-(age-&age_mean_yes)**2/(2*&age_std_yes**2)-(PM2_5-&PM2_5_mean_yes)**2/(2*&PM2_5_std_yes**2));
end;
run;

/*COMPUTING PREDICTION ACCURACY*/
data test;
 set test;
  if pred_prob_no < pred_prob_yes then pred_class='yes';
  else pred_class='no';
  if pneumonia=pred_class then pred=1; else pred=0;
 run;

 proc sql;
  select mean(pred) as accuracy
   from test;
quit;

