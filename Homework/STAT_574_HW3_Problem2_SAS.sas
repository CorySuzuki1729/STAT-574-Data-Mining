/* STAT 574 HW3 Problem 2 */

proc import out=concussions
datafile="C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/concussions_data.csv"
dbms=csv replace;

/*SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS*/
proc surveyselect data=concussions rate=0.8 seed=267363
out=concussions outall method=srs;
run;
data train (drop=selected);
set concussions;
if selected=1;
run;
data test (drop=selected);
set concussions;
if selected=0;
run;

/*COMPUTING PRIOR PROBABILITIES*/
proc freq data=train noprint;
table concussion/out=priors;
run;
data priors;
set priors;
percent=percent/100;
if concussion='mild' then
call symput('prior_mild', percent);
if concussion='moderate' then
call symput('prior_mod', percent);
if concussion='severe' then
call symput('prior_sev', percent);
run;

/*COMPUTING POSTERIOR PROBABILITIES FOR CATEGORICAL PREDICTORS*/
proc freq data=train noprint;
table concussion*position/out=position_perc;
run;
data position_perc;
set position_perc;
percent=percent/100;
if concussion='mild' and position='Cornerback' then
call symput('Cornerback_mild', percent);
if concussion='moderate' and position='Cornerback' then
call symput('Cornerback_mod', percent);
if concussion='severe' and position='Cornerback' then
call symput('Cornerback_sev', percent);
if concussion='mild' and position='Offensive Lineman' then
call symput('Offensive_Lineman_mild', percent);
if concussion='moderate' and position='Offensive Lineman' then
call symput('Offensive_Lineman_mod', percent);
if concussion='severe' and position='Offensive Lineman' then
call symput('Offensive_Lineman_sev', percent);
if concussion='mild' and position='Quarterback' then
call symput('Quarterback_mild', percent);
if concussion='moderate' and position='Quarterback' then
call symput('Quarterback_mod', percent);
if concussion='severe' and position='Quarterback' then
call symput('Quarterback_sev', percent);
if concussion='mild' and position='Running Back' then
call symput('Running_Back_mild', percent);
if concussion='moderate' and position='Running Back' then
call symput('Running_Back_mod', percent);
if concussion='severe' and position='Running Back' then
call symput('Running_Back_sev', percent);
if concussion='mild' and position='Wide Receiver' then
call symput('Wide_Receiver_mild', percent);
if concussion='moderate' and position='Wide Receiver' then
call symput('Wide_Receiver_mod', percent);
if concussion='severe' and position='Wide Receiver' then
call symput('Wide_Receiver_sev', percent);
run;

proc freq data=train noprint;
table concussion*prevconc/out=prevconc_perc
nocum list;
run;
data prevconc_perc;
set prevconc_perc;
percent=percent/100;
if concussion='mild' and prevconc=0 then call symput('prevconc0_mild', percent);
if concussion='moderate' and prevconc=0 then call symput('prevconc0_mod', percent);
if concussion='severe' and prevconc=0 then call symput('prevconc0_sev', percent);
if concussion='mild' and prevconc=1 then call symput('prevconc1_mild', percent);
if concussion='moderate' and prevconc=1 then call symput('prevconc1_mod', percent);
if concussion='severe' and prevconc=1 then call symput('prevconc1_sev', percent);
if concussion='mild' and prevconc=2 then call symput('prevconc2_mild', percent);
if concussion='moderate' and prevconc=2 then call symput('prevconc2_mod', percent);
if concussion='severe' and prevconc=2 then call symput('prevconc2_sev', percent);
if concussion='mild' and prevconc=3 then call symput('prevconc3_mild', percent);
if concussion='moderate' and prevconc=3 then call symput('prevconc3_mod', percent);
if concussion='severe' and prevconc=3 then call symput('prevconc3_sev', percent);
run;

/*COMPUTING MEAN AND STANDARD DEVIATION FOR NUMERICAL PREDICTORS*/
proc means data=train mean std noprint;
class concussion;
var age nyearsplaying;
output out=stats;
run;
data stats;
set stats;
if concussion='mild' and _stat_='MEAN' then
do;
call symput('age_mean_mild',age);
call symput('nyearsplaying_mean_mild',nyearsplaying);
end;
if concussion='mod' and _stat_='MEAN' then
do;
call symput('age_mean_mod',age);
call symput('nyearsplaying_mean_mod',nyearsplaying);
end;
if concussion='severe' and _stat_='MEAN' then
do;
call symput('age_mean_sev',age);
call symput('nyearsplaying_mean_sev',nyearsplaying);
end;
if concussion='mild' and _stat_='STD' then
do;
call symput('age_std_mild',age);
call symput('nyearsplaying_std_mild',nyearsplaying);
end;
if concussion='mod' and _stat_='STD' then
do;
call symput('age_std_mod',age);
call symput('nyearsplaying_std_mod',nyearsplaying);
end;
if concussion='severe' and _stat_='STD' then
do;
call symput('age_std_sev',age);
call symput('sev',nyearsplaying);
end;
run;

/*COMPUTING POSTERIOR PROBABILITIES FOR TESTING DATA*/
data test;
set test;
if (position='Cornerback' and prevconc=0) then do;
pred_prob_mild =
&prior_mild*&Cornerback_mild*&prevconc0_mild*1/(2*3.14)*1/(&age_std_mild*&nyearsplaying_std_mild)
*exp(-(age-&age_mean_mild)**2/(2*&age_std_mild**2)-(nyearsplaying-&nyearsplaying_mean_mild)**2/(2*&nyear
splaying_std_mild**2));
pred_prob_mod =
&prior_mod*&Cornerback_mod*&prevconc0_mod*1/(2*3.14)*1/(&age_std_mod*&nyearsplaying_std_mod)*exp(-(
age-&age_m
**2/(2*&age_std_mod**2)-(nyearsplaying-&nyearsplaying_mean_mod)**2/(2*&nyearsplaying_std_mod**2)));
pred_prob_sev =
&prior_sev*&Cornerback_sev*&prevconc0_sev*1/(2*3.14)*1/(&age_std_sev*&nyearsplaying_std_sev)
*exp(-(age-&age_mean_sev)**2/(2*&age_std_sev**2)-(nyearsplaying-&nyearsplaying_mean_sev)**2/(2*&nyear
splaying_std_sev**2)); end;
if (position='Cornerback' and prevconc=1) then do;
pred_prob_mild =
&prior_mild*&Cornerback_mild*&prevconc1_mild*1/(2*3.14)*1/(&age_std_mild*&nyearsplaying_std_mild)
*exp(-(age-&age_mean_mild)**2/(2*&age_std_mild**2)-(nyearsplaying-&nyearsplaying_mean_mild)**2/(2*&nyear
splaying_std_mild**2));
pred_prob_mod =
&prior_mod*&Cornerback_mod*&prevconc1_mod*1/(2*3.14)*1/(&age_std_mod*&nyearsplaying_std_mod)
*exp(-(age-&age_mean_mod)**2/(2*&age_std_mod**2)-(nyearsplaying-&nyearsplaying_mean_mod)**2/(2*&ny
earsplaying_std_mod**2));
pred_prob_sev =
&prior_sev*&Cornerback_sev*&prevconc1_sev*1/(2*3.14)*1/(&age_std_sev*&nyearsplaying_std_sev)
*exp(-(age-&age_mean_sev)**2/(2*&age_std_sev**2)-(nyearsplaying-&nyearsplaying_mean_sev)**2/(2*&nyear
splaying_std_sev**2)); end;
if (position='Cornerback' and prevconc=2) then do;
pred_prob_mild =
&prior_mild*&Cornerback_mild*&prevconc2_mild*1/(2*3.14)*1/(&age_std_mild*&nyearsplaying_std_mild)
*exp(-(age-&age_mean_mild)**2/(2*&age_std_mild**2)-(nyearsplaying-&nyearsplaying_mean_mild)**2/(2*&nyear
splaying_std_mild**2));
pred_prob_mod=&prior_mod*&Cornerback_mod*&prevconc2_mod*1/(2*3.14)*1/(&age_std_mod*&nyearsplayin
g_std_mod)
*exp(-(age-&age_mean_mod)**2/(2*&age_std_mod**2)-(nyearsplaying-&nyearsplaying_mean_mod)**2/(2*&ny
earsplaying_std_mod**2));
pred_prob_sev=&prior_sev*&Cornerback_sev*&prevconc2_sev*1/(2*3.14)*1/(&age_std_sev*&nyearsplaying_st
d_sev)
*exp(-(age-&age_mean_sev)**2/(2*&age_std_sev**2)-(nyearsplaying-&nyearsplaying_mean_sev)**2/(2*&nyear
splaying_std_sev**2)); end;
if (position='Cornerback' and prevconc=3) then do;
pred_prob_mild =
&prior_mild*&Cornerback_mild*&prevconc3_mild*1/(2*3.14)*1/(&age_std_mild*&nyearsplaying_std_mild)
*exp(-(age-&age_mean_mild)**2/(2*&age_std_mild**2)-(nyearsplaying-&nyearsplaying_mean_mild)**2/(2*&nyear
splaying_std_mild**2)
pred_prob_mod =
&prior_mod*&Cornerback_mod*&prevconc3_mod*1/(2*3.14)*1/(&age_std_mod*&nyearsplaying_std_mod)
*exp(-(age-&age_mean_mod)**2/(2*&age_std_mod**2)-(nyearsplaying-&nyearsplaying_mean_mod)**2/(2*&ny
earsplaying_std_mod**2));
pred_prob_sev =
&prior_sev*&Cornerback_sev*&prevconc3_sev*1/(2*3.14)*1/(&age_std_sev*&nyearsplaying_std_sev)
*exp(-(age-&age_mean_sev)**2/(2*&age_std_sev**2)-(nyearsplaying-&nyearsplaying_mean_sev)**2/(2*&nyear
splaying_std_sev**2)); end;
if (position='Offensive Lineman' and prevconc=0) then do;
pred_prob_mild =
&prior_mild*&Offensive_Lineman_mild*&prevconc0_mild*1/(2*3.14)*1/(&age_std_mild*&nyearsplaying_std_mild)*
exp (2*&age_std_mild**2)-(nyearsplaying-&nyearsplaying_mean_mild)**2/(2*&nyearsplaying_std_mild**2));
pred_prob_mod = &prior_mod*&Offensive_Lineman_mod*
&prevconc0_mod*1/(2*3.14)*1/(&age_std_mod*&nyearsplaying_std_mod)*exp(-(age-&age_mean_mod)**2/(2*&a
ge_std_mod**2)-(nyearsplay -&nyearsplaying_mean_mod)**2/(2*&nyearsplaying_std_mod**2));
pred_prob_sev =
&prior_sev*&Offensive_Lineman_sev*&prevconc0_sev*1/(2*3.14)*1/(&age_std_sev*&nyearsplaying_std_sev)*exp(-(age (2*&age_std_sev**2)-(nyearsplaying-&nyearsplaying_mean_sev)**2/(2*&nyearsplaying_std_sev**2)));
end;
if (position='Offensive Lineman' and prevconc=1) then do;
pred_prob_mild=&prior_mild*&Offensive_Lineman_mild*&prevconc1_mild*1/(2*3.14)*1/(&age_std_mild*&nyears
playing_std_mild)*exp(
-(age-&age_mean_mild)**2/(2*&age_std_mild**2)-(nyearsplaying-&nyearsplaying_mean_mild)**2/(2*&nyearspla
ying_std_mild**2));
pred_prob_mod=&prior_mod*&Offensive_Lineman_mod*&prevconc1_mod*1/(2*3.14)*1/(&age_std_mod*&
nyearsplaying_std_mod)*exp(
-(age-&age_mean_mod)**2/(2*&age_std_mod**2)-(nyearsplaying-&nyearsplaying_mean_mod)**2/(2*&nye
arsplaying_std_mod**2));
pred_prob_sev=&prior_sev*&Offensive_Lineman_sev*&prevconc1_sev*1/(2*3.14)*1/(&age_std_sev*&nyea
rsplaying_std_sev)*exp(
-(age-&age_mean_sev)**2/(2*&age_std_sev**2)-(nyearsplaying-&nyearsplaying_mean_sev)**2/(2*&nyears
playing_std_sev**2)); end;
if (position='Offensive Lineman' and prevconc=2) then do;
pred_prob_mild=&prior_mild*&Offensive_Lineman_mild*&prevconc2_mild*1/(2*3.14)*1/(&age_std_mild*&nyears
playing_std_mild)*exp(
-(age-&age_mean_mild)**2/(2*&age_std_mild**2)-(nyearsplaying-&nyearsplaying_mean_mild)**2/(2*&nyearspla
ying_std_mild**2));
pred_prob_mod=&prior_mod*&Offensive_Lineman_mod*&prevconc2_mod*1/(2*3.14)*1/(&age_std_mod*&
nyearsplaying_std_mod)*exp(
-(age-&age_mean_mod)**2/(2*&age_std_mod**2)-(nyearsplaying-&nyearsplaying_mean_mod)**2/(2*&nye
arsplaying_std_mod**2));
pred_prob_sev=&prior_sev*&Offensive_Lineman_sev*&prevconc2_sev*1/(2*3.14)*1/(&age_std_sev*&nyea
rsplaying_std_sev)*exp(
-(age-&age_mean_sev)**2/(2*&age_std_sev**2)-(nyearsplaying-&nyearsplaying_mean_sev)**2/(2*&nyears
playing_std_sev**2)); end;
if (position='Offensive Lineman' and prevconc=3) then do;
pred_prob_mild=&prior_mild*&Offensive_Lineman_mild*&prevconc3_mild*1/(2*3.14)*1/(&age_std_mild*&nyears
playing_std_mild)*exp(
-(age-&age_mean_mild)**2/(2*&age_std_mild**2)-(nyearsplaying-&nyearsplaying_mean_mild)**2/(2*&nyearspla
ying_std_mild**2));
pred_prob_mod=&prior_mod*&Offensive_Lineman_mod*&prevconc3_mod*1/(2*3.14)*1/(&age_std_mod*&
nyearsplaying_std_mod)*exp(
-(age-&age_mean_mod)**2/(2*&age_std_mod**2)-(nyearsplaying-&nyearsplaying_mean_mod)**2/(2*&nye
arsplaying_std_mod**2));
pred_prob_sev=&prior_sev*&Offensive_Lineman_sev*&prevconc3_sev*1/(2*3.14)*1/(&age_std_sev*&nyea
rsplaying_std_sev)*exp(
-(age-&age_mean_sev)**2/(2*&age_std_sev**2)-(nyearsplaying-&nyearsplaying_mean_sev)**2/(2*&nyears
playing_std_sev**2)); end;
if (position='Quarterback' and prevconc=0) then do;
pred_prob_mild=&prior_mild*&Quarterback_mild*&prevconc0_mild*1/(2*3.14)*1/(&age_std_mild*&nyearsplayi
ng_std_mild)*exp(
-(age-&age_mean_mild)**2/(2*&age_std_mild**2)-(nyearsplaying-&nyearsplaying_mean_mild)**2/(2*&nyearspl
aying_std_mild**2));
pred_prob_mod=&prior_mod*&Quarterback_mod*&prevconc0_mod*1/(2*3.14)*1/(&age_std_mod*&nyear
splaying_std_mod)*exp(
-(age-&age_mean_mod)**2/(2*&age_std_mod**2)-(nyearsplaying-&nyearsplaying_mean_mod)**2/(2*&ny
earsplaying_std_mod**2));
pred_prob_sev=&prior_sev*&Quarterback_sev*&prevconc0_sev*1/(2*3.14)*1/(&age_std_sev*&nyearsplayi
ng_std_sev)*exp(
-(age-&age_mean_sev)**2/(2*&age_std_sev**2)-(nyearsplaying-&nyearsplaying_mean_sev)**2/(2*&nyears
playing_std_sev**2)); end;
if (position='Quarterback' and prevconc=1) then do;
pred_prob_mild=&prior_mild*&Quarterback_mild*&prevconc1_mild*1/(2*3.14)*1/(&age_std_mild*&nyearsplayi
ng_std_mild)*exp(
-(age-&age_mean_mild)**2/(2*&age_std_mild**2)-(nyearsplaying-&nyearsplaying_mean_mild)**2/(2*&nyearspl
aying_std_mild**2));
pred_prob_mod=&prior_mod*&Quarterback_mod*&prevconc1_mod*1/(2*3.14)*1/(&age_std_mod*&nyears
playing_std_mod)*exp(
-(age-&age_mean_mod)**2/(2*&age_std_mod**2)-(nyearsplaying-&nyearsplaying_mean_mod)**2/(2*&nye
arsplaying_std_mod**2));
pred_prob_sev=&prior_sev*&Quarterback_sev*&prevconc1_sev*1/(2*3.14)*1/(&age_std_sev*&nyearsplayi
ng_std_sev)*exp(
-(age-&age_mean_sev)**2/(2*&age_std_sev**2)-(nyearsplaying-&nyearsplaying_mean_sev)**2/(2*&nyears
playing_std_sev**2)); end;
if (position='Quarterback' and prevconc=2) then do;
pred_prob_mild=&prior_mild*&Quarterback_mild*&prevconc2_mild*1/(2*3.14)*1/(&age_std_mild*&nyearsplayi
ng_std_mild)*exp(
-(age-&age_mean_mild)**2/(2*&age_std_mild**2)-(nyearsplaying-&nyearsplaying_mean_mild)**2/(2*&nyearspl
aying_std_mild**2));
pred_prob_mod=&prior_mod*&Quarterback_mod*&prevconc2_mod*1/(2*3.14)*1/(&age_std_mod*&nyears
playing_std_mod)*exp(
-(age-&age_mean_mod)**2/(2*&age_std_mod**2)-(nyearsplaying-&nyearsplaying_mean_mod)**2/(2*&nye
arsplaying_std_mod**2));
pred_prob_sev=&prior_sev*&Quarterback_sev*&prevconc2_sev*1/(2*3.14)*1/(&age_std_sev*&nyearsplayi
ng_std_sev)*exp(
-(age-&age_mean_sev)**2/(2*&age_std_sev**2)-(nyearsplaying-&nyearsplaying_mean_sev)**2/(2*&nyears
playing_std_sev**2)); end;
if (position='Quarterback' and prevconc=3) then do;
pred_prob_mild=&prior_mild*&Quarterback_mild*&prevconc3_mild*1/(2*3.14)*1/(&age_std_mild*&nyearsplayi
ng_std_mild)*exp(
-(age-&age_mean_mild)**2/(2*&age_std_mild**2)-(nyearsplaying-&nyearsplaying_mean_mild)**2/(2*&nyearspl
aying_std_mild**2));
pred_prob_mod=&prior_mod*&Quarterback_mod*&prevconc3_mod*1/(2*3.14)*1/(&age_std_mod*&nyears
playing_std_mod)*exp(
-(age-&age_mean_mod)**2/(2*&age_std_mod**2)-(nyearsplaying-&nyearsplaying_mean_mod)**2/(2*&nye
arsplaying_std_mod**2));
pred_prob_sev=&prior_sev*&Quarterback_sev*&prevconc3_sev*1/(2*3.14)*1/(&age_std_sev*&nyearsplayi
ng_std_sev)*exp(
-(age-&age_mean_sev)**2/(2*&age_std_sev**2)-(nyearsplaying-&nyearsplaying_mean_sev)**2/(2*&nyears
playing_std_sev**2)); end;
if (position='Running Back' and prevconc=0) then do;
pred_prob_mild=&prior_mild*&Running_Back_mild*&prevconc0_mild*1/(2*3.14)*1/(&age_std_mild*&nyearspla
ying_std_mild)*exp(
-(age-&age_mean_mild)**2/(2*&age_std_mild**2)-(nyearsplaying-&nyearsplaying_mean_mild)**2/(2*&nyearspl
aying_std_mild**2));
pred_prob_mod=&prior_mod*&Running_Back_mod*&prevconc0_mod*1/(2*3.14)*1/(&age_std_mod*&nyea
rsplaying_std_mod)*exp(
-(age-&age_mean_mod)**2/(2*&age_std_mod**2)-(nyearsplaying-&nyearsplaying_mean_mod)**2/(2*&nye
arsplaying_std_mod**2));
pred_prob_sev=&prior_sev*&Running_Back_sev*&prevconc0_sev*1/(2*3.14)*1/(&age_std_sev*&nyearspl
aying_std_sev)*exp(
-(age-&age_mean_sev)**2/(2*&age_std_sev**2)-(nyearsplaying-&nyearsplaying_mean_sev)**2/(2*&nyears
playing_std_sev**2)); end;
if (position='Running Back' and prevconc=1) then do;
pred_prob_mild=&prior_mild*&Running_Back_mild*&prevconc1_mild*1/(2*3.14)*1/(&age_std_mild*&nyearspla
ying_std_mild)*exp(
-(age-&age_mean_mild)**2/(2*&age_std_mild**2)-(nyearsplaying-&nyearsplaying_mean_mild)**2/(2*&nyearspl
aying_std_mild**2));
pred_prob_mod=&prior_mod*&Running_Back_mod*&prevconc1_mod*1/(2*3.14)*1/(&age_std_mod*&nyea
rsplaying_std_mod)*exp(
-(age-&age_mean_mod)**2/(2*&age_std_mod**2)-(nyearsplaying-&nyearsplaying_mean_mod)**2/(2*&nye
arsplaying_std_mod**2));
pred_prob_sev=&prior_sev*&Running_Back_sev*&prevconc1_sev*1/(2*3.14)*1/(&age_std_sev*&nyearspl
aying_std_sev)*exp(-(age
-&age_mean_sev)**2/(2*&age_std_sev**2)-(nyearsplaying-&nyearsplaying_mean_sev)**2/(2*&nyearsplayi
ng_std_sev**2)); end;
if (position='Running Back' and prevconc=2) then do;
pred_prob_mild=&prior_mild*&Running_Back_mild*&prevconc2_mild*1/(2*3.14)*1/(&age_std_mild*&nyearspla
ying_std_mild)*exp(
-(age-&age_mean_mild)**2/(2*&age_std_mild**2)-(nyearsplaying-&nyearsplaying_mean_mild)**2/(2*&nyearspl
aying_std_mild**2));
pred_prob_mod=&prior_mod*&Running_Back_mod*&prevconc2_mod*1/(2*3.14)*1/(&age_std_mod*&nyea
rsplaying_std_mod)*exp(
-(age-&age_mean_mod)**2/(2*&age_std_mod**2)-(nyearsplaying-&nyearsplaying_mean_mod)**2/(2*&nye
arsplaying_std_mod**2));
pred_prob_sev=&prior_sev*&Running_Back_sev*&prevconc2_sev*1/(2*3.14)*1/(&age_std_sev*&nyearspl
aying_std_sev)*exp(
-(age-&age_mean_sev)**2/(2*&age_std_sev**2)-(nyearsplaying-&nyearsplaying_mean_sev)**2/(2*&nyears
playing_std_sev**2)); end;
if (position='Running Back' and prevconc=3) then do;
pred_prob_mild=&prior_mild*&Running_Back_mild*&prevconc3_mild*1/(2*3.14)*1/(&age_std_mild*&nyearspla
ying_std_mild)*exp(
-(age-&age_mean_mild)**2/(2*&age_std_mild**2)-(nyearsplaying-&nyearsplaying_mean_mild)**2/(2*&nyearspl
aying_std_mild**2));
pred_prob_mod=&prior_mod*&Running_Back_mod*&prevconc3_mod*1/(2*3.14)*1/(&age_std_mod*&nye
arsplaying_std_mod)*exp(
-(age-&age_mean_mod)**2/(2*&age_std_mod**2)-(nyearsplaying-&nyearsplaying_mean_mod)**2/(2*&ny
earsplaying_std_mod**2));
pred_prob_sev=&prior_sev*&Running_Back_sev*&prevconc3_sev*1/(2*3.14)*1/(&age_std_sev*&nyearspl
aying_std_sev)*exp(
-(age-&age_mean_sev)**2/(2*&age_std_sev**2)-(nyearsplaying-&nyearsplaying_mean_sev)**2/(2*&nyears
playing_std_sev**2)); end;
if (position='Wide Receiver' and prevconc=0) then do;
pred_prob_mild=&prior_mild*&Wide_Receiver_mild*&prevconc0_mild*1/(2*3.14)*1/(&age_std_mild*&nyearspl
aying_std_mild)*exp(
-(age-&age_mean_mild)**2/(2*&age_std_mild**2)-(nyearsplaying-&nyearsplaying_mean_mild)**2/(2*&nyearspl
aying_std_mild**2));
pred_prob_mod=&prior_mod*&Wide_Receiver_mod*&prevconc0_mod*1/(2*3.14)*1/(&age_std_mod*&nye
arsplaying_std_mod)*exp(
-(age-&age_mean_mod)**2/(2*&age_std_mod**2)-(nyearsplaying-&nyearsplaying_mean_mod)**2/(2*&nye
arsplaying_std_mod**2));
pred_prob_sev=&prior_sev*&Wide_Receiver_sev*&prevconc0_sev*1/(2*3.14)*1/(&age_std_sev*&nyearspl
aying_std_sev)*exp(
-(age-&age_mean_sev)**2/(2*&age_std_sev**2)-(nyearsplaying-&nyearsplaying_mean_sev)**2/(2*&nyears
playing_std_sev**2)); end;
if (position='Wide Receiver' and prevconc=1) then do;
pred_prob_mild=&prior_mild*&Wide_Receiver_mild*&prevconc1_mild*1/(2*3.14)*1/(&age_std_mild*&nyearspl
aying_std_mild)*exp(
-(age-&age_mean_mild)**2/(2*&age_std_mild**2)-(nyearsplaying-&nyearsplaying_mean_mild)**2/(2*&nyearspl
aying_std_mild**2));
pred_prob_mod=&prior_mod*&Wide_Receiver_mod*&prevconc1_mod*1/(2*3.14)*1/(&age_std_mod*&nye
arsplaying_std_mod)*exp(
-(age-&age_mean_mod)**2/(2*&age_std_mod**2)-(nyearsplaying-&nyearsplaying_mean_mod)**2/(2*&nye
arsplaying_std_mod**2));
pred_prob_sev=&prior_sev*&Wide_Receiver_sev*&prevconc1_sev*1/(2*3.14)*1/(&age_std_sev*&nyearspl
aying_std_sev)*exp(
-(age-&age_mean_sev)**2/(2*&age_std_sev**2)-(nyearsplaying-&nyearsplaying_mean_sev)**2/(2*&nyears
playing_std_sev**2)); end;
if (position='Wide Receiver' and prevconc=2) then do;
pred_prob_mild=&prior_mild*&Wide_Receiver_mild*&prevconc2_mild*1/(2*3.14)*1/(&age_std_mild*&nyearsplayi
ng_std_mild)*exp(-(age
(2*&age_std_mild**2)-(nyearsplaying-&nyearsplaying_mean_mild)**2/(2*&nyearsplaying_std_mild**2)));
pred_prob_mod=&prior_mod*&Wide_Receiver_mod*&prevconc2_mod*1/(2*3.14)*1/(&age_std_mod*&nyearsplay
ing_std_mod)*exp(-(age-&age_
(2*&age_std_mod**2)-(nyearsplaying-&nyearsplaying_mean_mod)**2/(2*&nyearsplaying_std_mod**2)));
pred_prob_sev=&prior_sev*&Wide_Receiver_sev*&prevconc2_sev*1/(2*3.14)*1/(&age_std_sev*&nyearsplaying_
std_sev)*exp(-(age-&age_
(2*&age_std_sev**2)-(nyearsplaying-&nyearsplaying_mean_sev)**2/(2*&nyearsplaying_std_sev**2))); end;
if (position='Wide Receiver' and prevconc=3) then do;
pred_prob_mild=&prior_mild*&Wide_Receiver_mild*&prevconc3_mild*1/(2*3.14)*1/(&age_std_mild*&nyearsplayi
ng_std_mild)*exp(-(age
(2*&age_std_mild**2)-(nyearsplaying-&nyearsplaying_mean_mild)**2/(2*&nyearsplaying_std_mild**2)));
pred_prob_mod=&prior_mod*&Wide_Receiver_mod*&prevconc3_mod*1/(2*3.14)*1/(&age_std_mod*
&nyearsplaying_std_mod)*exp(-(age-&age_mean_mod)**2/(2*&age_std_mod**2)-(nyearsplaying
-&nyearsplaying_mean_mod)**2/(2*&nyearsplaying_std_mod**2));
pred_prob_sev=&prior_sev*&Wide_Receiver_sev*&prevconc3_sev*1/(2*3.14)*1/(&age_std_sev*&nyearsplaying_
std_sev)*exp(-(age-&age_
(2*&age_std_sev**2)-(nyearsplaying-&nyearsplaying_mean_sev)**2/(2*&nyearsplaying_std_sev**2))); end;
run;

/*COMPUTING PREDICTION ACCURACY*/
data test;
set test;
max_prob=max(pred_prob_mild, pred_prob_mod,
pred_prob_sev);
if max_prob=pred_prob_mod then pred_class='moderate';
if max_prob=pred_prob_mild then pred_class='mild';
if max_prob=pred_prob_sev then pred_class='severe';
if pred_class=concussion then pred=1; else pred=0;
run;

proc sql;
select mean(pred) as accuracy
from test;
quit;