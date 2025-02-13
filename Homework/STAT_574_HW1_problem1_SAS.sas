proc import out=hospital
datafile="C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/hospital_data.csv"
dbms=csv replace;
run;

/* (a) Splitting the data into 80% training and 20% testing sets*/

proc surveyselect data=hospital rate=0.8 seed=479576 
out=hospital outall method=srs;
run;

/*RSS Splitting Criterion-Full Tree*/

proc hpsplit data=hospital seed=113123;
    class ASA gender;
    model surgery_cost = gender age BMI ASA surgery_duration_min;
    grow RSS;
    partition rolevar=selected(train="1");
run;

/*RSS Splitting Criterion and Cost-Complexity Pruning*/

proc hpsplit data=hospital seed=113123;
    class ASA gender;
    model surgery_cost = gender age BMI ASA surgery_duration_min;
    grow RSS;
    prune costcomplexity(leaves=12);
    partition rolevar=selected(train="1");
    output out=predicted;
    ID selected;
run;

/*(b) Computing prediction accuracy for testing data*/

data test;
    set predicted;
    if (selected="0");
    keep _leaf_ surgery_cost P_surgery_cost;
run;

data accuracy;
    set test;
    if(abs(surgery_cost-P_surgery_cost)<0.10*surgery_cost)
    then ind10=1; else ind10=0;
    if(abs(surgery_cost-P_surgery_cost)<0.15*surgery_cost)
    then ind15=1; else ind15=0;
    if(abs(surgery_cost-P_surgery_cost)<0.20*surgery_cost)
    then ind20=1; else ind20=0;
run;

proc sql;
    select mean(ind10) as accuracy10, mean(ind15) as accuracy15, mean(ind20) as accuracy20
    from accuracy;
quit;

/* (c) CHAID Splitting Criterion - Full Tree*/

proc hpsplit data=hospital seed=108698;
    class ASA gender;
    model surgery_cost = gender age BMI ASA surgery_duration_min;
    grow CHAID;
    partition rolevar = selected(train="1");
run;

/*CHAID Splitting Criterion -Cost Complexity Pruning*/

proc hpsplit data=hospital seed=108698;
    class ASA gender;
    model surgery_cost = gender age BMI ASA surgery_duration_min;
    grow CHAID;
    prune costcomplexity(leaves=33);
    partition rolevar=selected(train="1");
    output out=predicted;
    ID selected;
run;

/*(d) Computing prediction accuracy on testing set for CHAID Tree*/

data test;
    set predicted;
    if (selected="0");
    keep _leaf_ surgery_cost P_surgery_cost;
run;

data accuracy;
    set test;
    if(abs(surgery_cost-P_surgery_cost)<0.10*surgery_cost)
    then ind10=1; else ind10=0;
    if(abs(surgery_cost-P_surgery_cost)<0.15*surgery_cost)
    then ind15=1; else ind15=0;
    if(abs(surgery_cost-P_surgery_cost)<0.20*surgery_cost)
    then ind20=1; else ind20=0;
run;

proc sql;
    select mean(ind10) as accuracy10, mean(ind15) as accuracy15, mean(ind20) as accuracy20
    from accuracy;
quit;