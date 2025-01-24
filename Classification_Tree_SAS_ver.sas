proc import out=pneumonia datafile="C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/pneumonia_data.csv"
dbms=csv replace;
    
/*Splitting data into 80% training and 20% testing.*/

proc surveyselect data=pneumonia rate=0.8 seed=6132208
out=pneumonia outall method=srs;
run;

/*Gini Splitting and cost-complexity pruning*/

proc hpsplit data=pneumonia maxdepth=4;
    class pneumonia gender tobacco_use;
    model pneumonia(event="yes")=age gender tobacco_use PM2_5;
    grow gini;
    prune costcomplexity;
    partition rolevar=selected(train="1");
    output out=predicted;
    ID selected;
run;

proc print data=predicted (obs=10);
run;

/*Computing prediction accuracy for testing set*/

data test;
    set predicted;
    if(selected="0");
    keep pneumonia P_pneumoniayes;
run;

data cutoffs;
    set test;
    do i=1 to 99;
    tp = (P_pneumoniayes > 0.01*i and pneumonia="yes");
    tn = (P_pneumoniayes < 0.01*i and pneumonia="no");
    output;
    end;
run;

proc sql;
    create table rates as
    select i, sum(tp+tn)/count(*) as trueclassrate
    from cutoffs
    group by i;
    select 0.01*i as cutoff, trueclassrate
    from rates
    having trueclassrate=max(trueclassrate);
quit;

/*Entropy splitting and cost-complexity pruning*/

proc hpsplit data=pneumonia maxdepth=4;
    class pneumonia gender tobacco_use;
    model pneumonia(event="yes")= age gender tobacco_use PM2_5;
    grow entropy;
    prune costcomplexity;
    partition rolevar=selected(train="1");
    output out=predicted;
    ID selected;
run;

/*CHAID splitting and cost-complexity pruning*/

proc hpsplit data=pneumonia maxdepth=4;
    class pneumonia gender tobacco_use;
    model pneumonia(event="yes")= age gender tobacco_use PM2_5;
    grow CHAID;
    prune costcomplexity;
    partition rolevar=selected(train="1");
    output out=predicted;
    ID selected;
run;

