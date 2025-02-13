proc import out=card_data datafile="C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/card_transdata.csv"
dbms=csv replace;
    
/* (a) Splitting the data into 80% training and 20% testing sets*/

proc surveyselect data=card_data rate=0.8 seed=122470
out=card_data outall method=srs;
run;

/*Gini-splitting and cost-complexity pruning*/

proc hpsplit data=card_data maxdepth=4;
    class repeat_retailer used_chip used_pin_number online_order fraud;
    model fraud(event="1")=distance_from_home distance_from_last_transaction ratio_to_median_purchase_price repeat_retailer used_chip used_pin_number online_order;
    grow gini;
    prune costcomplexity;
    partition rolevar=selected(train="1");
    output out=predicted;
    ID selected;
run;

/* (b)Computing prediction accuracy for testing set for Gini Tree*/

data test;
    set predicted;
    if(selected="0");
    keep fraud P_fraud;
run;

data cutoffs;
    set test;
    do i=1 to 99;
    tp = (P_fraudyes > 0.01*i and fraud="1");
    tn = (P_fraudyes < 0.01*i and fraud="0");
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

/* (c) Fitting binary classification tree using entropy splitting */
/* and cost-complexity pruning algorithm*/

proc hpsplit data=card_data maxdepth=4;
    class repeat_retailer used_chip used_pin_number online_order fraud;
    model fraud(event="1") = distance_from_home distance_from_last_transaction ratio_to_median_purchase_price repeat_retailer used_chip used_pin_number online_order;
    grow entropy;
    prune costcomplexity;
    partition rolevar=selected(train="1");
    output out=predicted2;
    ID selected;
run;

/* (d) Computing prediction accuracy for testing set for entropy */
/*splitting tree*/

data test2;
    set predicted2;
    if(selected="0");
    keep fraud P_fraud;
run;

data cutoffs2;
    set test2;
    do i=1 to 99;
    tp = (P_fraudyes > 0.01*i and fraud="1");
    tn = (P_fraudyes < 0.01*i and fraud="0");
    output;
    end;
run;

proc sql;
    create table rates2 as 
    select i, sum(tp+tn)/count(*) as trueclassrate
    from cutoffs2
    group by i;
    select 0.01*i as cutoff, trueclassrate
    from rates2
    having trueclassrate=max(trueclassrate);
quit;

/* (e) CHAID splitting and cost-complexity pruning*/

proc hpsplit data=card_data maxdepth=4;
    class repeat_retailer used_chip used_pin_number online_order fraud;
    model fraud(event="1") = distance_from_home distance_from_last_transaction ratio_to_median_purchase_price repeat_retailer used_chip used_pin_number online_order;
    grow CHAID;
    prune costcomplexity;
    partition rolevar=selected(train="1");
    output out=predicted3;
    ID selected;
run;

/* (f) Computing prediction accuracy for testing set for CHAID */
/*splitting tree*/

data test3;
    set predicted3;
    if(selected="0");
    keep fraud P_fraud;
run;

data cutoffs3;
    set test3;
    do i=1 to 99;
    tp = (P_fraudyes > 0.01*i and fraud="1");
    tn = (P_fraudyes < 0.01*i and fraud="0");
    output;
    end;
run;

proc sql;
    create table rates3 as 
    select i, sum(tp+tn)/count(*) as trueclassrate
    from cutoffs3
    group by i;
    select 0.01*i as cutoff, trueclassrate
    from rates3
    having trueclassrate=max(trueclassrate);
quit;


