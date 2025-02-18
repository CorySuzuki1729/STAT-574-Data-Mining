proc import out=card_data datafile="C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/card_transdata.csv"
dbms=csv replace;
    
/* Splitting the data into 80% training and 20% testing sets*/

proc surveyselect data=card_data rate=0.8 seed=122470
out=card_data outall method=srs;
run;

/*Gini-splitting and cost-complexity pruning*/

proc hpsplit data=card_data maxdepth=7;
    class repeat_retailer used_chip used_pin_number online_order fraud;
    model fraud(event="1")=distance_from_home distance_from_last_transaction ratio_to_median_purchase_price repeat_retailer used_chip used_pin_number online_order;
    grow gini;
    prune costcomplexity;
    partition rolevar=selected(train="1");
    output out=predicted;
    ID selected;
run;

/* (a) Computing the confusion matrix using the 0.5 cutoff for the
predicted probability of fraud*/

data test;
    set predicted;
    if(selected="0");
    tp = (P_fraud1 > 0.5 and fraud="1");
    fp = (P_fraud1 > 0.5 and fraud="0");
    tn = (P_fraud0 > 0.5 and fraud="0");
    fn = (P_fraud0 > 0.5 and fraud="1");
run;

proc sql;
    create table confusion as
    select sum(tp) as tp, sum(fp) as fp, sum(tn) as tn,
    sum(fn) as fn, count(*) as total
    from test;
    select * from confusion;
quit;

/* (b) Compute the prediction performance measures: accuracy,
misclassification rate, sensitivity, False positive rate, precision,
negative predictive value, F1 score*/

proc sql;
    select (tp+tn)/total as accuracy, (fp+fn)/total as
    misclassrate, tp/(tp+fn) as sensitivity,
    fn/(tp+fn) as FNR, tn/(fp+tn) as specificity,
    fp/(fp+tn) as FPR, tp/(tp+fp) as precision,
    tn/(fn+tn) as NPV, 2*tp/(2*tp+fn+fp) as F1score
    from confusion;
quit;