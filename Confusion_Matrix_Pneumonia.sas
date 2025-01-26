proc import out=pneumonia datafile="C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/pneumonia_data.csv"
dbms=csv replace;
    
/*Splitting data into 80% training and 20% testing*/

proc surveyselect data=pneumonia rate=0.8 seed=6132208 
out=pneumonia outall method=srs;
run;

/*Gini Splitting and cost-complexity pruning*/

proc hpsplit data=pneumonia maxdepth=4;
    class pneumonia gender tobacco_use;
    model pneumonia(event="yes")= age gender tobacco_use PM2_5;
    grow gini;
    prune costcomplexity;
    partition rolevar=selected(train="1");
    output out=predicted;
    ID selected;
run;

/*Computing confusion matrix and performance measures for testing set*/

data test;
set predicted;
if(selected="0");
tp=(P_pneumoniayes > 0.5 and pneumonia="yes");
fp=(P_pneumoniayes > 0.5 and pneumonia="no");
tn=(P_pneumoniano > 0.5 and pneumonia="no");
fn=(P_pneumoniano > 0.5 and pneumonia="yes");
run;

proc sql;
    create table confusion as
    select sum(tp) as tp, sum(fp) as fp, sum(tn) as tn,
    sum(fn) as fn, count(*) as total
    from test;
    select * from confusion;
quit;

proc sql;
    select (tp+tn)/total as accuracy, (fp+fn)/total as
    misclassrate, tp/(tp+fn) as sensitivity,
    fn/(tp+fn) as FNR, tn/(fp+tn) as specificity,
    fp/(fp+tn) as FPR, tp/(tp+fp) as precision,
    tn/(fn+tn) as NPV, 2*tp/(2*tp+fn+fp) as F1score
    from confusion;
quit;

