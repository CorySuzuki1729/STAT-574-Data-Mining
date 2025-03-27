/* STAT 574 HW3 Problem 1 */

proc import out=cardtrans
 datafile="C:/Users/coryg/OneDrive/Desktop/STAT_574_Data_Mining/card_transdata.csv" dbms=csv
 replace;
 /*SPLITTING DATA INTO 80% TRAINING AND 20% TESTING SETS*/
 proc surveyselect data=cardtrans rate=0.8 seed=210925
 out=cardtrans outall method=srs;
 run;
 data train (drop=selected);
 set cardtrans;
 if selected=1;
 run;
 data test (drop=selected);
 set cardtrans;
 if selected=0;
 run;
 /*COMPUTING PRIOR PROBABILITIES*/
 proc freq data=train noprint;
 table fraud/out=priors;
 run;
 data priors;
 set priors;
 percent=percent/100;
 if fraud=0 then
 call symput('prior_no', percent);
 if fraud=1 then
 call symput('prior_yes', percent);
 run;
 /*COMPUTING POSTERIOR PROBABILITIES FOR CATEGORICAL PREDICTORS*/
 proc freq data=train noprint;
 table fraud*repeat_retailer/out=repeat_retailer_perc
 nocum list;
 run;
 data repeat_retailer_perc;
 set repeat_retailer_perc;
 percent=percent/100;
 if fraud=0 and repeat_retailer=0 then
 call symput('repeat_retailer_no', percent);
 if fraud=0 and repeat_retailer=1 then
 call symput('repeat_retailer_yes', percent);
 if fraud=1 and repeat_retailer=0 then
 call symput('repeat_retailer_no', percent);
 if fraud=1 and repeat_retailer=1 then
 call symput('repeat_retailer_yes', percent);
 run;
 proc freq data=train noprint;
 table fraud*used_chip/out=used_chip_perc
 nocum list;
 run;
 data used_chip_perc;
 set used_chip_perc;
 percent=percent/100;
 if fraud=0 and used_chip=0 then
 call symput('used_chip_no', percent);
 if fraud=0 and used_chip=1 then
 call symput('used_chip_yes', percent);
 if fraud=1 and used_chip=0 then
 call symput('used_chip_no', percent);
 if fraud=1 and used_chip=1 then
 call symput('used_chip_yes', percent);
 run;
 proc freq data=train noprint;
 table fraud*used_pin_number/out=used_pin_number_perc
 nocum list;
 run;
 data used_pin_number_perc;
 set used_pin_number_perc;
 percent=percent/100;
 if fraud=0 and used_pin_number=0 then
 call symput('used_pin_number_no', percent);
 if fraud=0 and used_pin_number=1 then
 call symput('used_pin_number_yes', percent);
 if fraud=1 and used_pin_number=0 then
 call symput('used_pin_number_no', percent);
 if fraud=1 and used_pin_number=1 then
 call symput('used_pin_number_yes', percent);
 run;
 proc freq data=train noprint;
 table fraud*online_order/out=online_order_perc
 nocum list;
 run;
 data online_order_perc;
 set online_order_perc;
 percent=percent/100;
 if fraud=0 and online_order=0 then
 call symput('online_order_no', percent);
 if fraud=0 and online_order=1 then
 call symput('online_order_yes', percent);
 if fraud=1 and online_order=0 then
 call symput('online_order_no', percent);
 if fraud=1 and online_order=1 then
 call symput('online_order_yes', percent);
 run;
 /*COMPUTING MEAN AND STANDARD DEVIATION FOR
 NUMERICAL PREDICTORS*/ proc means data=train mean std
 noprint;
 class fraud;
 var distance_from_home distance_from_last_transaction
 ratio_to_median_purchase_price;
 output out=stats;
 run;
 data stats;
 set stats;
 if fraud=0 and _stat_='MEAN' then
 do;
 call symput('dist_home_mean_no',distance_from_home);
 call symput('dist_trans_mean_no',distance_from_last_transaction);
 call symput('ratio_price_mean_no',ratio_to_median_purchase_price);
 end;
 if fraud=0 and _stat_='STD' then
 do;
 call symput('dist_home_std_no',distance_from_home);
 call symput('dist_trans_std_no',distance_from_last_transaction);
 call symput('ratio_price_std_no',ratio_to_median_purchase_price);
 end;
 if fraud=1 and _stat_='MEAN' then
 do;
 call symput('dist_home_mean_yes',distance_from_home);
 call symput('dist_trans_mean_yes',distance_from_last_transaction);
 call symput('ratio_price_mean_yes',ratio_to_median_purchase_price);
 end;
 if fraud=1 and _stat_='STD' then
 do;
 call symput('dist_home_std_yes',distance_from_home);
 call symput('dist_trans_std_yes',distance_from_last_transaction);
 call symput('ratio_price_std_yes',ratio_to_median_purchase_price);
 end;
 run;
 /*COMPUTING POSTERIOR PROBABILITIES FOR TESTING DATA*/
 data test;
 set test;
 if (repeat_retailer=0 and used_chip=0 and used_pin_number=0 and
 online_order=0) then do;
 pred_prob_no=&prior_no*&repeat_retailer_no*&used_chip_no*&used_pin_number_no
 *&online_order_no*1/(2*3.14)**1.5*1/(&dist_home_std_no*&dist_trans_std_no
 *&ratio_price_std_no)*exp(-(distance_from_home-&dist_home_mean_no)**2/
 (2*&dist_home_std_no**2)-(distance_from_last_transaction-
 &dist_trans_mean_no)**2/(2*&dist_trans_std_no**2)-(ratio_to_median_purchase_price-&ratio_price_mean_no)**2/
 (2*&ratio_price_std_no**2));
 pred_prob_yes=&prior_yes*&repeat_retailer_no*&used_chip_no*&used_pin_number_no
 *&online_order_no*1/(2*3.14)**1.5*1/(&dist_home_std_yes*&dist_trans_std_yes
 *&ratio_price_std_yes)*exp(-(distance_from_home-&dist_home_mean_yes)*
 *2/ (2*&dist_home_std_yes**2)-(distance_from_last_transaction-&dist_trans_mean_yes)**2/(2*&dist_trans_std_yes**2)
 (ratio_to_median_purchase_price-&ratio_price_mean_yes)**2/(2*&ratio_price_std_yes**2));
 end;
 if(repeat_retailer=1 and used_chip=0 and used_pin_number=0 and online_order=0)
 then do;
 pred_prob_no=&prior_no*&repeat_retailer_yes*&used_chip_no*&used_pin_number_no *&online_order_no*1/(2*3.14)**1.5*1/(&dist_home_std_no*&dist_trans_std_no*
 &ratio_price_std_no)*exp(-(distance_from_home-&dist_home_mean_no)**2/
 (2*&dist_home_std_no**2)-(distance_from_last_transaction-&dist_trans_mean_no)**
 2/
 (2*&dist_trans_std_no**2)-(ratio_to_median_purchase_price-&ratio_price_mean_no)
 **2/ (2*&ratio_price_std_no**2));
 pred_prob_yes=&prior_yes*&repeat_retailer_yes*&used_chip_no*&used_pin_number_no*&online_order_no*1/(2*3.14)**1.5*1/(&dist_home_std_yes*&dist_trans_std_yes*
 &ratio_price_std_yes)*exp(-(distance_from_home-&dist_home_mean_yes)**2/
 (2*&dist_home_std_yes**2)-(distance_from_last_transaction-&dist_trans_mean_yes)**2/(2*&dist_trans_std_yes**2)-(ratio_to_median_purchase_price-&ratio_price_mean_yes)**2/
 (2*&ratio_price_std_yes**2));
 end;
 if(repeat_retailer=0 and used_chip=1 and used_pin_number=0 and online_order=0)
 then do;
 pred_prob_no=&prior_no*&repeat_retailer_no*&used_chip_yes*&used_pin_number_no *&online_order_no*1/(2*3.14)**1.5*1/(&dist_home_std_no*&dist_trans_std_no*
 &ratio_price_std_no)*exp(-(distance_from_home-&dist_home_mean_no)**2/
 (2*&dist_home_std_no**2)-(distance_from_last_transaction-&dist_trans_mean_no)**2/
 (2*&dist_trans_std_no**2)-(ratio_to_median_purchase_price-&ratio_price_mean_no)**2/ (2*&ratio_price_std_no**2));
 pred_prob_yes=&prior_yes*&repeat_retailer_no*&used_chip_yes*&used_pin_number_no*&online_order_no*1/(2*3.14)**1.5*1/(&dist_home_std_yes*&dist_trans_std_yes*
 &ratio_price_std_yes)*exp(-(distance_from_home-&dist_home_mean_yes)**2/
 (2*&dist_home_std_yes**2)-(distance_from_last_transaction-&dist_trans_mean_yes)**2/(2*&dist_trans_std_yes**2)-(ratio_to_median_purchase_price-&ratio_price_mean_yes)**2/
 (2*&ratio_price_std_yes**2));
 end;
 if(repeat_retailer=0 and used_chip=0 and used_pin_number=1 and online_order=0)
 then do;
 pred_prob_no=&prior_no*&repeat_retailer_no*&used_chip_no*&used_pin_number_yes *&online_order_no*1/(2*3.14)**1.5*1/(&dist_home_std_no*&dist_trans_std_no*
 &ratio_price_std_no)*exp(-(distance_from_home-&dist_home_mean_no)**2/
 (2*&dist_home_std_no**2)-(distance_from_last_transaction-&dist_trans_mean_no)**
 2/(2*&dist_trans_std_no**2)-(ratio_to_median_purchase_price-&ratio_price_mean_no)
 **2/ (2*&ratio_price_std_no**2));
 pred_prob_yes=&prior_yes*&repeat_retailer_no*&used_chip_no*&used_pin_number_yes*&online_order_no*1/(2*3.14)**1.5*1/(&dist_home_std_yes*&dist_trans_std_yes*&ratio_price_std_yes)*exp(-(distance_from_home-&dist_home_mean_yes)**2/
 (2*&dist_home_std_yes**2)-(distance_from_last_transaction-&dist_trans_mean_yes)**2/(2*&dist_trans_std_yes**2)-(ratio_to_median_purchase_price-&ratio_price_mean_yes)**2/
 (2*&ratio_price_std_yes**2));
 end;
 if(repeat_retailer=0 and used_chip=0 and used_pin_number=0 and online_order=1)
 then do;
 pred_prob_no=&prior_no*&repeat_retailer_no*&used_chip_no*&used_pin_number_no *&online_order_yes*1/(2*3.14)**1.5*1/(&dist_home_std_no*&dist_trans_std_no*
 &ratio_price_std_no)*exp(-(distance_from_home-&dist_home_mean_no)**2/
 (2*&dist_home_std_no**2)-(distance_from_last_transaction-&dist_trans_mean_no)**
 2/(2*&dist_trans_std_no**2)-(ratio_to_median_purchase_price-&ratio_price_mean_no)**2/ (2*&ratio_price_std_no**2));
 pred_prob_yes=&prior_yes*&repeat_retailer_no*&used_chip_no*&used_pin_number_no
 *&online_order_yes*1/(2*3.14)**1.5*1/(&dist_home_std_yes*&dist_trans_std_yes*&ratio_price_std_yes)*exp(-(distance_from_home-&dist_home_mean_yes)**2/
 (2*&dist_home_std_yes**2)-(distance_from_last_transaction-&dist_trans_mean_yes)**2/(2*&dist_trans_std_yes**2)-(ratio_to_median_purchase_price-&ratio_price_mean_yes)**2/
 (2*&ratio_price_std_yes**2));
 end;
 if(repeat_retailer=1 and used_chip=1 and used_pin_number=0 and online_order=0)
 then do;
 pred_prob_no=&prior_no*&repeat_retailer_yes*&used_chip_yes*&used_pin_number_no
 *&online_order_no*1/(2*3.14)**1.5*1/(&dist_home_std_no*&dist_trans_std_no*
 &ratio_price_std_no)*exp(-(distance_from_home-&dist_home_mean_no)**2/
 (2*&dist_home_std_no**2)-(distance_from_last_transaction-&dist_trans_mean_no)**2/(2*&dist_trans_std_no**2)-(ratio_to_median_purchase_price-&ratio_price_mean_no)**2/ (2*&ratio_price_std_no**2));
 pred_prob_yes=&prior_yes*&repeat_retailer_yes*&used_chip_yes*&used_pin_number_no
 *&online_order_no*1/(2*3.14)**1.5*1/(&dist_home_std_yes*&dist_trans_std_yes*&ratio_price_std_yes)*exp(-(distance_from_home-&dist_home_mean_yes)**2/
 (2*&dist_home_std_yes**2)-(distance_from_last_transaction-&dist_trans_mean_yes)**2/(2*&dist_trans_std_yes**2)-(ratio_to_median_purchase_price-&ratio_price_mean_yes)**2/
 (2*&ratio_price_std_yes**2));
 end;
 if(repeat_retailer=1 and used_chip=0 and used_pin_number=1 and online_order=0)
 then do;
 pred_prob_no=&prior_no*&repeat_retailer_yes*&used_chip_no*&used_pin_number_yes *&online_order_no*1/(2*3.14)**1.5*1/(&dist_home_std_no*&dist_trans_std_no*
 &ratio_price_std_no)*exp(-(distance_from_home-&dist_home_mean_no)**2/
 (2*&dist_home_std_no**2)-(distance_from_last_transaction-&dist_trans_mean_no)**2/
 (2*&dist_trans_std_no**2)-(ratio_to_median_purchase_price-&ratio_price_mean_no)**2/ (2*&ratio_price_std_no**2));
 pred_prob_yes=&prior_yes*&repeat_retailer_yes*&used_chip_no*&used_pin_number_yes
 *&online_order_no*1/(2*3.14)**1.5*1/(&dist_home_std_yes*&dist_trans_std_yes*
 &ratio_price_std_yes)*exp(-(distance_from_home-&dist_home_mean_yes)**2/
 (2*&dist_home_std_yes**2)-(distance_from_last_transaction-&dist_trans_mean_yes)**2/(2*&dist_trans_std_yes**2)-(ratio_to_median_purchase_price-&ratio_price_mean_yes)**2/
 (2*&ratio_price_std_yes**2));
 end;
 if(repeat_retailer=1 and used_chip=0 and used_pin_number=0 and online_order=1)
 then do;
 pred_prob_no=&prior_no*&repeat_retailer_yes*&used_chip_no*&used_pin_number_no *&online_order_yes*1/(2*3.14)**1.5*1/(&dist_home_std_no*&dist_trans_std_no*
 &ratio_price_std_no)*exp(-(distance_from_home-&dist_home_mean_no)**2/
 (2*&dist_home_std_no**2)-(distance_from_last_transaction-&dist_trans_mean_no)**2/
 (2*&dist_trans_std_no**2)-(ratio_to_median_purchase_price-&ratio_price_mean_no)
 **2/ (2*&ratio_price_std_no**2));
 pred_prob_yes=&prior_yes*&repeat_retailer_yes*&used_chip_no*&used_pin_number_no
 *&online_order_yes*1/(2*3.14)**1.5*1/(&dist_home_std_yes*&dist_trans_std_yes
 * &ratio_price_std_yes)*exp(-(distance_from_home-&dist_home_mean_yes)**2/
 (2*&dist_home_std_yes**2)-(distance_from_last_transaction-&dist_trans_mean_yes)**2/(2*&dist_trans_std_yes**2)-(ratio_to_median_purchase_price-&ratio_price_mean_yes)**2/
 (2*&ratio_price_std_yes**2));
 end;
 if(repeat_retailer=0 and used_chip=1 and used_pin_number=1 and online_order=0)
 then do;
 pred_prob_no=&prior_no*&repeat_retailer_no*&used_chip_yes*&used_pin_number_yes *&online_order_no*1/(2*3.14)**1.5*1/(&dist_home_std_no*&dist_trans_std_no*
 &ratio_price_std_no)*exp(-(distance_from_home-&dist_home_mean_no)**2/
 (2*&dist_home_std_no**2)-(distance_from_last_transaction-&dist_trans_mean_no)**2/
 (2*&dist_trans_std_no**2)-(ratio_to_median_purchase_price-&ratio_price_mean_no)**2/ (2*&ratio_price_std_no**2));
 pred_prob_yes=&prior_yes*&repeat_retailer_no*&used_chip_yes*&used_pin_number_yes
 *&online_order_no*1/(2*3.14)**1.5*1/(&dist_home_std_yes*&dist_trans_std_yes*
 &ratio_price_std_yes)*exp(-(distance_from_home-&dist_home_mean_yes)**2/
 (2*&dist_home_std_yes**2)-(distance_from_last_transaction-&dist_trans_mean_yes)**2/(2*&dist_trans_std_yes**2)-(ratio_to_median_purchase_price-&ratio_price_mean_yes)**2/
 (2*&ratio_price_std_yes**2));
 end;
 if(repeat_retailer=0 and used_chip=1 and used_pin_number=0 and online_order=1)
 then do;
 pred_prob_no=&prior_no*&repeat_retailer_no*&used_chip_yes*&used_pin_number_no *&online_order_yes*1/(2*3.14)**1.5*1/(&dist_home_std_no*&dist_trans_std_no*
 &ratio_price_std_no)*exp(-(distance_from_home-&dist_home_mean_no)**2/
 (2*&dist_home_std_no**2)-(distance_from_last_transaction-&dist_trans_mean_no)**2/
 (2*&dist_trans_std_no**2)-(ratio_to_median_purchase_price-&ratio_price_mean_no)**2/ (2*&ratio_price_std_no**2));
 pred_prob_yes=&prior_yes*&repeat_retailer_no*&used_chip_yes*&used_pin_number_no
 *&online_order_yes*1/(2*3.14)**1.5*1/(&dist_home_std_yes*&dist_trans_std_yes
 * &ratio_price_std_yes)*exp(-(distance_from_home-&dist_home_mean_yes)**2/
 (2*&dist_home_std_yes**2)-(distance_from_last_transaction-&dist_trans_mean_yes)**2/(2*&dist_trans_std_yes**2)-(ratio_to_median_purchase_price-&ratio_price_mean_yes)**2/
 (2*&ratio_price_std_yes**2));
 end;
 if(repeat_retailer=0 and used_chip=0 and used_pin_number=1 and online_order=1)
 then do;
 pred_prob_no=&prior_no*&repeat_retailer_no*&used_chip_no*&used_pin_number_yes *&online_order_yes*1/(2*3.14)**1.5*1/(&dist_home_std_no*&dist_trans_std_no*
 &ratio_price_std_no)*exp(-(distance_from_home-&dist_home_mean_no)**2/
 (2*&dist_home_std_no**2)-(distance_from_last_transaction-&dist_trans_mean_no)**2/
 (2*&dist_trans_std_no**2)-(ratio_to_median_purchase_price-&ratio_price_mean_no)**2/ (2*&ratio_price_std_no**2));
 pred_prob_yes=&prior_yes*&repeat_retailer_no*&used_chip_no*&used_pin_number_yes
 *&online_order_yes*1/(2*3.14)**1.5*1/(&dist_home_std_yes*&dist_trans_std_yes
 * &ratio_price_std_yes)*exp(-(distance_from_home-&dist_home_mean_yes)**2/
 (2*&dist_home_std_yes**2)-(distance_from_last_transaction-&dist_trans_mean_yes)**2/(2*&dist_trans_std_yes**2)-(ratio_to_median_purchase_price-&ratio_price_mean_yes)**2/
 (2*&ratio_price_std_yes**2));
 end;
 if(repeat_retailer=1 and used_chip=1 and used_pin_number=1 and online_order=0)
 then do;
 pred_prob_no=&prior_no*&repeat_retailer_yes*&used_chip_yes*&used_pin_number_yes *&online_order_no*1/(2*3.14)**1.5*1/(&dist_home_std_no*&dist_trans_std_no*
 &ratio_price_std_no)*exp(-(distance_from_home-&dist_home_mean_no)**2/
 (2*&dist_home_std_no**2)-(distance_from_last_transaction-&dist_trans_mean_no)**2/
 (2*&dist_trans_std_no**2)-(ratio_to_median_purchase_price-&ratio_price_mean_no)**2/ (2*&ratio_price_std_no**2));
 pred_prob_yes=&prior_yes*&repeat_retailer_yes*&used_chip_yes*&used_pin_number_yes
 *&online_order_no*1/(2*3.14)**1.5*1/(&dist_home_std_yes*&dist_trans_std_yes*
 &ratio_price_std_yes)*exp(-(distance_from_home-&dist_home_mean_yes)**2/
 (2*&dist_home_std_yes**2)-(distance_from_last_transaction-&dist_trans_mean_yes)**2/(2*&dist_trans_std_yes**2)-(ratio_to_median_purchase_price-&ratio_price_mean_yes)**2/
 (2*&ratio_price_std_yes**2));
 end;
 if(repeat_retailer=1 and used_chip=1 and used_pin_number=0 and online_order=1)
 then do;
 pred_prob_no=&prior_no*&repeat_retailer_yes*&used_chip_yes*&used_pin_number_no *&online_order_yes*1/(2*3.14)**1.5*1/(&dist_home_std_no*&dist_trans_std_no*
 &ratio_price_std_no)*exp(-(distance_from_home-&dist_home_mean_no)**2/
 (2*&dist_home_std_no**2)-(distance_from_last_transaction-&dist_trans_mean_no)**2/
 (2*&dist_trans_std_no**2)-(ratio_to_median_purchase_price-&ratio_price_mean_no)**2/ (2*&ratio_price_std_no**2));
 pred_prob_yes=&prior_yes*&repeat_retailer_yes*&used_chip_yes*&used_pin_number_no
 *&online_order_yes*1/(2*3.14)**1.5*1/(&dist_home_std_yes*&dist_trans_std_yes*
 &ratio_price_std_yes)*exp(-(distance_from_home-&dist_home_mean_yes)**2/
 (2*&dist_home_std_yes**2)-(distance_from_last_transaction-&dist_trans_mean_yes)**2/(2*&dist_trans_std_yes**2)-(ratio_to_median_purchase_price-&ratio_price_mean_yes)**2/
 (2*&ratio_price_std_yes**2));
 end;
 if(repeat_retailer=1 and used_chip=0 and used_pin_number=1 and online_order=1)
 then do;
 pred_prob_no=&prior_no*&repeat_retailer_yes*&used_chip_no*&used_pin_number_yes *&online_order_yes*1/(2*3.14)**1.5*1/(&dist_home_std_no*&dist_trans_std_no*
 &ratio_price_std_no)*exp(-(distance_from_home-&dist_home_mean_no)**2/
 (2*&dist_home_std_no**2)-(distance_from_last_transaction-&dist_trans_mean_no)**2/
 (2*&dist_trans_std_no**2)-(ratio_to_median_purchase_price-&ratio_price_mean_no)**2/ (2*&ratio_price_std_no**2));
 pred_prob_yes=&prior_yes*&repeat_retailer_yes*&used_chip_no*&used_pin_number_yes
 *&online_order_yes*1/(2*3.14)**1.5*1/(&dist_home_std_yes*&dist_trans_std_yes*
 &ratio_price_std_yes)*exp(-(distance_from_home-&dist_home_mean_yes)**2/
 (2*&dist_home_std_yes**2)-(distance_from_last_transaction-&dist_trans_mean_yes)**2/(2*&dist_trans_std_yes**2)-(ratio_to_median_purchase_price-&ratio_price_mean_yes)**2/
 (2*&ratio_price_std_yes**2));
 end;
 if(repeat_retailer=0 and used_chip=1 and used_pin_number=1 and online_order=1)
 then do;
 pred_prob_no=&prior_no*&repeat_retailer_no*&used_chip_yes*&used_pin_number_yes *&online_order_yes*1/(2*3.14)**1.5*1/(&dist_home_std_no*&dist_trans_std_no*
 &ratio_price_std_no)*exp(-(distance_from_home-&dist_home_mean_no)**2/
 (2*&dist_home_std_no**2)-(distance_from_last_transaction-&dist_trans_mean_no)**2/
 (2*&dist_trans_std_no**2)-(ratio_to_median_purchase_price-&ratio_price_mean_no)**2/ (2*&ratio_price_std_no**2));
 pred_prob_yes=&prior_yes*&repeat_retailer_no*&used_chip_yes*&used_pin_number_yes
 *&online_order_yes*1/(2*3.14)**1.5*1/(&dist_home_std_yes*&dist_trans_std_yes*
 &ratio_price_std_yes)*exp(-(distance_from_home-&dist_home_mean_yes)**2/
 (2*&dist_home_std_yes**2)-(distance_from_last_transaction-&dist_trans_mean_yes)**2/(2*&dist_trans_std_yes**2)-(ratio_to_median_purchase_price-&ratio_price_mean_yes)**2/
 (2*&ratio_price_std_yes**2));
 end;
 if (repeat_retailer=1 and used_chip=1 and used_pin_number=1 and
 online_order=1) then do;
 pred_prob_no=&prior_no*&repeat_retailer_yes*&used_chip_yes*&used_pin_number_yes *&online_order_yes*1/(2*3.14)**1.5*1/(&dist_home_std_no*&dist_trans_std_no*
 &ratio_price_std_no)*exp(-(distance_from_home-&dist_home_mean_no)**2/
 (2*&dist_home_std_no**2)-(distance_from_last_transaction-&dist_trans_mean_no)**2/
 (2*&dist_trans_std_no**2)-(ratio_to_median_purchase_price-&ratio_price_mean_no)**2/ (2*&ratio_price_std_no**2));
 pred_prob_yes=&prior_yes*&repeat_retailer_yes*&used_chip_yes*&used_pin_number_yes
 *&online_order_yes*1/(2*3.14)**1.5*1/(&dist_home_std_yes*&dist_trans_std_yes*
 &ratio_price_std_yes)*exp(-(distance_from_home-&dist_home_mean_yes)**2/
 (2*&dist_home_std_yes**2)-(distance_from_last_transaction-&dist_trans_mean_yes)**2/(2*&dist_trans_std_yes**2)-(ratio_to_median_purchase_price-&ratio_price_mean_yes)**2/
 (2*&ratio_price_std_yes**2));
end;
run;

/*COMPUTING PREDICTION ACCURACY*/
data test;
set test;
if pred_prob_no < pred_prob_yes then pred_class=1;
else pred_class=0;
if fraud=pred_class then pred=1; else pred=0;
run;

proc sql;
select mean(pred) as accuracy
from test;
quit;