/********** Scale score by PDO *******************************************************************/
 
%let pdo=70;
%let base_score=999;
%let odds=9;
 
data test;
set  dev;
 
/* Score */
      LOG_ODD = (-3.317277053)+ PRODUCT_TYPE_WOE*(-2.730093946)+ INSURANCE_TYPE1_WOE*(-1.761195018)+
                  STATE_WOE*( -0.17235483)+ APPLICANT_AGE_WOE*(-1.882743603)+ LTV_WOE*(-1.230591863)+ EMI_WOE*(-1.371595761);
      score                   =exp(log_odd)/(1+exp(log_odd));
      Reverse_Score           =1-score;
      format scale_score Reverse_scale_Score 10.;
      scale_score             = round(score*1000,1);
      Reverse_scale_Score     =round(Reverse_Score*1000,1);
 
/* Scale score by PDO */
/*    score       =_ offset + factor*ln(odds);*/
/*    score       =   offset - ((woe*ß)+ a)*factor;*/
/*    http://stats.stackexchange.com/questions/10092/what-does-20-ln2-mean-in-logistic-regression*/
/*    offset      =     600; Typically in credit scoring one would choose a baseline score e.g. 600*/
/*    Offset = 600 – {28.8539 ln (50)} = 487.123*/
/*    http://bankmodel.blogspot.in/2012/09/a-common-pitfall-of-pdo-points-double.html*/
/*    Factor      =     20/ln(2) = 28.8539 ; */
/*    interpretation of the 20/ln(2) is that for a 20-point increase in score, the odds double.*/
     
      Factor                              =     &pdo./log(2);
      Offset                              =     &base_score.-Factor*log(&odds.);
      LOG_ODD_PDO                   =     Offset + Factor*(LOG_ODD);
/*    score_pdo                     =     exp(LOG_ODD_PDO)/(1+exp(LOG_ODD_PDO));*/
/*    Reverse_Score_pdo       =     1-score_pdo;*/
/*    scale_score_pdo         =     round(score_pdo*1000,1);*/
/*    Reverse_scale_Score_pdo =     round(Reverse_Score_pdo*1000,1);*/
 
run;
 
 
/*proc sql;*/
/*select */
/*min(LOG_ODD)          as min_LOG_ODD,*/
/*max(LOG_ODD)          as max_LOG_ODD,*/
/*min(LOG_ODD_PDO)      as min_LOG_ODD_PDO,*/
/*max(LOG_ODD_PDO)      as max_LOG_ODD_PDO,*/
/*min(score_pdo)        as min_score_pdo,*/
/*max(score_pdo)        as max_score_pdo,*/
/*min(Reverse_Score_pdo)            as min_Reverse_Score_pdo,*/
/*max(Reverse_Score_pdo)            as max_Reverse_Score_pdo,*/
/*min(scale_score_pdo)        as min_scale_score_pdo,*/
/*max(scale_score_pdo)        as max_scale_score_pdo,*/
/*min(Reverse_scale_Score_pdo)            as min_Reverse_scale_Score_pdo,*/
/*max(Reverse_scale_Score_pdo)            as max_Reverse_scale_Score_pdo*/
/*from test;*/
/*quit;*/
 
 
 
/*ods html body = "test.xls";*/
Title "PDO=&pdo. , base_score=&base_score. , odds=&odds.";
proc sql; select min(Factor) as Factor, min(Offset) as Offset from test;quit;
%KS(in_data=test,bad=GBI_Tag,score=LOG_ODD_PDO, bin_ks=1, weight=wgt, no_bin=10);
/*ods html close;*/
 