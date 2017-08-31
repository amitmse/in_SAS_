
options mprint mlogic;

/*ods html body = "dev.xls";*/
/*%KS(in_data=&out.,bad=GBI_Tag,score=Reverse_scale_Score);*/
/*ods html close;*/

%macro KS(in_data, bad, score, bin_ks, weight, no_bin );
%let input_data     = &in_data.;/* Development_scored Validation_scored OOT_scored write name of input data */
%let depdended_var  = &bad.;  	/* write name of dependent variable.  (bad=0/1) */
%let good           = good ;  	/* mention depdended_var=1 refer to good or bad */
%let bad            = bad;    	/* mention depdended_var=0 refer to good or bad */
%let score_variable	= &score. ; /* Reverse_scale_Score scale_score */
%let logic			= &bin_ks.; /* 1=bin wise KS table, rest only max KS */
%let weight 		= &weight;	/* Weight variable. If no weight then give 1 */
%let bin 			= &no_bin;	/* No of bin in lift table */
%put &score_variable. ;

%KS_CALCULATION(&score_variable.); /* KS calculation */
%mend KS;



/* Information value calculation macro */
%macro KS_CALCULATION(var);

data &input_data._1;
set  &input_data.;
	wgt=&weight.;
run;


proc freq data = &input_data._1 noprint;
	tables &depdended_var. / out = BadsCount;
	weight wgt ;
run;
 
proc sql noprint; select  count /*(&depdended_var.)*/ into:total_bad  	from BadsCount where &depdended_var.=1;quit;
proc sql noprint; select  count /*(&depdended_var.)*/ into:total_good  	from BadsCount where &depdended_var.=0;quit;
%let total   = %SYSEVALF(&total_bad + &total_good);
%put &total.;
%put &total_bad.;
%put &total_good.; 

proc rank data = &input_data._1 (keep = &var. &depdended_var. wgt) group=&bin. out=data1 ties=mean;
        var &var.;
        ranks rank_var;
run;
/**/

data data1;
set data1;
	_tot_=1;
run;

/* binwise Total, Min, Max  */
proc summary SUM data = data1 nway;
	class rank_var;
	weight wgt;
	var  &var. &depdended_var. _tot_ ;
	output out = data2  (Drop = _FREQ_ _TYPE_  )
						MIN(&var.) 				= min_var 
						MAX(&var.) 				= max_var 
						SUM(&depdended_var.) 	= nbr_&bad.
						sum(_tot_) 				= Total_bin; 
run;
 

data data2;
set  data2;
	nbr_&good. 	= Total_bin - nbr_&bad.	;
run;

 
    proc sql noprint;
        create table data3 as
            select
                *,
                (nbr_&good./&total_good.)  	as pct_&good.,
                (nbr_&good/Total_bin)   	as &good._rate,
                (nbr_&bad./&total_bad.)   	as pct_&bad.,
                (nbr_&bad./Total_bin)     	as &bad._rate
        from data2;
    quit;

    proc sort data = data3; by rank_var; run;

    data data4;
    set  data3 ;
        by rank_var;
        if first.rank_var;
		retain cum_pct_&bad. cum_pct_&good. cum_lift 0		;
		cum_pct_&bad. 		= cum_pct_&bad. + pct_&bad.  	;
		cum_pct_&good.		= cum_pct_&good.+ pct_&good. 	;
		KS 					= abs(cum_pct_&good. - cum_pct_&bad.);
		pct_total			= (Total_bin/&total.) 				;
		lag_cum_pct_&bad.	= lag(cum_pct_&bad.)			;
		Gini				= ((sum(cum_pct_&bad.,lag_cum_pct_&bad.))/2)*(pct_total);
/*		Lift				= (&bad._rate)*(&tot_bad_rate.) ;*/
/*		cum_lift			= cum_lift+Lift					;*/
/*		Gain_index			= Lift - 1						;*/
/*		cum_gain			= (cum_gain + Gain_index)		;*/
/*		Gain				= abs(cum_gain)					;*/


		label 
			rank_var 		=	'Rank' 
			min_var			=	'Min Score'
			max_var			=	'Max Score'
			pct_total		=	'% Total'
			nbr_&good.		=	"# &good."
			&good._rate		=	"&good. rate"
			pct_&good.		=	"% &good."
			cum_pct_&good.	=	"Cumulative % &good."
			nbr_&bad.		=	"# &bad."
			&bad._rate		=	"&bad. rate"
			pct_&bad.		=	"% &bad."
			cum_pct_&bad.	=	"Cumulative % &bad."
;
    run;
/* GINI INDFEX */
/*	proc sql; select ((sum(Gini)-(0.5))/(0.5)) as Gini_index from data4; quit;*/

	proc sql; select max(KS) as KS FORMAT=PERCENT7.2 from data4; quit;

    proc sql noprint;
        create table KS as select
            rank_var		Format=Comma20.0,
            min_var			Format=Comma20.0,
            max_var			Format=Comma20.0,
			Total_bin		Format=Comma20.0 as Total,
			nbr_&bad.		Format=Comma20.0,
			nbr_&good.		Format=Comma20.0,
			&bad._rate		FORMAT=PERCENT7.2,
			&good._rate		FORMAT=PERCENT7.2,
			pct_total		FORMAT=PERCENT7.2,
            pct_&bad.		FORMAT=PERCENT7.2,
            pct_&good. 		FORMAT=PERCENT7.2,
			cum_pct_&bad.  	FORMAT=PERCENT7.2,
			cum_pct_&good. 	FORMAT=PERCENT7.2,
			KS 				FORMAT=PERCENT7.2
/*			Lift,*/
/*			cum_lift*/
/*			cum_gain,*/
/*			gain*/

    from data4;
    quit;

%if &logic. = 1 %then %do;
	proc print data = KS;	run;
%end;

proc delete data = &input_data._1 data1 data2 data3 data4 KS BadsCount; run;

%mend KS_CALCULATION;
