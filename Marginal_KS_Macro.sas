
options mprint mlogic merror serror source2 ;

/***************************************************************************************/;
/****** Macro FOR developing logistic model with Lift , VIF & Correlation tables********/;
/***************************************************************************************/;
/************KS Macro*******************************************************************/;

%macro KS_CALCULATION(input_data);

	%let good = good ;  	
	%let bad  = bad; 

	proc freq data = &input_data. noprint;
		tables &dep_var. / out = BadsCount;
		weight wgt ;
	run;
	proc sql noprint; select count into:total_bad from BadsCount where &dep_var.=1;quit;
	proc sql noprint; select count into:total_good from BadsCount where &dep_var.=0;quit;
		%let total   		= %SYSEVALF(&total_bad + &total_good);
		%put &total.;
		%put &total_bad.;
		%put &total_good.; 

	proc rank data = &input_data. (keep = model_score &dep_var. wgt _tot_) 
							group=10 out=data1 ties=mean;
	        var model_score;
	        ranks rank_score;
	run;

/* binwise Total, Min, Max  */
	proc summary SUM data = data1 nway;
		class rank_score;
		weight wgt;
		var  model_score &dep_var. _tot_ ;
		output out = data2  (Drop = _FREQ_ _TYPE_  )
							MIN(model_score) 		= min_score 
							MAX(model_score) 		= max_score 
							SUM(&dep_var.) 			= nbr_&bad.
							sum(_tot_) 				= Total_bin; 
	run;
 
	proc sort data = data2; by rank_score ; run;

	data data3;
	set  data2;
		by rank_score ;
		retain cum_pct_&bad.  cum_pct_&good. 0	;
			nbr_&good. 			= sum(Total_bin, -nbr_&bad.);
			pct_&good.			= nbr_&good./&total_good. 	;
			pct_&bad.			= nbr_&bad./&total_bad.		;
			cum_pct_&bad. 		= sum(cum_pct_&bad. , pct_&bad.)  		;
			cum_pct_&good.		= sum(cum_pct_&good., pct_&good.) 		;
			KS 					= abs(cum_pct_&good. - cum_pct_&bad.)	;
	run;

	proc sql /*noprint*/; 
		create table _summary_ks_	as 
				select 
					"&drop_var."				as Variable length 250,
					max(KS) 					as KS_&input_data. 	FORMAT=PERCENT7.2
				from data3; 
	quit;
	proc append data = _summary_ks_ 	base= Marginal_KS force; 	run;

	proc delete data = data1 data2 data3 _summary_ks_ BadsCount; run;

%mend KS_CALCULATION;
/***** END OF KS Macro ****************************************************************/;
/***************************************************************************************/;
/******Scoring Macro*******************************************************************/;
%macro score(indata);
/* if dataset name is blank then it will not run */;
	%if &indata. ne  %then %do;
	/* score calculation */;
		data &indata._scored;
		set &indata.;
				LOG_ODD= &final_equation.;
				Raw_score_model		=exp(log_odd)/(1+exp(log_odd));
				Raw_score_non_model	=1-Raw_score_model;
			format model_score non_model_score 10.;
				model_score 		= round(Raw_score_model*1000,1);
				non_model_score		= round(Raw_score_non_model*1000,1);
		run;
		/* generating the lift table*/;
		Title "&indata."; 
			%KS_CALCULATION(input_data=&indata._scored);
	%end;
%mend score;
/***END OF Scoring Macro**************************************************************/;
/**************************************************************************************/;

%macro Marginal_KS_Calculation();

	/* generate parameter estimate in dataset */
	ods output 	parameterestimates	=	estimate; 
		proc logistic data=dev_test_ 	DESCENDING 	namelen=32	;
			model &dep_var. = &keep_var. /lackfit;
			Weight wgt;
		run;


	/* use the estimate data to get the model equation */;
	data 	estimate1;
	set 	estimate end=eof;
		format equation $250.;
					if _N_ =1 	then equation="("||Estimate||")+";
			else 	if eof =1 	then equation=trim(Variable)||"*("||trim(Estimate)||")";
			else 	if _N_ >1 	then equation=trim(Variable)||"*("||trim(Estimate)||")+";
	run;

	proc sql noprint; select count(*) into: nbr_var from estimate1;quit; 
	%put total number of variables in model: &nbr_var.;

	proc transpose data=estimate1 out=estimate2 (drop=_NAME_); var equation; run;

	data estimate3;
	set  estimate2;
		last_var=compress("col"||&nbr_var.);
	run;

	proc sql noprint; select last_var into: last_var from estimate3;quit;	
		%put last variable of model: &last_var.;

	data 	estimate4;
	set 	estimate3;
		format final_eq $3050.;
			final_eq = catx('', of col1 - &last_var.);
	run;
	/* final model equation  for scoring logic */;
	proc sql noprint; select final_eq into: final_equation from estimate4;quit; 	
		%put Logistic Equation: &final_equation.;
	/* delete the dataset */;
	proc delete data = 	estimate estimate1 estimate2 estimate3 estimate4 ; run;
/*****score***********************************************************************/;
	%score(dev_test_);
%mend Marginal_KS_Calculation;
/*************************************************************************************/;
/************************ Macro End **************************************************/;
/*************************************************************************************/;

%Macro Marginal_KS();
	data dev_test_;
	set  &dev_data. ;
		wgt=&weight_var.;
		_tot_	=1;
	run;

	%if %sysfunc(exist(varlist_data)) %then %do;
		proc delete data = varlist_data; run;
	%end;

    %do i = 1 %to &no_of_model_var.; 						
        %let Model_Var = %scan(&Model_Var_List., &i.); 	
        %put &Model_Var.; 
		data a_&i.;
		length varlist $32.;
		varlist="&Model_Var.";
		run;
		proc append data = a_&i. base = varlist_data force;run;
		proc delete data = a_&i.; run;
	%end;
	proc print data = varlist_data;run;

	%do i = 1 %to &no_of_model_var.; 
		data drop_var  keep_var;
		set  varlist_data;
			if _N_ = &i. then output drop_var; else output keep_var;
		run;
		proc sql; select varlist 	into: drop_var 	from drop_var ;quit; 
			%put drop variable &&drop_var;
		proc sql; select varlist 	into: keep_var 	from keep_var ;quit; 
			%put keep variable &&keep_var;
		proc delete data = drop_var  keep_var; run;
		%Marginal_KS_Calculation();
	%end;
%Mend Marginal_KS;
/***********************************************************************************/

%let dev_data		=dev ;
%let dep_var 		=GBI_Tag ;
%Let weight_var		=wgt;
%Let no_of_model_var=7;

/* List of model variable */
%let Model_Var_List= 
					PRODUCT_TYPE_WOE
					Prof_Grp_WOE
					INSURANCE_TYPE1_WOE
					APPLICANT_AGE_WOE
					STATE_WOE
					LTV_WOE
					EMI_WOE
;

%Marginal_KS;
