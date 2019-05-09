
options nocenter macrogen  MFILE symbolgen   mprint  mlogic  merror serror ls=144 ps=77 source2;

/************KS Macro*******************************************************************/;
%macro KS_CALCULATION();

	data &input_data._1;
	set  &input_data.;
		wgt		=&weight.;
		_tot_	=1;
		/*Raw_score_model = &score/1000;*/
		/*
		tot_obs + wgt ;
		if last then do ;call symput('tot_obs',left(put(tot_obs,best15.))) ;end ;
		*/
	run;
  	
	/*	%let size = %sysevalf(&tot_obs/&group,ceil) ;*/

	proc freq data = &input_data._1 noprint;
		tables &depdended_var. / out = BadsCount;
		weight wgt ;
	run;
 
	proc sql noprint; select  count /*(&depdended_var.)*/ into:total_bad  from BadsCount where &depdended_var.=1;quit;
	proc sql noprint; select  count /*(&depdended_var.)*/ into:total_good from BadsCount where &depdended_var.=0;quit;
		%let total   		= %SYSEVALF(&total_bad + &total_good);
		%let tot_bad_rate   = %SYSEVALF(&total_bad./&total.);
		%put &total.;
		%put &total_bad.;
		%put &total_good.; 
		%put &tot_bad_rate. ;

	%if &custom. = 0 %then %do; 
		proc rank data = &input_data._1 (keep = &score. &pd. &depdended_var. wgt _tot_ ) group=&bin. out=data1 ties=mean;
		        var &score.;
		        ranks rank_score;
			run;

		/*
			proc sort data = &input_data._1 ;
				by &score. ;
				run ;
			data &input_data._1 ;
				if _n_ 	   = 1 			then rank_var = 1 ;
			    if &score <= .Z 		then rank_var = &score. ;
			set &input_data._1 end = last ;
			    lagval = lag(&score) ;
			    wobs + wgt ;
			    if ((wobs - wgt) ge &size) and (&score. ne lagval) and (&score. > .Z) then do ;
					rank_var + 1 ;
					wobs = wgt ;
			    end ;
			    if rank_var <= .Z  then rank_var = &var ;
			    keep obs &var weight dv &dv. meandv rank_var ;
			  run ;
		*/

		/* binwise Total, Min, Max  */
		proc summary SUM data = data1 nway;
				class rank_score;
				weight wgt;
				var  &score. &depdended_var. _tot_ ;
				output out = data2  (Drop = _FREQ_ _TYPE_  )
									MIN(&score.) 			= min_score 
									MAX(&score.) 			= max_score 
									SUM(&depdended_var.) 	= nbr_&bad.
									sum(_tot_) 				= Total_bin
									mean(&pd.) 				= exp_bad_rate
									; 
			run;

	%end;
 	%else 	%do;
		proc summary SUM data = &input_data._1 nway;
				class rank_score;
				weight wgt;
				var  &score. &depdended_var. _tot_ ;
				output out = data2  (Drop = _FREQ_ _TYPE_  )
									MIN(&score.) 			= min_score 
									MAX(&score.) 			= max_score 
									SUM(&depdended_var.) 	= nbr_&bad.
									sum(_tot_) 				= Total_bin
									mean(&pd.) 				= exp_bad_rate
									; 
			run;
	%end; 
	data data3;
		set  data2;
			FORMAT observed_&bad._rate expected_&bad._rate PERCENT7.2			;
			nbr_&good. 			= sum(Total_bin, -nbr_&bad.);
			pct_&good.			= nbr_&good./&total_good. 	;
			&good._rate			= nbr_&good/Total_bin		;
			pct_&bad.			= nbr_&bad./&total_bad.		;			
			observed_&bad._rate	= nbr_&bad./Total_bin		;
			expected_&bad._rate = exp_bad_rate				;
			pct_total			= (Total_bin/&total.) 		;
			drop exp_bad_rate;
		run;

	/* sort the data by ascending/descending order */
			%if &order. = 1 %then %do; 
				proc sort data = data3; by rank_score 				; 
				run;
			%end;
	%else 	%if &order. = 2 %then %do; 
				proc sort data = data3; by descending  rank_score 	; 
				run; 
			%end;

    data Lift_Table;
		retain 	rank_score min_score max_score Total_bin nbr_&bad. nbr_&good. observed_&bad._rate expected_&bad._rate
				&good._rate pct_total pct_&bad. pct_&good. cum_pct_&bad. cum_pct_&good. KS 
				Gini ;
	set  data3 ;
		retain cum_pct_&bad.  cum_pct_&good. 0	;
			cum_pct_&bad. 		= sum(cum_pct_&bad. , pct_&bad.)  		;
			cum_pct_&good.		= sum(cum_pct_&good., pct_&good.) 		;
			KS 					= abs(cum_pct_&good. - cum_pct_&bad.)	;
			lag_cum_pct_&bad.	= lag(cum_pct_&bad.)					;
			Gini				= ((sum(cum_pct_&bad.,lag_cum_pct_&bad.))/2)*(pct_total);
		Drop rank_score lag_cum_pct_&bad.;
    run;
	
	proc sql /*noprint*/; 
		create table SUMMARY	as 
				select 					
					max(KS) 						as KS_&input_data. 	FORMAT=PERCENT7.2	,
					abs((sum(Gini)-(0.5))/(0.5)) 	as Gini_index_&input_data.				
				from Lift_Table; 
	quit;

	proc delete data = &input_data._1  data2 data3   BadsCount ; run;

	proc print data = SUMMARY					noobs;	run;
	proc print data = Lift_Table (drop=  Gini)	noobs;	run;

%mend KS_CALCULATION;



data dev;
	set Old_New_Score_Bad;	
		wgt=1;
		where segment='Current' and ci_samp_flag = 'OOT-Dec16';	/* 'dev'  	'OOT-Dec16'		'OOT-Dec17'*/

If Score_New <= 454 then rank_score = 1 ; else				
If 454 < Score_New <= 467 then rank_score = 2 ; else				
If 467 < Score_New <= 477 then rank_score = 3 ; else				
If 477 < Score_New <= 485 then rank_score = 4 ; else				
If 485 < Score_New <= 489 then rank_score = 5 ; else				
If 489 < Score_New <= 501 then rank_score = 6 ; else				
If 501 < Score_New <= 508 then rank_score = 7 ; else				
If 508 < Score_New <= 519 then rank_score = 8 ; else				
If 519 < Score_New <= 528 then rank_score = 9 ; else				
If Score_New > 528 then rank_score = 10 ; 				
				
			

			

 

	run;

/***********************************************************************************************************************/
%let input_data     = dev			; 		/* name of score data data */
%let depdended_var  = cb_achl_bad	;  		/* write name of dependent variable.  (bad=0/1) */
%let good           = good 			;  		/* mention depdended_var=1 refer to good or bad */
%let bad            = bad			;    	/* mention depdended_var=0 refer to good or bad */
%let pd				= pr_old 		; 		/* Probability score 	pr_old			pr_new*/
%let score			= final_score	; 		/* scaled score 		final_score 	Score_New	*/
%let order			= 1				; 		/* 1=ascending, 2=descending */
%let weight 		= wgt			;		/* Weight variable. If no weight then give 1 */
%let bin 			= 10				;		/* No of bin in lift table */
%let custom			= 1			;		/* 1 if lift table in custom score range*/

%KS_CALCULATION()	; 		/* KS calculation */


