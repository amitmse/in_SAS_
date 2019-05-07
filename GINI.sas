

/************KS Macro*******************************************************************/;
%macro GINI_CALCULATION();

	data &input_data._1;
	set  &input_data.;
		wgt		=&weight.;
		_tot_	=1;
	run;

	proc freq data = &input_data._1 noprint;
		tables &depdended_var. / out = BadsCount;
		weight wgt ;
	run;
 
	proc sql noprint; select  count /*(&depdended_var.)*/ into:total_bad from BadsCount where &depdended_var.=1;quit;
	proc sql noprint; select  count /*(&depdended_var.)*/ into:total_good from BadsCount where &depdended_var.=0;quit;
		%let total   		= %SYSEVALF(&total_bad + &total_good);
		%let tot_bad_rate   = %SYSEVALF(&total_bad./&total.);
		%put &total.;
		%put &total_bad.;
		%put &total_good.; 
		%put &tot_bad_rate. ;

	proc summary SUM data = &input_data._1 nway;
			class &score.;
			weight wgt;
			var  &score. &depdended_var. _tot_ ;
			output out = data2  (Drop = _FREQ_ _TYPE_  )
								MIN(&score.) 			= min_score 
								MAX(&score.) 			= max_score 
								SUM(&depdended_var.) 	= nbr_&bad.
								sum(_tot_) 				= Total_bin; 
		run;
 
	data data3;
		set  data2;
			nbr_&good. 	= sum(Total_bin, - nbr_&bad.);
			pct_&good.	= nbr_&good./&total_good. 	;
			&good._rate	= nbr_&good/Total_bin		;
			pct_&bad.	= nbr_&bad./&total_bad.		;
			&bad._rate	= nbr_&bad./Total_bin		;
			pct_total	= (Total_bin/&total.) 		;
		run;

	proc sort data = data3; by &score. 				; 
		run;

    data Lift_Table;
		retain 	min_score max_score Total_bin nbr_&bad. nbr_&good. &bad._rate 
				&good._rate pct_total pct_&bad. pct_&good. cum_pct_&bad. cum_pct_&good. KS 
				Gini ;
	set  data3 ;
		retain cum_pct_&bad.  cum_pct_&good. 0	;
			cum_pct_&bad. 		= sum(cum_pct_&bad. , pct_&bad.)  		;
			cum_pct_&good.		= sum(cum_pct_&good., pct_&good.) 		;
			KS 					= abs(cum_pct_&good. - cum_pct_&bad.)	;
			lag_cum_pct_&bad.	= lag(cum_pct_&bad.)					;
			Gini				= ((sum(cum_pct_&bad.,lag_cum_pct_&bad.))/2)*(pct_total);
		Drop lag_cum_pct_&bad. good_rate &score.;
    run;

	proc sql /*noprint*/; 
		create table _summary_ks_	as 
				select 					
					max(KS) 						as KS_&input_data. 	FORMAT=PERCENT7.2	,
					abs((sum(Gini)-(0.5))/(0.5)) 	as Gini_index_&input_data.									
				from Lift_Table; 
	quit;

	proc delete data = &input_data._1 data2 data3  BadsCount ; run;

	proc print data = _summary_ks_		noobs;	run;
	proc print data = Lift_Table 		noobs;	run;

%mend GINI_CALCULATION;



data dev;
	set cur;	
		wgt=1;
	run;

/***********************************************************************************************************************/

%let input_data     = dev			; 		/* name of score data data */
%let depdended_var  = cb_achl_bad	;  		/* write name of dependent variable.  (bad=0/1) */
%let good           = good 			;  		/* mention depdended_var=1 refer to good or bad */
%let bad            = bad			;    	/* mention depdended_var=0 refer to good or bad */
%let score			= final_score 	; 		/* model_score non_model_score */
%let weight 		= wgt			;		/* Weight variable. If no weight then give 1 */

/***********************************************************************************************************************/

%GINI_CALCULATION()	; 						/* AR calculation */

/***********************************************************************************************************************/
