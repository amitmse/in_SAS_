

/************KS Macro*******************************************************************/;
%macro KS_CALCULATION(score);

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
 
	proc sql noprint; select  count /*(&depdended_var.)*/ into:total_bad from BadsCount where &depdended_var.=1;quit;
	proc sql noprint; select  count /*(&depdended_var.)*/ into:total_good from BadsCount where &depdended_var.=0;quit;
		%let total   		= %SYSEVALF(&total_bad + &total_good);
		%let tot_bad_rate   = %SYSEVALF(&total_bad./&total.);
		%put &total.;
		%put &total_bad.;
		%put &total_good.; 
		%put &tot_bad_rate. ;

	proc rank data = &input_data._1 (keep = &score. &depdended_var. wgt _tot_) group=&bin. out=data1 ties=mean;
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
								sum(_tot_) 				= Total_bin; 
		run;
 
	data data3;
		set  data2;
			nbr_&good. 	= sum(Total_bin, -nbr_&bad.);
			pct_&good.	= nbr_&good./&total_good. 	;
			&good._rate	= nbr_&good/Total_bin		;
			pct_&bad.	= nbr_&bad./&total_bad.		;
			&bad._rate	= nbr_&bad./Total_bin		;
			pct_total	= (Total_bin/&total.) 		;
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

    data data4;
		retain 	rank_score min_score max_score Total_bin nbr_&bad. nbr_&good. &bad._rate 
				&good._rate pct_total pct_&bad. pct_&good. cum_pct_&bad. cum_pct_&good. KS 
				Gini ;
	set  data3 ;
		retain cum_pct_&bad.  cum_pct_&good. 0	;
			cum_pct_&bad. 		= sum(cum_pct_&bad. , pct_&bad.)  		;
			cum_pct_&good.		= sum(cum_pct_&good., pct_&good.) 		;
			KS 					= abs(cum_pct_&good. - cum_pct_&bad.)	;
			lag_cum_pct_&bad.	= lag(cum_pct_&bad.)					;
			Gini				= ((sum(cum_pct_&bad.,lag_cum_pct_&bad.))/2)*(pct_total);
			lag_&bad._rate 		= lag(&bad._rate)						;
		        if _n_=1 then inc_monotonic =0;
        else    if lag_&bad._rate > &bad._rate then inc_monotonic= 1; else inc_monotonic=99;
                if _n_=1 then dec_monotonic =0;
        else    if lag_&bad._rate < &bad._rate then dec_monotonic= 1; else dec_monotonic=99;
		Drop rank_score lag_cum_pct_&bad. lag_&bad._rate;
    run;

	proc sql noprint; 
		select sum(inc_monotonic), sum(dec_monotonic) 
						into :sum_inc_monotonic, :sum_dec_monotonic  
		from data4; 
	Quit;

    data Lift_Table;
    set  data4;
		Format Break_Risk_Rank 4.;
			if 	&sum_inc_monotonic. <=10 or &sum_dec_monotonic. <=10 then Break_Risk_Rank=0;
			else 														  Break_Risk_Rank=1;
		drop inc_monotonic dec_monotonic;
    run;
	
	proc sql /*noprint*/; 
		create table _summary_ks_	as 
				select 					
					max(KS) 						as KS_&input_data. 	FORMAT=PERCENT7.2	,
					abs((sum(Gini)-(0.5))/(0.5)) 	as Gini_index_&input_data.				,					
					case when sum(Break_Risk_Rank) >0 then 'Yes' else 'No' end as Risk_rank_break_&input_data.
				from Lift_Table; 
	quit;

	proc delete data = &input_data._1 data1 data2 data3 data4  BadsCount ; run;

	proc print data = _summary_ks_				noobs;	run;
	proc print data = Lift_Table (drop=  Gini)	noobs;	run;

%mend KS_CALCULATION;



data dev;
	set App_score_perf_nodup;	
		wgt=1;
		where Industry = 'Govt';
		keep bad wgt &keep_var.;
	run;

/***********************************************************************************************************************/
%let input_data     = dev			; 		/* name of score data data */
%let depdended_var  = bad			;  		/* write name of dependent variable.  (bad=0/1) */
%let good           = good 			;  		/* mention depdended_var=1 refer to good or bad */
%let bad            = bad			;    	/* mention depdended_var=0 refer to good or bad */
%let score_variable	= n_score_cb 	; 		/* model_score non_model_score */
%let order			= 1				; 		/* 1=ascending, 2=descending */
%let weight 		= wgt			;		/* Weight variable. If no weight then give 1 */
%let bin 			= 5				;		/* No of bin in lift table */

%KS_CALCULATION(&score_variable.)	; 		/* KS calculation */
