

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

/*Concordance method */
/*
%Macro Concordant();
		data good(keep=good_score) bad(keep=bad_score);
			set &Input.;
				if &Bad_Variable. = 1 then bad_score  = &score_Variable.; 
				if &Bad_Variable. = 0 then good_score = &score_Variable.;
				if &Bad_Variable. = 1 then output bad;
				if &Bad_Variable. = 0 then output good;
		run;

		proc sql; create table CART_0 as select a.*, b.* from good as a ,bad as b; quit;

		data CART_0;
			set CART_0;
				length hit $32;
				if good_score > bad_score then hit = 'Concordant';else
				if good_score < bad_score then hit = 'Discordant';else
				if good_score = bad_score then hit = 'tied';
			run;

		proc freq data = CART_0 noprint;
			tables  hit    /out=cart_1;
			run;

		proc transpose data=cart_1 out=cart_2 (drop=_LABEL_ _NAME_ ); var PERCENT; id hit; run;

		data cart;
			set cart_2;
				FORMAT SOMERS_D PERCENT7.2;
				AUC 		=   (Concordant/100) + (0.5*(Tied/100)) 	;
				SOMERS_D 	=2*((Concordant/100) + (0.5*(Tied/100))) - 1	;
			run;

		proc delete data = good bad CART_0 cart_1 cart_2;run;

		proc print data=cart;
			var SOMERS_D;
			run;
%Mend;
*/
%let Input 		= test 		; /*input data*/
%let Bad_Variable	= cb_achl_bad	; /*Bad Variable Name */
%let score_Variable	= Score_New	; /*Score variable name. High score refer to Good customer. 
					 	if otherwise then update the first part of code */
%Concordant();

/******************************************************************************************/
%macro concdisc(data=, event=, nonevent=, response=);
    %global n;
    proc sql;reset noprint;
					select count(*) into :N from &data.;	/* number of observations in original data set into macro variable N */
        create table _nonevents as 	select pbin as pbin0 	from &data. where &response=&nonevent; /* create data set of nonevents */
        create table _events 	as 	select pbin as pbin1 	from &data. where &response=&event; /* create data set of events */
	/* create data set of all event-nonevent pairs and determine concordance */
	create table _pairs 	as 	select *, (pbin1>pbin0) as Concordant,(pbin1=pbin0) as Tied,(pbin0>pbin1) as Discordant 
								from _nonevents, _events;
	quit;
%mend;

/*%concdisc(data=out, event=1, nonevent=0, response=outcome);*/



