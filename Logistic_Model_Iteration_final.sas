/********************************************************************************************/;
/****** Macro For developing logistic model with Lift table, VIF & Correlation tables********/;
/********************************************************************************************/;
options mprint mlogic merror serror source2  linesize=256;
/************KS Macro************************************************************************/;

%macro KS(in_data, bad, score, scr_order,  bin_ks, weight, no_bin );
		%let input_data         = &in_data.; 	/* name of score data data */
		%let depdended_var      = &bad.;  	/* write name of dependent variable.  (bad=0/1) */
		%let good               = good ;  	/* mention depdended_var=1 refer to good or bad */
		%let bad                = bad;    	/* mention depdended_var=0 refer to good or bad */
		%let score_variable	= &score. ; 	/* model_score non_model_score */
		%let order		= &scr_order. ; /* 1=ascending, 2=descending */
		%let weight 		= &weight;	/* Weight variable. If no weight then give 1 */
		%let bin 		= &no_bin;	/* No of bin in lift table */
		%put &score_variable. ;
		%KS_CALCULATION(&score_variable.); 	/* KS calculation */
%mend KS;

/**/
%macro KS_CALCULATION(score);

	data &input_data._1;
	set  &input_data.;
		wgt		=&weight.;
		_tot_	=1;
	/*	scale_score = round(score*1000,1);*/
	run;



	proc freq data=&input_data._1 noprint;
			tables Raw_score_model*&bad_var.  / measures;
			output out=somersd(keep=_SMDRC_)	 smdrc;
			run;
	
	proc sql noprint; select  _SMDRC_ into: Somers_D  from somersd;quit;

	proc freq data = &input_data._1 noprint;
		tables &depdended_var. / out = BadsCount;
		weight wgt ;
	run;
 
	proc sql noprint; select  count /*(&depdended_var.)*/ into:total_bad  	
								from BadsCount where &depdended_var.=1;quit;
	proc sql noprint; select  count /*(&depdended_var.)*/ into:total_good  	
								from BadsCount where &depdended_var.=0;quit;
		%let total   		= %SYSEVALF(&total_bad + &total_good);
		%let tot_bad_rate   = %SYSEVALF(&total_bad./&total.);
		%put &total.;
		%put &total_bad.;
		%put &total_good.; 
		%put &tot_bad_rate. ;


	proc rank data = &input_data._1 (keep = &score. &depdended_var. wgt _tot_) 
							group=&bin. out=data1 ties=mean;
	        var &score.;
	        ranks rank_score;
	run;

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

    data data5;
    set  data4;
		Format Break_Risk_Rank 4.;
			if 	&sum_inc_monotonic. <=10 or &sum_dec_monotonic. <=10 then Break_Risk_Rank=0;
			else 														  Break_Risk_Rank=1;
		drop inc_monotonic dec_monotonic;
    run;
	
	proc sql /*noprint*/; 
		create table _summary_ks_	as 
				select 
					&iteratation.					as Model length 5						,
					max(KS) 						as KS_&input_data. 	FORMAT=PERCENT7.2	,
					abs((sum(Gini)-(0.5))/(0.5)) 	as Gini_index_&input_data.				,
					&Somers_D						as Somers_D_&input_data.				,
					case when sum(Break_Risk_Rank) >0 then 'Yes' else 'No' end as Risk_rank_break_&input_data.
				from data5; 
	quit;

	data 	_STAT_	;
	merge 	_STAT_ 		_summary_ks_;
		by Model;
	run;

	proc print data = data5 (drop=  Gini)	noobs;	run;
	proc delete data = &input_data._1 data1 data2 data3 data4 data5 _summary_ks_ BadsCount somersd; 
	run;

%mend KS_CALCULATION;
/**************************************************************************************/;
/***** END OF KS Macro ****************************************************************/;
/***************************************************************************************/;
/******Scoring Macro*******************************************************************/;
/**************************************************************************************/;
/* macro for calculating the score and generating the lift table*/
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
		Title "KS Table: &indata. - model_score"; 
			%KS(in_data		=&indata._scored	,	
				bad			=&bad_var.			,	
				score		=model_score		, 
				scr_order	=&sort_score.		,
				weight		=&weight_var. 		, 
				no_bin		=&ks_bin.
			);
		Title "KS Table: &indata. - non_model_score"; 
			%KS(in_data		=&indata._scored	,	
				bad			=&bad_var.			,	
				score		=non_model_score	, 
				scr_order	=&sort_score.		,
				weight		=&weight_var. 		, 
				no_bin		=&ks_bin.
			);
			Proc delete data = &indata._scored;	run;
		%end;
	
%mend score;
/**************************************************************************************/;
/***END OF Scoring Macro**************************************************************/;
/**************************************************************************************/;

%macro _model_(stepwise_info);
	/* generate parameter estimate in dataset */
	ods output 	parameterestimates	=	estimate
				Association 		= 	test0					; 
		proc logistic data=dev_test_ 	&model. 	namelen=32	;
			model &bad_var. = &model_var. / &stepwise_info. lackfit;
			Weight wgt;
		run;
	proc sql noprint; select distinct variable into: logistic_var SEPARATED by ' ' from estimate where variable ne "Intercept"; quit;
	%put &logistic_var.;
			%global model_var; 
			%let model_var=&logistic_var.;
			%put &model_var.;

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

	%if %sysfunc(exist(test0)) %then %do;
			proc transpose data=test0 out=test1; var cValue1; id label1; run;
			proc transpose data=test0 out=test2; var cValue2; id Label2; run;

			data 	test1;
			set 	test1;
				Length Model 5.;
					Model= &iteratation.;
				drop _NAME_;
			run;

			data 	test2;
			set 	test2;
				Length Model 5.;
					Model= &iteratation.;
				drop _NAME_;
			run;

			data test12;
			  retain 	Model Percent_Concordant Percent_Discordant Percent_Tied 
						Pairs Somers__D Gamma Tau_a c Total_Variable;
			merge test1 test2;
					Format Total_Variable 12.;
						Total_Variable=(&nbr_var.-1);
					Length Model_Variables $560.;
						Model_Variables = "&model_var.";
			by Model;
					label 
						Model				=	'Model'
						Percent_Concordant 	= 	'Percent Concordant'
						Percent_Discordant 	= 	'Percent Discordant'
						Percent_Tied		=	'Percent Tied'
						Pairs				=   'Pairs'
						Somers__D			=	'Somers D'
						Gamma				=	'Gamma'
						Tau_a				=	'Tau-a'
						c					=	'AUC/C-Stat/Area under curve'
						Total_Variable		=   'Total Number of variables in the Model'
					;
			run;
	%end;
	%Else %do;
			data 	test12;
				Length Model 5.;
					Model= &iteratation.;
				Length Percent_Concordant Percent_Discordant Percent_Tied Pairs $9.;
					Percent_Concordant 	= ' ';
					Percent_Discordant 	= ' ';
					Percent_Tied 		= ' ';
					Pairs				= ' ';
				Length Somers__D Gamma Tau_a c $5.;
					Somers__D			= ' ';
					Gamma 				= ' ';
					Tau_a				= ' ';
					c					= ' ';
				Format Total_Variable 12.;
					Total_Variable		= (&nbr_var.-1);
				Length Model_Variables $560.;
					Model_Variables = "&model_var.";
			run;
	%End;
	proc append data = test12 	base= _STAT_ force; 	run;
	/* delete the dataset */;
	proc delete data = 	estimate1 estimate2 estimate3 test0 test1 test2 test12 ; 	run;

/********score******************************************************************************/;
	%score(&dev_data.);
	%score(&val1_data.);
	%score(&val2_data.);
	%score(&val3_data.);

	/* VIF */; 			
	ods output 	parameterestimates=vif (keep=variable VarianceInflation);
		proc reg data=&dev_data.; 	
			model &bad_var. = 	&model_var./ VIF; 
			weight &weight_var.;
		run; quit;

	/* Correlation with Bad/dependend variable*/ 	
		proc corr data=&dev_data. NOSIMPLE NOPROB noprint	outp=bad_corr; 	
			var	&model_var. ; 
			weight &weight_var.;	
			with &bad_var.;
		run; quit;
		data bad_corr;
		set bad_corr;
			where _TYPE_ in ("CORR");
			drop _TYPE_ _NAME_;
		run;
		proc transpose data=bad_corr  out=bad_corr1 (rename =(COL1=Correlation_with_bad)); run;

	/* Correlation among model variables */
		proc corr data=&dev_data. NOSIMPLE NOPROB noprint outp=corr; 	
			var	&model_var. ; 
			weight &weight_var.;	
		run; quit;
		data corr;
		set corr;
			where _TYPE_ in ("CORR");
			drop _TYPE_ ;
			rename _NAME_=corr_var;
		run;

	proc sql;
		create table _model_summary_ as 
			select
					a.*,
					b.VarianceInflation,
					e.Autobin_Info_val,
					c.Correlation_with_bad,
					d.*
					from estimate 			as a
					left join vif 			as b on a.Variable = b.Variable
					left join bad_corr1		as c on a.Variable = c._Name_
					left join corr			as d on a.variable = d.corr_var
					left join summ.overall	as e on a.variable = trim(e.Var)||"W"
					order by a.WaldChiSq desc;
	quit;

	data _model_summary_ ;
	set  _model_summary_  (drop=corr_var);
		if Estimate < 0 then Negative_Estimate=0;else Negative_Estimate=1;
	run;

	proc append data = _STAT_ 	base=summ._MODEL_STAT_ force; 			run;
	proc sort 	data = summ._MODEL_STAT_ NODUPRECS; by Model; 			run;
	Title "Model Iteratation-Summary";
	proc print 	data = summ._MODEL_STAT_ noobs; 						run;
	proc sql ; 	select final_eq as Scoring_Logic from estimate4;		quit; 
	Title "Model-Summary";
	Proc print data = _model_summary_(drop=Negative_Estimate)	noobs;	run;
	Proc delete data = _STAT_  estimate estimate4 vif bad_corr bad_corr1 corr  ;run;
	%global total_var MAX_VIF MAX_P	MIN_WaldChiSq Dis_Esti_Neg; 
	proc sql noprint; select count(Variable) into: total_var from _model_summary_	where variable ne "Intercept";
	proc sql noprint; 
		select 
			(round(max(VarianceInflation),0.001)-0.001), 
			(round(max(ProbChiSq),0.001)-0.001),
			(round(MIN(WaldChiSq),0.001)+0.001),
			 count(distinct Negative_Estimate)
			into: MAX_VIF	, : MAX_P	, : MIN_WaldChiSq	, :Dis_Esti_Neg
		from _model_summary_;
	quit; 
		%put--Model iteratation:%trim(&iteratation.)--total var:%trim(&total_var.)--MAX VIF:%trim(&MAX_VIF.)--
		    --MAX P-value:%trim(&MAX_P.)--MIN Wald ChiSq:%trim(&MIN_WaldChiSq.)--Distinct Estimate:%trim(&Dis_Esti_Neg.)--;
%mend _model_;
/*****************************************************************************************************/

	%macro VIF_CHECK();
		proc sql noprint; 
			select Variable into: MAX_VIF_VAR from _model_summary_ where VarianceInflation ge &MAX_VIF.;
		quit; 

		%put &MAX_VIF_VAR.;

		data vif_chk;
		set _model_summary_;
			keep Variable WaldChiSq VarianceInflation Autobin_Info_val &MAX_VIF_VAR.;
			if &MAX_VIF_VAR. = 1 then &MAX_VIF_VAR. =.; else &MAX_VIF_VAR. = abs(&MAX_VIF_VAR.);
		run;
		Proc sql noprint; select (round(max(&MAX_VIF_VAR.),0.0001)-0.0001) into: MAX_CORR from vif_chk; quit;
		%PUT &MAX_CORR.;
		proc sql noprint; 
			select Variable into: MAX_CORR_VAR from vif_chk where &MAX_VIF_VAR. ge &MAX_CORR.;
		quit; 
		%PUT &MAX_CORR_VAR.;

		data vif_corr_chk;
		set vif_chk;
			keep Variable WaldChiSq VarianceInflation Autobin_Info_val &MAX_VIF_VAR.;
			where Variable in ("&MAX_VIF_VAR." , "&MAX_CORR_VAR.");
		run;
		proc sql noprint; 
			select (round(min(Autobin_Info_val),0.00001)+0.00001) into: MIN_INFO_VAL from vif_corr_chk;
		quit;
		%PUT &MIN_INFO_VAL.;
		proc sql noprint; 
			select Variable into: MIN_INFO_VAL_VAR from vif_corr_chk 
				where Autobin_Info_val le &MIN_INFO_VAL.;
		quit;
		%PUT &MIN_INFO_VAL_VAR.;
		data drop_var;
		set vif_corr_chk (keep = Variable);
			where Variable in ("&MIN_INFO_VAL_VAR.");
		run;
		proc sort data = _model_summary_ (keep=variable) nodupkey out= model_var;
			by variable;
			where variable ne "Intercept";
		run;
		proc sort data = drop_var (keep=variable) nodupkey out= model_var_drop;
			by variable;
			where variable ne "Intercept";
		run;
		data model_var_remain;
		merge model_var(in=a)  model_var_drop(in=b);
			by variable;
			if a and not b;
		run;		 
		proc sql noprint; select count(variable) into: TOT_VIF_VAR from model_var_remain ; quit;
		%PUT &TOT_VIF_VAR.;
		proc sql noprint; select distinct variable into: VIF_VAR SEPARATED by ' ' from model_var_remain; quit;
			%put &VIF_VAR.;
			%global model_var; 
			%let model_var=&VIF_VAR.;
			%put &model_var.;
		data drop_var;
		set drop_var;
			Model=&iteratation.;
			Length Drop_Variables $50.;
			Drop_Variables=variable;
			drop Variable;
		run;
		Proc append data = drop_var base= All_drop_var force; run;
		proc delete data = vif_chk vif_corr_chk drop_var model_var model_var_drop model_var_remain;run;
		/*Logistic model after removing VIF*/
		%_model_();
	%mend VIF_CHECK;
/******************************************************************************************************************/
	%macro P_value();
		proc sql; 
			create table _p_value_wald_ as 
				select Variable from _model_summary_  
				where ProbChiSq ge &MAX_P. and WaldChiSq le &MIN_WaldChiSq.;	
		quit;
		%if %sysfunc(exist(_p_value_wald_)) %then %do;
			data drop_P_var;
			set _p_value_wald_;
			run;
			proc delete data = _p_value_wald_ ; run; 
		%end;
		%else %do;
			proc sql ; 
				create table drop_P_var as 
					select Variable from _model_summary_ where ProbChiSq ge &MAX_P.; 
			quit; 
		%end;

		proc sort data = _model_summary_ (keep=variable) nodupkey out= model_P_var;
			by variable;
			where variable ne "Intercept";
		run;
		proc sort data = drop_P_var (keep=variable) nodupkey out= model_drop_P_var;
			by variable;
			where variable ne "Intercept";
		run;
		data model_P_var_remain;
		merge model_P_var(in=a)  model_drop_P_var(in=b);
			by variable;
			if a and not b;
		run;
		 
		proc sql noprint; select count(variable) into: TOT_P_VAR from model_P_var_remain ; quit;
		%PUT &TOT_P_VAR.;
		proc sql noprint; select distinct variable into: P_VAR SEPARATED by ' ' from model_P_var_remain; quit;
			%put &P_VAR.;
			%global model_var; 
			%let model_var=&P_VAR.;
			%put &model_var.;
		data model_drop_P_var;
		set  model_drop_P_var;
			Model=&iteratation.;
			Length Drop_Variables $50.;
			Drop_Variables=variable;
			drop Variable;
		run;
		Proc append data = model_drop_P_var base= All_drop_var force; run;
		proc delete data = drop_P_var model_P_var  model_drop_P_var model_P_var_remain ;run;
		/*Logistic model after removing VIF*/
		%_model_();
	%mend P_value;
/******************************************************************************************************************/

	%macro ESTIMATE_SIGN_CHECK();
		proc sql;
			create table SIGN_CHECK as 
				select *, count(Negative_Estimate) as nbr_Estimate from _model_summary_ group by Negative_Estimate; 
		quit;

		proc sql noprint; select min(nbr_Estimate) into: MIN_ESTIMATE_SIGN from SIGN_CHECK;	quit; 
		%put &MIN_ESTIMATE_SIGN.;

		data SIGN_CHECK1;
		set  SIGN_CHECK;
			keep Variable WaldChiSq VarianceInflation Autobin_Info_val nbr_Estimate;
			if nbr_Estimate = &MIN_ESTIMATE_SIGN.;
		run;

		Proc sql noprint; select (round(min(WaldChiSq),0.0001)+0.0001) into: MIN_WaldChiSq_Sign from SIGN_CHECK1; quit;
		%PUT &MIN_WaldChiSq_Sign.;

		data drop_var;
		set SIGN_CHECK1 ;
			keep  Variable;
			where WaldChiSq le &MIN_WaldChiSq_Sign.;
		run;
		proc sort data = _model_summary_ (keep=variable) nodupkey out= model_var;
			by variable;
			where variable ne "Intercept";
		run;
		proc sort data = drop_var (keep=variable) nodupkey out= model_var_drop;
			by variable;
			where variable ne "Intercept";
		run;
		data model_var_remain;
		merge model_var(in=a)  model_var_drop(in=b);
		by variable;
		if a and not b;
		run;
		 
		proc sql noprint; select count(variable) into: TOT_SIGN_VAR from model_var_remain ; quit;
		%PUT &TOT_SIGN_VAR.;
		proc sql noprint; select distinct variable into: SIGN_VAR SEPARATED by ' ' from model_var_remain; quit;
			%put &SIGN_VAR.;
			%global model_var; 
			%let model_var=&SIGN_VAR.;
			%put &model_var.;
		data model_var_drop;
		set  model_var_drop;
			Model=&iteratation.;
			Length Drop_Variables $50.;
			Drop_Variables=variable;
			drop Variable;
		run;
		Proc append data = model_var_drop base= All_drop_var force; run;
		proc delete data =  SIGN_CHECK SIGN_CHECK1 drop_var model_var model_var_drop model_var_remain;run;
		/*Logistic model after removing VIF*/
		%_model_();
	%mend ESTIMATE_SIGN_CHECK;
/******************************************************************************************************************/
	%macro NUMBER_OF_MODEL_VAR();
		data drop_var;
		set _model_summary_;
			keep  Variable;
			where WaldChiSq le &MIN_WaldChiSq.;
		run;
		proc sort data = _model_summary_ (keep=variable) nodupkey out= model_var;
			by variable;
			where variable ne "Intercept";
		run;
		proc sort data = drop_var (keep=variable) nodupkey out= model_var_drop;
			by variable;
			where variable ne "Intercept";
		run;
		data model_var_remain;
		merge model_var(in=a)  model_var_drop(in=b);
		by variable;
		if a and not b;
		run;
		 
		proc sql noprint; select count(variable) into: TOT_NO_MODEL_VAR from model_var_remain ; quit;
		%PUT &TOT_NO_MODEL_VAR.;
		proc sql noprint; select distinct variable into: TOT_MODEL_VAR SEPARATED by ' ' from model_var_remain; quit;
			%put &TOT_MODEL_VAR.;
			%global model_var; 
			%let model_var=&TOT_MODEL_VAR.;
			%put &model_var.;
		data model_var_drop;
		set  model_var_drop;
			Model=&iteratation.;
			Length Drop_Variables $50.;
			Drop_Variables=variable;
			drop Variable;
		run;
		Proc append data = model_var_drop base= All_drop_var force; run;
		proc delete data =  drop_var model_var model_var_drop model_var_remain;run;
		/*Logistic model after removing VIF*/
		%_model_();
	%mend NUMBER_OF_MODEL_VAR;
/******************************************************************************************************************/

%macro Logistic_model(	);

		proc printto log="&output.Log_Logistic_Model_Iteration.txt";
		libname summ "&infoval_data_path.";

		data dev_test_;
		set  &dev_data;
			wgt=&weight_var.;
		run;
		%global model_var; 
		%let model_var=&varlist.;
		%put &model_var.;
/****************************************************************************************************/
/****************************************************************************************************/
			%if &excel. =2 %then %do; ods tagsets.excelxp file = "&output.Logistic_Model_Iteration.xls";%end;
/****************************************************************************************************/
/* Model iteratation for stepwise */
/****************************************************************************************************/
	%if &stepwise. = 1 %then %do;
			/*--------------------------------*/
				%if &excel. =1 %then %do; ods html file = "&output.1.Model Iteration-Stepwise.xls"; %end;
				%if &excel. =2 %then %do; ods tagsets.excelxp options(orientations = 'Landscape' Fitpage='yes' 
					pages_fitwidth ='1' pages_fitheight='100' foreground = 'whilte' font_size = "xx-small" 
					sheet_name= "1.Model Iteration-Stepwise");
				%end;
			/*--------------------------------*/
		%global iteratation ; 
		%let iteratation = 1;
		/* logistic model*/
		%_model_(stepwise_info=stepwise sls=&exit. sle=&entry.);
			/*--------------------------------*/
				%if &excel. =1 %then %do; ods html close; %end;
	%end;
/****************************************************************************************************/
/* Model iteratation - First */
/****************************************************************************************************/
	%else %do;
		%global iteratation ; 
		%let iteratation = 1;
		/*--------------------------------*/
			%if &excel. =1 %then %do; ods html file = "&output.&iteratation..Model Iteration-&iteratation..xls"; %end;
			%if &excel. =2 %then %do; ods tagsets.excelxp options(orientations = 'Landscape' Fitpage='yes' 
				pages_fitwidth ='1'	pages_fitheight='100' foreground = 'whilte' font_size = "xx-small" 
				sheet_name= "&iteratation..Model Iteration-&iteratation.");	%end;
		/*--------------------------------*/
		/* logistic model*/
		%_model_();
		/*--------------------------------*/
			%if &excel. =1 %then %do; ods html close;	%end;
		/*--------------------------------*/
	%end;
/****************************************************************************************************/
/* Model iteratation for VIF */
/****************************************************************************************************/

	%IF &MAX_VIF. > &VIF_CUT_OFF. %then %do;
	 	%do %until(&MAX_VIF. < &VIF_CUT_OFF.);
			%global iteratation ; 
			%let iteratation = %eval(&iteratation. +1);
				/*--------------------------------*/
					%if &excel. =1 %then %do; ods html file = "&output.&iteratation..Model Iteration-&iteratation..xls"; %end;
					%if &excel. =2 %then %do; ods tagsets.excelxp options(orientations = 'Landscape' Fitpage='yes' 
						pages_fitwidth ='1' pages_fitheight='100' foreground = 'whilte' font_size = "xx-small" 
						sheet_name= "&iteratation..Model Iteration-&iteratation.");	%end;
				/*--------------------------------*/
			%VIF_CHECK;
				/*--------------------------------*/
					%if &excel. =1 %then %do; ods html close; %end;
				/*--------------------------------*/
		%end;
	%end;
/****************************************************************************************************/
/* Model iteratation for in-significant variable */
/****************************************************************************************************/
	%IF &MAX_P. > &P_Value_CUT_OFF. %then %do;
		 	%do %until(&MAX_P. < &P_Value_CUT_OFF.);
			%global iteratation ; 
			%let iteratation = %eval(&iteratation. +1);
				/*--------------------------------*/
					%if &excel. =1 %then %do; ods html file = "&output.&iteratation..Model Iteration-&iteratation..xls"; %end;
					%if &excel. =2 %then %do; ods tagsets.excelxp options(orientations = 'Landscape' Fitpage='yes' 
						pages_fitwidth ='1' pages_fitheight='100' foreground = 'whilte' font_size = "xx-small" 
						sheet_name= "&iteratation..Model Iteration-&iteratation.");	%end;
				/*--------------------------------*/
			%P_value;
				/*--------------------------------*/
					%if &excel. =1 %then %do; ods html close; %end;
				/*--------------------------------*/
		%end;
	%end;
/****************************************************************************************************/
/* Model iteratation for sign of Estimates */
/****************************************************************************************************/
	%if &sign_check = 1 %then %do;
		%IF &Dis_Esti_Neg. > 1  %then %do;
		 	%do %until(&Dis_Esti_Neg. <= 1);
				%global iteratation ; 
				%let iteratation = %eval(&iteratation. +1);
				/*--------------------------------*/
						%if &excel. =1 %then %do; ods html file = "&output.&iteratation..Model Iteration-&iteratation..xls"; %end;
						%if &excel. =2 %then %do; ods tagsets.excelxp options(orientations = 'Landscape' Fitpage='yes' 
							pages_fitwidth ='1' pages_fitheight='100' foreground = 'whilte' font_size = "xx-small" 
							sheet_name= "&iteratation..Model Iteration-&iteratation.");	%end;
				/*--------------------------------*/
				%ESTIMATE_SIGN_CHECK;
				/*--------------------------------*/
						%if &excel. =1 %then %do; ods html close; %end;
				/*--------------------------------*/
			%end;
		%end;
	%end;
/****************************************************************************************************/
/* Model iteratation for number of variable in the model */
/****************************************************************************************************/

	%IF &total_var. > &TOTAL_NO_OF_VAR.  %then %do;
	 	%do %until(&total_var. <= &TOTAL_NO_OF_VAR.);
			%global iteratation ; 
			%let iteratation = %eval(&iteratation. +1);
			/*--------------------------------*/
					%if &excel. =1 %then %do; ods html file = "&output.&iteratation..Model Iteration-&iteratation..xls"; %end;
					%if &excel. =2 %then %do; ods tagsets.excelxp options(orientations = 'Landscape' Fitpage='yes' 
						pages_fitwidth ='1' pages_fitheight='100' foreground = 'whilte' font_size = "xx-small" 
						sheet_name= "&iteratation..Model Iteration-&iteratation.");	%end;
			/*--------------------------------*/
			%NUMBER_OF_MODEL_VAR;
			/*--------------------------------*/
					%if &excel. =1 %then %do; ods html close; %end;
			/*--------------------------------*/
		%end;
	%end;
/****************************************************************************************************/
	%if %sysfunc(exist(All_drop_var)) %then %do;
		/*--------------------------------*/
			%if &excel. =1 %then %do; ods html file = "&output.99.Model Iteration-Drop var list.xls"; %end;
			%if &excel. =2 %then %do; ods tagsets.excelxp options(orientations = 'Landscape' Fitpage='yes' 
				pages_fitwidth ='1' pages_fitheight='100' foreground = 'whilte' font_size = "xx-small" 
				sheet_name= "99.Model Iteration-Drop var list.");	%end;
		Proc print data = All_drop_var noobs; run;
		/*--------------------------------*/
			%if &excel. =1 %then %do; ods html close; %end;
		/*--------------------------------*/
	%END;
/****************************************************************************************************/
					%if &excel. =2 %then %do; ods tagsets.excelxp close; %end;
/****************************************************************************************************/
	Proc delete data = Dev_test_	All_drop_var _model_summary_;run;
/****************************************************************************************************/
	proc printto;run;
/****************************************************************************************************/
%mend Logistic_model;

/**************************************************************************************************************/;
/************************ Macro End **************************************************************************/;
/************************************************************************************************************/;
/*******Provide Input ******************************************************************************************/

%let dev_data			= dev				; 	/* development dataset name*/
%let val1_data			= val				;	/* validation dataset name otherwise leave blank*/
%let val2_data			= 					;	/* validation dataset name otherwise leave blank*/
%let val3_data			= 					;	/* validation dataset name otherwise leave blank*/
%let bad_var			= ab_ivpr_bad_pl	;	/* Dependent (good/bad) variable. */
%let model				= DESCENDING		;	/* DESCENDING:Model 1, otherwise leave blank, by default it models 0*/
%let weight_var			= wgt				;	/* weight variable otherwise blank */
%let ks_bin				= 10				;	/* number of bins in KS/LIFT/SINGLE GAIN report */
%let sort_score			= 1					;	/* sort the score varible while generating KS table*/
%let stepwise			= 1					;	/* 1= run Stepwise*/
%let exit 				= 0.01				;	/* "0.01/0.05" SLS, significant level for removing a variable in stepwise*/
%let entry				= 0.01				;	/* SLE, significant level for entering a variable in stepwise*/
%let VIF_CUT_OFF		= 2.0				;	/* VIF(variance inflation factor) cut off for dropping a variable*/
%let sign_check			= 0					; 	/* Check the sign of coeff. and buid model only with same sign */
%let P_Value_CUT_OFF	= 0.05				;	/* P value significant level,drop varible based on it*/
%let TOTAL_NO_OF_VAR	= 10				;	/* Total number of variable in the model*/

%let varlist 			= &woe_var_list.
											;	/* List of model variable */

%let info_val_data		= overall			;	/* Information value data */
%let infoval_data_path	= C:\Users\1567478\Desktop\TW_COL_1\data\				
											;	/* Provide the path of info val dataset generated by auto biining*/
%let Excel				= 1					;	/* 1=multiple excel file for each Iteration, 2=one excel file*/
%let output				= C:\Users\1567478\Desktop\TW_COL_1\data\				
											;	/* save output report in this location as C:\Users\Lenovo\ */;

/***************************************************************************************************************/

/*proc printto log="C:\Users\1567478\Desktop\TW_COL_1\data\Logistic_model.txt";*/

%Logistic_model();

/*proc printto;run;*/

/***************************************************************************************************************/
/*******END OF MACRO *******************************************************************************************/
/***************************************************************************************************************/

/**/
/**/
/*%let output = C:\Users\Lenovo\Downloads\fwdproject\test\;*/
/*libname local  "&output.";*/


/*data   Ttd_dev;*/
/*set    local.Ttd_dev;*/
/*%include   "C:\Users\Lenovo\Downloads\fwdproject\test\32_MB_Num_woe_code_var_missing.sas";*/
/*%include   "C:\Users\Lenovo\Downloads\fwdproject\test\02_MB_Num_woe_varlist_var_missing.sas";*/
/*%include   "C:\Users\Lenovo\Downloads\fwdproject\test\31_MB_Num_woe_code_Nomissing.sas";*/
/*%include   "C:\Users\Lenovo\Downloads\fwdproject\test\01_MB_Num_woe_varlist_Nomissing.sas";*/
/*%include   "C:\Users\Lenovo\Downloads\fwdproject\test\52_MB_Num_CA_code_var_missing.sas";*/
/*%include   "C:\Users\Lenovo\Downloads\fwdproject\test\62_MB_Num_CA_Varlist_var_missing.sas";*/
/*%include   "C:\Users\Lenovo\Downloads\fwdproject\test\51_MB_Num_CA_code_Nomissing.sas";*/
/*%include   "C:\Users\Lenovo\Downloads\fwdproject\test\61_MB_Num_CA_Varlist_Nomissing.sas";*/
/*%include   "C:\Users\Lenovo\Downloads\fwdproject\test\35_MB_Char_woe_code_Nomissing.sas";*/
/*%include   "C:\Users\Lenovo\Downloads\fwdproject\test\05_MB_Char_woe_varlist_Nomissing.sas";*/
/*%include   "C:\Users\Lenovo\Downloads\fwdproject\test\55_MB_Char_CA_code_Nomissing.sas";*/
/*%include   "C:\Users\Lenovo\Downloads\fwdproject\test\65_MB_Char_CA_Varlist_Nomissing.sas";*/
/*       */
/* Keep  GBI_TAG wgt */
/*   &NUM_woe_Num_var_missing.           */
/*   &NUM_woe_Num_nonmissing.            */
/*   &NUM_CA_Num_var_missing.            */
/*   &NUM_CA_Num_nonmissing.             */
/*   &Char_woe_Char_nonmissing.          */
/*   &Char_CA_Char_nonmissing.           */
/*                                       ;   */
/*                                           */
/*RUN    ;*/


%let woe_var_list = 
AP_IVPR_PLPAY_OST_6MW
CT_KP_KPS_1MW
CT_OPS_ACT_CD_LS_1MW
MAX_PL_AFEE_1MW
TM_SN_LST_PLPAYW
TM_SN_LS_OPS_ACT_CD_RVW
TM_SN_LS_OPS_RPCW
CB_BUR_REFIN_CODE_ML_6MW
CB_PL_DLQ_XDAYS_3MW
CT_OPS_RPC_1MW
TM_SN_LST_KP_DATEW
TM_SN_LST_KP_PTPST_ETYW
AP_IVPR_NRPC_TOT_1MW
AP_IVPR_PLINT_OST_1MW
AVG_INV_DELQ_AMT_X_3MW
CT_OPS_NRPC_IC_3MW
CT_OPS_TXN_HARD_6MW
MAX_BUR_CO_ACC_L6MW
MAX_BUR_USEC_ACC_L1MW
MAX_BUR_UTIL_DUAL_CD_1MW
TM_SN_LST_BP_PTPST_NCONW
TM_SN_LS_OPS_PTY_AW
AP_IVPR_RPC_TOT_1MW
AVG_INV_DELQ_AMT_60_3MW
CT_PLPAY_TXN_3MW
CT_OPS_ACT_CD_RV_6MW
CT_OPS_TXN_HARD_1MW
MAX_PL_AINT_1MW
TM_SN_LS_OPS_NRPCW




;


/*proc logistic data=dev DESCENDING	namelen=32	;*/
/*			model ab_ivpr_bad_pl = &woe_var_list. /selection=stepwise sls=0.01 sle=0.01 ;*/
/*			Weight wgt;*/
/*		run;*/



/*options nocenter macrogen  MFILE symbolgen   mprint  mlogic  merror serror ls=144 ps=77 source2;*/


proc logistic data=dev;
 		model ab_ivpr_bad_pl=

AP_IVPR_PLPAY_OST_6MW
CB_BUR_REFIN_CODE_ML_6MW
CB_PL_DLQ_XDAYS_3MW
CT_OPS_ACT_CD_LS_1MW
MAX_PL_AFEE_1MW
TM_SN_LST_KP_DATEW
TM_SN_LST_KP_PTPST_ETYW



				 ;
 		CODE file = 'C:\Users\1567478\Desktop\TW_COL_1\data\pprob.sas';	/* File pprob.sas saved in "C:\Users\1567478\pprob.sas" */
	run;

data VAL1;
   set VAL;
   %include 'C:\Users\1567478\Desktop\TW_COL_1\data\pprob.sas';
run;

data dev1;
   set dev;
   %include 'C:\Users\1567478\Desktop\TW_COL_1\data\pprob.sas';
run;


/**/
/**/
/*proc freq data=VAL1 noprint;*/
/*	tables P_ab_ivpr_bad_pl1*ab_ivpr_bad_pl  / measures;*/
/*	output out=somersd(keep=_SMDRC_)	 smdrc;*/
/*	run;*/
/**/
/*Title "Somers' D R|C";*/
/*proc print data = somersd; run;*/
;


%Macro risk_ranking_lift_table(input, bad_flag, score, total_bins );
	proc rank data 	= &input. group=&total_bins.
			out 	= &input._1(keep = &bad_flag. &score. rank_var);
			var 	&score. ;
	    	ranks 	rank_var ;
		run ;

	proc transreg data = &input._1  noprint;
	      model 	identity(&bad_flag.) = monotone(rank_var) / additive ;
	      output	out	=	&input._2 dap ;
	      id 		&score. ;
	    run ;

	proc sql; 
		create table &input._3 (drop=Trank_var) as 
			select 	Trank_var, 
					min(&score.) 				as min_score, 
					max(&score.) 				as max_score, 
					count(*) 					as Total,
					sum(case when &bad_flag. = 0 then 1 else 0 end) as Good,
					sum(&bad_flag.) 			as Bad,
					sum(&bad_flag.)/count(*)  	as Bad_Rate
			from &input._2 
			group by 1;
			quit;


	/*
		proc freq data = &input._2 noprint;
			tables Trank_var*&bad_flag. /missing norow nocol nopercent nocum out=&input._3;
		run;

		proc transpose data=&input._3 out=&input._4 (drop= _NAME_ _LABEL_ Rename =(_0 = Good _1 = Bad));
		    by Trank_var 		;
		    id &bad_flag.	;
		    var COUNT			;
		run;

		data &input._4;
			set &input._4;			
				Total = Good + Bad;
				lag_scr=lag(Trank_var);
				if _n_ = 1 then lag_scr = 0;
				rename Trank_var = score;
			run;
	*/
	Title "Lift Tables";
	proc print data = &input._3  ; run;

	proc delete data = &input._1 &input._2 &input._3; run;
	Title "";

%Mend risk_ranking_lift_table;

%risk_ranking_lift_table(input=dev1, bad_flag=ab_ivpr_bad_pl, score=P_ab_ivpr_bad_pl1, total_bins=10 );

/******************************************************************************************************************/
/*Logistic model is predicting "1" (bad=1) */;
/*dev_data 		= development data*/;
/* if any validation dataset is not available then leave as a blank */;
/*val1_data		= Intime validation data;*/;
/*val2_data		= out of time data*/;
/*val3_data 	= out of time data*/;
/*bad_var		= Bad variable */;
/*model			= DESCENDING if model 1(bad), other wise leave blank model 0(good)*/
/*score_var 	= Score variable 
								scale_score 		=> whatever modeling either 1/0,
								Reverse_scale_Score => Other wise, */;
/*weight_var	= Weight variable, if not available then put 1*/
/*No_bin 		= No of bin in lift table (10/20)*/
/******************************************************************************************************************/
