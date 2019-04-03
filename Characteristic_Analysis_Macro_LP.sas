
/********Characteristic Analysis **************************************************************************************/
/**********************************************************************************************************************/
/****** User Input ****************************************************************************************************/
/**********************************************************************************************************************/
%let Dev_data     	= dev; 					/* Development dataset name */
%let Val_data     	= val_1; 					/* Validation  dataset name */
%let dep_var  		= ab_ivpr_bad_pl;  	 	/* Dependend/bad Variable (bad=0/1) name */
%let Weight			= 1;			 		/* Weight variable name, */
%let No_Var			= 3;			 		/* Total Number of variable for Characteristic Analysis*/
%let pd				= score	;					/* probability from logistic for LP*/
%let CA_Output		= Y:\TW_US_C_01\docs\Model_Iteration\PL\New Model Iterations\CA.xls; /* Output file name with location*/

/* List of variables for Characteristic Analysis */
%let CA_Var_list	= 	ab_ivpr_st_roll_3m	crt_pl_mob_fst_woe crt_inv_lst_sucs_cntct_woe; 
/**********************************************************************************************************************/
options mprint mlogic symbolgen;

/**********************************************************************************************************************/
/************** Macro: Characteristic Analysis ************************************************************************/
/**********************************************************************************************************************/

%macro CA();

	Data _Dev_ ;
	set  &Dev_data. end=last;
			%if &pd. ne  %then %do ;  _pd_ = &pd. ;	%end ;	%else %do ;	_pd_ 	= 1 ; %end ;
			Wgt_var=&Weight.;
			
			obs = 1 ;
			tot_obs + Wgt_var ;
			tot_dvs + &dep_var.*Wgt_var ;

			if last then do ;
			   	call symput('dev_tot_obs',left(put(tot_obs,best15.))) ;
			   	call symput('dev_tot_dvs',left(put(tot_dvs,best15.))) ;
			end ;

			keep &dep_var. &CA_Var_list. Wgt_var _pd_;
		Run;


  	%let dev_tot_nodvs 	= %sysevalf(&dev_tot_obs - &dev_tot_dvs) ;
  	%put TOT_OBS = &dev_tot_obs TOT_DV = &dev_tot_dvs TOT_NODV = &dev_tot_nodvs;


	Data  _Val_ ;
	set  &Val_data. end=last;
		%if &pd. ne  %then %do ;  _pd_ = &pd. ;	%end ;	%else %do ;	_pd_ 	= 1 ; %end ;
		Wgt_var=&Weight.;

		obs = 1 ;
		tot_obs + Wgt_var ;
		tot_dvs + &dep_var.*Wgt_var ;

		if last then do ;
			   	call symput('val_tot_obs',left(put(tot_obs,best15.))) ;
			   	call symput('val_tot_dvs',left(put(tot_dvs,best15.))) ;
		end ;

		keep &dep_var. &CA_Var_list. Wgt_var _pd_;
	Run;


  	%let val_tot_nodvs 	= %sysevalf(&val_tot_obs - &val_tot_dvs) ;
  	%put TOT_OBS = &val_tot_obs TOT_DV = &val_tot_dvs TOT_NODV = &val_tot_nodvs;


    %do i = 1 %to &No_Var.; 					/* (&No_Var.) count of total variable for Characteristic Analysis */
        %let CA_Var = %scan(&CA_Var_list., &i.); /* get the variable name one by one */
        %put &CA_Var.; 							/* check the variable name */
        %CA_Cal(&CA_Var.); 						/* Macro call for CA calculation */

    %end;


	/****Export  Characteristic Analysis Report ********/
	ods tagsets.excelxp file="&CA_Output." style=statistical options(sheet_name='Overall_PSI' 	sheet_interval='Proc');
		proc print data = Overall_PSI; 	run;
	ods tagsets.excelxp  				style=statistical options(sheet_name='CA_Report' 		sheet_interval='Proc');
		proc print data = CA_Report; 	where variable ne ''; run;
	ods _all_ close;

	Proc delete data = _dev_ _val_  CA_Report Overall_PSI; run;


%mend CA;
/**********************************************************************************************************************/

/**********************************************************************************************************************/
/*********** Characteristic Analysis Calculation **********************************************************************/
/**********************************************************************************************************************/
%Macro CA_Cal(var);
		/* Development */

		proc sql; create table test4 as 
							select
								"&var."											as Variable			format=$50. length=50,
								cats(&var.)										as Var_Category 	format=$50. length=50,
								sum(Wgt_var) 									as Dev_Total				,
								(calculated Dev_Total)/&dev_tot_obs.		 	as Dev_Pct_tot				,
								sum(&dep_var.*Wgt_var) 							as Dev_Bad					,
								(calculated Dev_Bad)/&dev_tot_dvs.				as Dev_Pct_Bad				,
								(calculated Dev_Total - calculated Dev_Bad ) 	as Dev_Good					,
								(calculated Dev_Good)/&dev_tot_nodvs.			as Dev_Pct_Good				,
								(calculated Dev_Bad)/(calculated Dev_Total)		as Dev_Bad_Rate 			format percent8.2,
								mean(_pd_) 										as Dev_expected_bad_rate 	format percent8.2
							from _Dev_
							group by 1,2
							order by Dev_Bad_Rate desc  ;
						quit;


		/* Validation  */
		proc sql; create table test8 as 
							select
								"&var."											as Variable			format=$50. length=50,
								cats(&var.)										as Var_Category 	format=$50. length=50,
								sum(Wgt_var) 									as Val_Total				,
								(calculated Val_Total)/&val_tot_obs.		 	as Val_Pct_tot				,
								sum(&dep_var.*Wgt_var) 							as Val_Bad					,
								(calculated Val_Bad)/&val_tot_dvs.				as Val_Pct_Bad				,
								(calculated Val_Total - calculated Val_Bad ) 	as Val_Good					,
								(calculated Val_Good)/&val_tot_nodvs.			as Val_Pct_Good				,
								(calculated Val_Bad)/(calculated Val_Total)		as Val_Bad_Rate 			format percent8.2,
								mean(_pd_) 										as Val_expected_bad_rate 	format percent8.2
							from _val_
							group by 1,2
							order by Val_Bad_Rate desc  ;
						quit;



		proc sql;
			create table test9 as select 

				/* development */
				a.Variable			,
				a.Var_Category		,
				a.Dev_Good			,
				a.Dev_Bad			,
				a.Dev_Total			,
				a.Dev_Pct_tot		format percent8.0,
				a.Dev_Pct_Bad		format percent8.0,
				a.Dev_Pct_Good		format percent8.0,
				a.Dev_Bad_Rate		format percent8.2,
				a.Dev_expected_bad_rate format percent8.2,

				/* Validation */
				b.Val_Good			,
				b.Val_Bad			,
				b.Val_Total			,
				b.Val_Pct_tot		format percent8.0,
				b.Val_Pct_Bad		format percent8.0,
				b.Val_Pct_Good		format percent8.0,
				b.Val_Bad_Rate		format percent8.2,
				b.Val_expected_bad_rate format percent8.2,	
	
				((a.Dev_Pct_Good - a.Dev_Pct_Bad)*(log(a.Dev_Pct_Good / a.Dev_Pct_Bad))) as Dev_Info_Val 	format percent8.2	,
				((b.Val_Pct_Good - b.Val_Pct_Bad)*(log(b.Val_Pct_Good / b.Val_Pct_Bad))) as Val_Info_Val 	format percent8.2	,
				((a.Dev_Pct_tot  - b.Val_Pct_tot)*(log(a.Dev_Pct_tot  / b.Val_Pct_tot))) as PSI 			format percent8.2
				/*(log(a.Dev_Pct_Good / a.Dev_Pct_Bad)) 									 as Dev_Woe 		format 8.6	*/

			from 		test4 	as a
			Full Join 	test8 	as b on a.Variable=b.Variable and a.Var_Category=b.Var_Category;
		quit;

		proc sql; 
			create table test10 as select 
				distinct  Variable, 
				sum(Dev_Info_Val) 	as Dev_Info_Val 	format percent7.2 	,
				sum(Val_Info_Val) 	as Val_Info_Val 	format percent7.2 	,
				sum(PSI) 			as Total_PSI 		format percent7.2 

		from test9; quit;

		proc append data = test10 	base=Overall_PSI force; 	run;
		proc append data = test9 	base=CA_Report   force; 	run;

		proc delete data = test4 test8 test9 test10 ; run;

		ods html close; 
		ods preferences; 
%Mend CA_Cal;

/**********************************************************************************************************************/
/************End Of macro *********************************************************************************************/
/**********************************************************************************************************************/

%CA;
