
/*************************************************************************************************************************************************/
/******* 12 Months historical variable ************************/
/*************************************************************************************************************************************************/
%macro scoring_code();

	libname loan "&path_loan_data.";
	libname cc "&path_cc_data.";
	libname deposit "&path_deposit_data.";
	Libname output "&path_output_data.";

	options nomprint nomlogic nosymbolgen;
	/******************************************************************************/
	%let loan_var 		= C_LCUSCODE 		C_LAGNO				C_EMPNAME 		M_NETINCOME 		C_ASTATUS 			I_OVERDUEDAYS 	AGE_AT_APPLICATION
																C_PAYMENTTYPE 	C_PORTDESC 			I_MONTHSNO_CURREMP	D_LOAN_ACOPENDT	c_proddesc	;
	%let loan_lim_var	= c_lcuscode 		C_LAGNO 			C_ASTATUS	 	I_OVERDUEDAYS 		D_LOAN_ACOPENDT		c_proddesc;
	%let gov_condition	= c_proddesc not in ('GUARANTEED SCHEME LOAN PRODUCT', 'GUARANTEED MORTGAGE LOAN');
	/******************************************************************************/
	data _null_;
		file_date 	= input("&scoring_month."||'01',yymmdd8.);
		%do i = 0 %to 11 ;/*change 11 for more than 12 months history */
			%global t&i. ;
		  	call symputx("t&i.",put(intnx('month', file_date, -&i., 'same'),yymmn6.));
			%put  &&t&i.;
		%end;
	run;
	/******************************************************************************/
	proc sort data = loan.bw_loans_&t0.(where=(C_ASTATUS eq "L" and &gov_condition.) 
								keep=C_LCUSCODE C_ASTATUS c_proddesc)
								out=customer_base (drop=C_ASTATUS c_proddesc rename=(C_LCUSCODE=ck_los_custno)) nodupkey;
		by C_LCUSCODE;
	run;
	/*****************************************************************************************/
	data loan;
		set loan.bw_loans_&t0.(keep = &loan_var. where =(&gov_condition.) );			
			length 	ck_los_custno $16		ak_acno $16	ci_achl_empname $40 cv_achl_netinc 8 cr_achl_appage 8 	ai_achl_paytype $3 	ai_achl_portdesc $2 ai_achl_proddesc $40 ;
			length	cr_achl_currempmon 8 	ai_achl_astutus_0 $1 ad_achl_open_0 8 an_achl_overduedays_0 8 an_achl_overduedays_lv_0 8;
			format ad_achl_open_0 date9.;
			ck_los_custno				= c_lcuscode			;	label ck_los_custno			= "Customer Number (LOS)"			;
			ak_acno 					= C_LAGNO				;	label ak_acno				= "Loan Agreement Number (RMS)"		;
			ci_achl_empname 			= C_EMPNAME 			;	label ci_achl_empname		= "Customer Employer Name"			;
			cv_achl_netinc 				= M_NETINCOME			;	label cv_achl_netinc		= "Net Monthly Income"				;
			cr_achl_appage 				= AGE_AT_APPLICATION	;	label cr_achl_appage		= "Age at Application"				;
			ai_achl_paytype 			= C_PAYMENTTYPE 		;	label ai_achl_paytype		= "Loan Repayment Type"				;
			ai_achl_portdesc 			= C_PORTDESC 			;	label ai_achl_portdesc		= "Portfolio Description"			;
			ai_achl_proddesc			= c_proddesc			;	label ai_achl_proddesc		= "Product Description"				;
			cr_achl_currempmon 			= I_MONTHSNO_CURREMP	;	label cr_achl_currempmon	= "Number of Months at Current Employement"	;
			ai_achl_astutus_0 			= C_ASTATUS 			;/*	lable ai_achl_astutus_0		= "Agreement Status"				;*/
			ad_achl_open_0 				= D_LOAN_ACOPENDT		;/*	lable ad_achl_open_0		= "Account Open Date"				;*/
			an_achl_overduedays_0 		= I_OVERDUEDAYS			;/*	lable an_achl_overduedays_0	= "Number of Over Due Days"			;*/
			an_achl_overduedays_lv_0	= I_OVERDUEDAYS			;	if C_ASTATUS not in ("L") then an_achl_overduedays_lv_0 = .; 				
			drop &loan_var.;
		run;

		proc sql; create table loan as select * from loan where &condition. order by ck_los_custno, ak_acno;quit;
	/**********************************************************************************************************************************/
	%do i = 1 %to 11;
			data loan_&i.;
				set loan.bw_loans_&&t&i..(keep = &loan_lim_var. where =(&gov_condition.) );				
					length 	ck_los_custno $16		ak_acno $16	ai_achl_astutus_&i. $1 	an_achl_overduedays_&i. 8 an_achl_overduedays_lv_&i. 8 ad_achl_open_&i. 8;
					format ad_achl_open_&i. date9.;
					ck_los_custno				= c_lcuscode			;
					ak_acno 					= C_LAGNO				;
					ai_achl_astutus_&i. 		= C_ASTATUS 			;					
					ad_achl_open_&i.			= D_LOAN_ACOPENDT		;
					an_achl_overduedays_&i.		= I_OVERDUEDAYS			;
					an_achl_overduedays_lv_&i.	= I_OVERDUEDAYS			;	if C_ASTATUS not in ("L") then an_achl_overduedays_lv_&i. = .;
					drop &loan_lim_var.;
			run;

			proc sql; create table loan_&i. as select * from loan_&i. where &condition. order by ck_los_custno, ak_acno;quit;

			data loan;
				merge 	loan(in=l0) 
						loan_&i.(in=l&i.);
					by ck_los_custno ak_acno;
					if l0 or l&i.;
				run;
		/*proc delete data = loan_&i.;run;*/
	%end;
	/**************************************************************************************************/
	proc sql; create table loan_customer as 
		select  
				ck_los_custno													,
				max(an_achl_overduedays_0) 		as cn_achl_max_arrear_0			,
				max(cb_achl_bad_obs) 			as cb_achl_bad_obs				,
				max(cb_achl_co_6mon) 			as cb_achl_co_6mon				,
				max(cn_ivpr_dlqmx_6m) 			as cn_ivpr_dlqmx_6m				,
				max(cb_achl_lv_mob) 			as cb_achl_lv_mob				,
				max(cb_ivpr_ind_gov) 			as cb_ivpr_ind_gov				,
				max(cnw_achl_netinc_1) 			as cnw_achl_netinc_1			,
				max(cnt_ivpr_dlqmx_lv_12m) 		as cnt_ivpr_dlqmx_lv_12m		,
				max(cb_ivpr_age_gt50) 			as cb_ivpr_age_gt50				,
				max(cb_achl_paytype_das) 		as cb_achl_paytype_das			,
				max(cb_ivpr_dlqobs_lv_pl_gt30) 	as cb_ivpr_dlqobs_lv_pl_gt30	,
				max(cnt_ivpr_dlqmx_12m) 		as cnt_ivpr_dlqmx_12m			,
				max(cb_ivpr_curr_emp_ge_36) 	as cb_ivpr_curr_emp_ge_36		,
				max(cr_achl_fst_mob) 			as cr_achl_fst_mob				,
				max(cvl_achl_netinc)			as cvl_achl_netinc	
		from loan
		group by ck_los_custno
		order by ck_los_custno
		;
		quit;

	/*proc delete data = all_customer_base cc cc_customer customer customer_base deposit deposit_customer loan loan_customer;run;*/
	%if &export_in_csv. = 1 %then %do;
		proc export data	= output.Final_Score_as_of_&scoring_month.
			   		outfile	= "&path_output_data.\Final_Score_as_of_&scoring_month..csv"
			   		dbms	= csv
			   		replace;
			run;
	%end;
	/***********************************************************************************************************/
%Mend scoring_code;
/***********************************************************************************************************/	

%let path_loan_data 	= \\10.20.185.140\Projects\BW_PL_B_01\landing\Loan;	/* Provide path for loan data and name should be in "bw_loans_YYYYMM" i.e."bw_loans_201312"*/
%let path_output_data 	= \\10.20.185.140\Projects\BW_PL_B_01\Build 2\data;	/* Provide path for final scored data in SAS/CSV format */
%let export_in_csv		= 1;												/* 1: export in csv, 0: not export in csv */
%let scoring_month 		= 201712;											/* Prove scoring month in YYYYMM format */

%scoring_code();

/*************************************************************************************************************************************************/
/******END****************************************************************************************************************************************/
/*************************************************************************************************************************************************/

/*************************************************************************************************************************************************/
/******12 months or more performance *************************************************************************************************************/
/*************************************************************************************************************************************************/
proc sort data=base out=customer_base (keep=ck_los_custno) nodupkey; by ck_los_custno; run;

%Macro data(scoring_month, in_libname, cust_key, end_loop);
	data _null_;
			file_date 	= input("&scoring_month."||'01',yymmdd8.);
			/*call symputx("scoring_time",put(file_date,yymmn6.));*/
			
			%do i = 0 %to &end_loop. ;
				%global t&i. ;
			  	call symputx("t&i.",put(intnx('month', file_date, &i., 'same'),yymmn6.));
				%put  &i. "-:" &&t&i.;
			%end;
		run;

	%do i = 1 %to &end_loop.;
	
		data bw_loans_&&t&i..;
			set &in_libname..bw_loans_&&t&i..;
				
				length ck_los_custno $16 ai_achp_agr_sts_&i. $3 ai_achp_arr_lvl_&i. 8 ai_achp_chargeoff_&i. 8;
							
				ck_los_custno 			= &cust_key.						;	label ck_los_custno 	= "Customer Number"			;			
				ai_achp_arr_lvl_&i.		= I_OVERDUEDAYS						;	label ai_achp_arr_lvl_&i.	= "Days Past Due"			;
				ai_achp_agr_sts_&i.		= C_ASTATUS							;	label ai_achp_agr_sts_&i.	= "Agreement Status"		;
				ai_achp_chargeoff_&i.	= M_ChargeOffAmt					;	label ai_achp_chargeoff_&i.	= "chargeoff amount"		;

				keep ck_los_custno ai_achp_arr_lvl_&i.  ai_achp_agr_sts_&i. ai_achp_chargeoff_&i. ;
			run;

		proc sql; create table out.bw_loans_&&t&i.. as select * from bw_loans_&&t&i.. where ck_los_custno in (select distinct ck_los_custno from customer_base) order by ck_los_custno;quit;		

		proc delete data = bw_loans_&&t&i..; run;

		%if &i. > 1 %then %do;
			
			data out.bw_loans_201801;
				merge out.bw_loans_201801(in=a) out.bw_loans_&&t&i..(in=b);
				by ck_los_custno;
				if a;
				run;

			
		%end;
	%end;

%mend;

libname pl 'Y:\BW_PL_B_01\Build 2\input';
libname out 'Y:\BW_PL_B_01\Build 2\data';

%data(scoring_month=201712, in_libname=pl, 	cust_key=c_lcuscode, end_loop=12  );
/*************************************************************************************************************************************************/
/******END****************************************************************************************************************************************/
/*************************************************************************************************************************************************/
