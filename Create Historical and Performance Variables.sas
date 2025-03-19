
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
								out=customer_base (drop=C_ASTATUS c_proddesc rename=(C_LCUSCODE=custno)) nodupkey;
		by C_LCUSCODE;
	run;
	/*****************************************************************************************/
	data loan;
		set loan.bw_loans_&t0.(keep = &loan_var. where =(&gov_condition.) );			
			length 	custno $16		ak_acno $16	ci_achl_empname $40 cv_achl_netinc 8 cr_achl_appage 8 	ai_achl_paytype $3 	ai_achl_portdesc $2 ai_achl_proddesc $40 ;
			length	cr_achl_currempmon 8 	ai_achl_astutus_0 $1 ad_achl_open_0 8 an_achl_overduedays_0 8 an_achl_overduedays_lv_0 8;
			format ad_achl_open_0 date9.;
			custno				= c_lcuscode			;	label custno		= "Customer"			;
			ak_acno 			= C_LAGNO			;	label ak_acno		= "Loan Number"		;
			empname 			= C_EMPNAME 			;	label empname		= "Customer Employer Name"			;
			netinc 				= M_NETINCOME			;	label netinc		= "Net Monthly Income"				;
			appage 				= AGE_AT_APPLICATION		;	label appage		= "Age at Application"				;
			paytype 			= C_PAYMENTTYPE 		;	label paytype		= "Loan Repayment Type"				;
			portdesc 			= C_PORTDESC 			;	label portdesc		= "Portfolio Description"			;
			proddesc			= c_proddesc			;	label proddesc		= "Product Description"				;
			currempmon 			= I_MONTHSNO_CURREMP		;	label currempmon	= "Number of Months at Current Employement"	;
			astutus_0 			= C_ASTATUS 			;/*	lable astutus_0		= "Agreement Status"				;*/
			open_0 				= D_LOAN_ACOPENDT		;/*	lable open_0		= "Account Open Date"				;*/
			overduedays_0 			= I_OVERDUEDAYS			;/*	lable overduedays_0	= "Number of Over Due Days"			;*/
			overduedays_lv_0		= I_OVERDUEDAYS			;	if C_ASTATUS not in ("L") then overduedays_lv_0 = .; 				
			drop &loan_var.;
		run;

		proc sql; create table loan as select * from loan where &condition. order by custno, ak_acno;quit;
	/**********************************************************************************************************************************/
	%do i = 1 %to 11;
			data loan_&i.;
				set loan.bw_loans_&&t&i..(keep = &loan_lim_var. where =(&gov_condition.) );				
					length 	custno $16		ak_acno $16	astutus_&i. $1 	overduedays_&i. 8 overduedays_lv_&i. 8 open_&i. 8;
					format ad_achl_open_&i. date9.;
					custno		= c_lcuscode		;
					ak_acno 		= C_LAGNO		;
					astutus_&i. 		= C_ASTATUS 			;					
					open_&i.		= D_LOAN_ACOPENDT		;
					overduedays_&i.		= I_OVERDUEDAYS			;
					overduedays_lv_&i.	= I_OVERDUEDAYS			;	if C_ASTATUS not in ("L") then overduedays_lv_&i. = .;
					drop &loan_lim_var.;
			run;

			proc sql; create table loan_&i. as select * from loan_&i. where &condition. order by custno, ak_acno;quit;

			data loan;
				merge 	loan(in=l0) 
						loan_&i.(in=l&i.);
					by custno ak_acno;
					if l0 or l&i.;
				run;
		/*proc delete data = loan_&i.;run;*/
	%end;
	/**************************************************************************************************/
	proc sql; create table loan_customer as 
		select  
				custno													,
				max(overduedays_0) 		as max_arrear_0				,
				max(bad_obs) 			as bad_obs				,
				max(co_6mon) 			as co_6mon				,
				max(dlqmx_6m) 			as dlqmx_6m				,
				max(lv_mob) 			as lv_mob				,
				max(ind_gov) 			as ind_gov				,
				max(netinc_1) 			as netinc_1				,
				max(dlqmx_lv_12m) 		as dlqmx_lv_12m				,
				max(age_gt50) 			as age_gt50				,
				max(paytype_das) 		as paytype_das				,
				max(dlqobs_lv_pl_gt30) 		as dlqobs_lv_pl_gt30			,
				max(dlqmx_12m) 			as dlqmx_12m				,
				max(urr_emp_ge_36) 		as emp_ge_36				,
				max(fst_mob) 			as fst_mob				,
				max(netinc)			as netinc	
		from loan
		group by 1
		order by 1
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

%let path_loan_data 	= C:\Loan;	/* Provide path for loan data and name should be in "loans_YYYYMM" i.e."oans_201312"*/
%let path_output_data 	= C:\data;	/* Provide path for final scored data in SAS/CSV format */
%let export_in_csv	= 1;		/* 1: export in csv, 0: not export in csv */
%let scoring_month 	= 201712;	/* Prove scoring month in YYYYMM format */

%scoring_code();

/*************************************************************************************************************************************************/
/******END****************************************************************************************************************************************/
/*************************************************************************************************************************************************/

/*************************************************************************************************************************************************/
/******12 months or more performance *************************************************************************************************************/
/*************************************************************************************************************************************************/
proc sort data=base out=customer_base (keep=custno) nodupkey; by custno; run;

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
				
				length custno $16 ai_achp_agr_sts_&i. $3 ai_achp_arr_lvl_&i. 8 ai_achp_chargeoff_&i. 8;
							
				custno 		= &cust_key.					;	label custno 		= "Customer Number"		;			
				lvl_&i.		= I_OVERDUEDAYS					;	label arr_lvl_&i.	= "Days Past Due"		;
				sts_&i.		= C_ASTATUS					;	label sts_&i.		= "Agreement Status"		;
				chargeoff_&i.	= M_ChargeOffAmt				;	label chargeoff_&i.	= "chargeoff amount"		;

				keep custno ai_achp_arr_lvl_&i.  ai_achp_agr_sts_&i. ai_achp_chargeoff_&i. ;
			run;

		proc sql; create table out.bw_loans_&&t&i.. as select * from bw_loans_&&t&i.. where custno in (select distinct custno from customer_base) order by custno;quit;		

		proc delete data = bw_loans_&&t&i..; run;

		%if &i. > 1 %then %do;
			
			data out.loans_201801;
				merge out.loans_201801(in=a) out.loans_&&t&i..(in=b);
				by custno;
				if a;
				run;

			
		%end;
	%end;

%mend;

libname pl 'C:\input';
libname out 'C:\data';

%data(scoring_month=201712, in_libname=pl, 	cust_key=c_lcuscode, end_loop=12  );
/*************************************************************************************************************************************************/
/******END****************************************************************************************************************************************/
/*************************************************************************************************************************************************/
