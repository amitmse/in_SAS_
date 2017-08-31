
/********Characteristic Analysis **************************************************************************************/
/**********************************************************************************************************************/
/****** User Input ****************************************************************************************************/
/**********************************************************************************************************************/
%let Dev_data     	= local.ttd_dev; 				/* Development dataset name */
%let Val_data     	= local.ttd_val; 				/* Validation  dataset name */
%let dep_var  		= gbi_tag;  	 		/* Dependend/bad Variable (bad=0/1) name */
%let Weight			= wgt;			 		/* Weight variable name, */
%let No_Var			= 3;			 		/* Total Number of variable for Characteristic Analysis*/
%let CA_Output		= C:\Users\amit.kumar\Desktop\SAS code - TTD Model\CA.xls; /* Output file name with location*/

/* List of variables for Characteristic Analysis */
%let CA_Var_list	= GENDER	CO_BORROWER BANK_ACCOUNT_STATUS	; 
/**********************************************************************************************************************/


/**********************************************************************************************************************/
/************** Macro: Characteristic Analysis ************************************************************************/
/**********************************************************************************************************************/
%macro CA();

	Data Dev ;
	set  &Dev_data. ;
		Wgt_var=&Weight.;
		keep &dep_var. &CA_Var_list. Wgt_var;
	Run;

	Data  Val ;
	set  &Val_data. ;
		Wgt_var=&Weight.;
		keep &dep_var. &CA_Var_list. Wgt_var;
	Run;

    %do i = 1 %to &No_Var.; 					/* (&No_Var.) count of total variable for Characteristic Analysis */
        %let CA_Var = %scan(&CA_Var_list., &i.); /* get the variable name one by one */
        %put &CA_Var.; 							/* check the variable name */
        %CA_Cal(&CA_Var.); 						/* Macro call for CA calculation */
    %end;

	/****Export  Characteristic Analysis Report ********/
	ods tagsets.excelxp file="&CA_Output." style=statistical options(sheet_name='Overall_PSI' 	sheet_interval='Proc');
		proc print data = Overall_PSI; 	run;
	ods tagsets.excelxp  				style=statistical options(sheet_name='CA_Report' 		sheet_interval='Proc');
		proc print data = CA_Report; 	run;
	ods _all_ close;

	Proc delete data = Dev Val CA_Report Overall_PSI; run;


%mend CA;
/**********************************************************************************************************************/

/**********************************************************************************************************************/
/*********** Characteristic Analysis Calculation **********************************************************************/
/**********************************************************************************************************************/
%Macro CA_Cal(var);
/* Development */
proc freq data = Dev;
	tables &var.*&dep_var. /out=test1;
	weight Wgt_var;
run;

proc transpose data=test1 out=test2 (drop= _NAME_ _LABEL_ Rename =(_0 = Good _1 = Bad));
    by &var. 		;
    id &dep_var.	;
    var COUNT		;
run;

data test3;
set  test2;
	Length Var $50;
	Var = "&var.";
	Length Category $50;
	Category=&var.;
	Total = Good+Bad;
	Bad_Rate = Bad/Total;
run;

proc sql;
create table test4 as select 
	Var					as	Variable,
	Category			as  Var_Category,
	Good				as  Dev_Good,
	Bad					as  Dev_Bad,
	Total				as  Dev_Total,
	Total/sum(Total) 	as 	Dev_Pct_tot,
	Bad/sum(Bad) 		as 	Dev_Pct_Bad,
	Good/sum(Good) 		as 	Dev_Pct_Good
from test3;
quit;

/* Validation  */
proc freq data = Val;
	tables &var.*&dep_var. /out=test5;
	weight Wgt_var;
run;

proc transpose data=test5 out=test6 (drop= _NAME_ _LABEL_ Rename =(_0 = Good _1 = Bad));
    by &var. 		;
    id &dep_var.	;
    var COUNT		;
run;

data test7;
set  test6;
	Length Var $50;
	Var = "&var.";
	Length Category $50;
	Category=&var.;
	Total = Good+Bad;
	Bad_Rate = Bad/Total;
run;

proc sql;
create table test8 as select 
	Var					as	Variable		,
	Category			as  Var_Category	,
	Good				as  Val_Good		,
	Bad					as  Val_Bad			,
	Total				as  Val_Total		,
	Total/sum(Total) 	as 	Val_Pct_tot		,
	Bad/sum(Bad) 		as 	Val_Pct_Bad		,
	Good/sum(Good) 		as 	Val_Pct_Good
from test7;
quit;

proc sql;
	create table test9 as select 

		/* development */
		a.Variable			,
		a.Var_Category		,
		a.Dev_Good			,
		a.Dev_Bad			,
		a.Dev_Total			,
		a.Dev_Pct_tot		,
		a.Dev_Pct_Bad		,
		a.Dev_Pct_Good		,
		/* Validation */
		b.Val_Good			,
		b.Val_Bad			,
		b.Val_Total			,
		b.Val_Pct_tot		,
		b.Val_Pct_Bad		,
		b.Val_Pct_Good		,
		(log(a.Dev_Pct_Good / a.Dev_Pct_Bad)) 									 as Dev_Woe 		format 8.6	,
		((a.Dev_Pct_Good - a.Dev_Pct_Bad)*(log(a.Dev_Pct_Good / a.Dev_Pct_Bad))) as Dev_Info_Val 	format 8.6	,
		((a.Dev_Pct_tot  - b.Val_Pct_tot)*(log(a.Dev_Pct_tot  / b.Val_Pct_tot))) as PSI 			format 8.6
	from 		test4 	as a
	Full Join 	test8 	as b on a.Variable=b.Variable and a.Var_Category=b.Var_Category;
quit;

proc sql; 
	create table test10 as select 
		distinct  Variable, 
		sum(Dev_Info_Val) 	as Total_Info_Val 	format percent7.2 	,
		sum(PSI) 			as Total_PSI 		format percent7.2 

from test9; quit;

proc append data = test10 	base=Overall_PSI; 	run;
proc append data = test9 	base=CA_Report; 	run;

proc delete data = test1 test2 test3 test4 test5 test6 test7 test8 test9 test10; run;

%Mend CA_Cal;
/**********************************************************************************************************************/
/************End Of macro *********************************************************************************************/
/**********************************************************************************************************************/

%CA;
