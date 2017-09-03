
/**********************************************************************************************************************/
/******** Weight of Evidence Calculation from Bin variable ************************************************************/
/**********************************************************************************************************************/
/****** User Input ****************************************************************************************************/
/**********************************************************************************************************************/

libname local  'F:\AMIT\TVS';

%let lib	     	= work;				/* Library name. Please define the Library */
%let Dev_data     	= Dev;				/* Development dataset name */
%let Val_data     	= Val;				/* Validation  dataset name */
%let dep_var  		= gbi_tag;  	 	/* Dependend/bad Variable (bad=0/1) name */
%let Weight_var		= wgt;			 	/* Weight variable name, if no weight then put 1 */
%let No_Var			= 6;			 	/* Total Number of variable for Characteristic Analysis*/
%let ext			= _B;				/* Suffix of bin variable */
/* List of variables for Characteristic Analysis */
%let WoE_Var_list	= ; 
/**********************************************************************************************************************/

/**********************************************************************************************************************/
/************** Macro: Weight of Evidence Calculation *****************************************************************/
/**********************************************************************************************************************/
%macro WoE();

	data &Dev_data._woe;
	set  &lib..&Dev_data.;
		Wgt = &Weight_var.;
	run;

	data &Val_data._woe;
	set  &lib..&Val_data.;
		Wgt = &Weight_var.;
	run;

	%let Dev_woe     	= &Dev_data._woe; 	%put &Dev_woe.;
	%let Val_woe     	= &Val_data._woe;	%put &Val_woe.;

    %do i = 1 %to &No_Var.; 						/* (&No_Var.) count of total variable for Characteristic Analysis */
        %let WoE_Var = %scan(&WoE_Var_list., &i.); 	/* get the variable name one by one */
        %put &WoE_Var.; 								/* check the variable name */
        %WoE_Cal(&WoE_Var.); 						/* Macro call for WoE calculation */
    %end;

%mend WoE;
/**********************************************************************************************************************/


/**********************************************************************************************************************/
/*********** WoE Calculation **********************************************************************/
/**********************************************************************************************************************/
%Macro WoE_Cal(var);

/* Development */
proc freq data = &Dev_woe.;
	tables &var.&ext.*&dep_var. /out=test1;
	weight wgt;
run;

proc transpose data=test1 out=test2 (drop= _NAME_ _LABEL_ Rename =(_0 = Good _1 = Bad));
    by &var.&ext.	;
    id &dep_var.	;
    var COUNT		;
run;

proc sql;
create table test3 as select 
	&var.&ext.	,
	log((Good/sum(Good))/ (Bad/sum(bad))) 	as 	Woe 			format 8.6
from test2;
quit;


proc sql noprint;
	create table test4 as
	select 
		a.*, 
		b.Woe as &var._Woe 
	from &Dev_woe. a 
left join test3 b on a.&var.&ext. =b.&var.&ext.;
quit;

data &Dev_woe.;
set	 test4;
run;

proc sql noprint;
	create table test5 as
	select 
		a.*, 
		b.woe as &var._WOE 
	from &Val_woe. a 
left join test3 b on a.&var.&ext. =b.&var.&ext.;
quit;

data &Val_woe. ;
set	 test5;
run;

proc delete data =  test1 test2 test3 test4 test5; run;

%Mend WoE_Cal;
/**********************************************************************************************************************/
/************ End Of WoE Calculation Macro ****************************************************************************/
/**********************************************************************************************************************/

%WoE;
