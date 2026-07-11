/**********************************************************************************************************************/
/************** Macro: Weight of Evidence Calculation *****************************************************************/
/**********************************************************************************************************************/
/* %WoE_Cal, from Woe_calculation_Macro.sas (amitmse/in_SAS_), byte-identical to the   */
/* source macro definition. The source driver read Dev/Val datasets from an external   */
/* libname ('F:\AMIT\TVS') that isn't available here, so this Jenner compatibility     */
/* bundle supplies small mock Dev/Val datasets with the same column shape the macro    */
/* expects (a binned variable ending in the source's _B suffix + a 0/1 dependent       */
/* variable) - the macro body and its call are unmodified.                             */
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

/* Mock Dev/Val datasets standing in for the source's "&lib..&Dev_data." / "&lib..&Val_data."
   (an external libname, F:\AMIT\TVS, not available here) - same column shape the macro
   expects: a binned variable (LTV_B, already grouped into bins) and the 0/1 dependent
   variable (gbi_tag). */
data Dev_woe;
	input LTV_B gbi_tag wgt;
	datalines;
	1 1 1
	1 1 1
	1 0 1
	2 1 1
	2 0 1
	2 0 1
	3 0 1
	3 0 1
	3 1 1
	4 0 1
	4 0 1
	4 0 1
	;
run;

data Val_woe;
	input LTV_B gbi_tag wgt;
	datalines;
	1 1 1
	1 0 1
	2 1 1
	2 0 1
	3 0 1
	3 0 1
	4 0 1
	4 1 1
	;
run;

%let dep_var = gbi_tag;
%let ext     = _B;
%let Dev_woe = Dev_woe;
%let Val_woe = Val_woe;

%WoE_Cal(LTV);

proc print data=Dev_woe noobs; run;
proc print data=Val_woe noobs; run;
