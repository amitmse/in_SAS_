Exclusion:
	- Bad at Obs / DRP in obs / Missing Performance
	- Fraud, Lost, Stolen, Closed & Deceased (Obs, Perf)
	- Staff Customers / VIP Customers / Test customers / Corporate customers
	- Secured / Government Guaranteed
	- Inactive
	- MoB < 6 months
	- Age < 18 years
	- No Bureau / Ever bureau off-us charge off
	- Blacklisted Customers
	- Policy Exclusions
	- Card Fee / Low Balance Charge offs (LBCO<=5000) / Low Balance
	- Pre-Approved Cards / Top-Up
	- Not Booked
	- Observation & performance period overlap for the customers having multiple cards


options nocenter macrogen  MFILE symbolgen   mprint  mlogic  merror serror ls=144 ps=77 COMPRESS=YES;

/*Change SAS work directory:*/
	https://stackoverflow.com/questions/43429208/change-temporary-file-directory-in-sas
	libname user 'C:\Users\1567478\MyData\SAS';
	data zipcode;
		set sashelp.zipcode;
		run;
		
/* Check available memory */
data _null_;
    		mem = input(getoption('xmrlmem'),20.2)/10e6;
    		format mem 20.2;
    		put "You have " mem "GB memory available";
	run;

/**************************************************************/
/* get list of data from lib*/
libname a '\\inwpipfps21\wkgrps\Data_Landing_Area\SG\Bankruptcy Projection Model';
ods output Members=Members;
proc datasets library=a memtype=data;
run;
quit;
/**************************************************************/

%put &sqlobs.;

%macro distinct_count(Numvar);
	%do i = 1 %to &Numvar. ; 
		%let var = %upcase(%scan(&keep_var.,&i)) ;
		proc sort data = dev(keep=&var.) out=test nodupkey;by &var.; run;
		proc sql; create table t as select "&var." as var length=32, count(*) as distinct_value from test; quit;
		proc append data = t base=distinct force;run;
		proc delete data = t test; run;
	%end ;
%mend distinct_count;


%distinct_count(Numvar=697);


libname local  'F:\AMIT\TVS';
/************************************************************************************************************/
/***Clear the Log and output window**************************************************************************/
/***********************************************************************************************************/
%macro clear_log_output;
	DM "log; clear; ";
	DM "odsresults; clear; ";
%mend clear_log_output;
/*%clear_log_output;*/
/***********************************************************************************************************/
/***********************************************************************************************************/

DATA ONEs TWOs inBOTH NOmatch1 NOmatch2 allRECS NOmatch;
MERGE ONE(IN=In1) TWO(IN=In2);
	 BY ID;
	 IF In1=1 				then output ONEs;
	 IF In2=1 				then output TWOs;
	 IF (In1=1 and In2=1) 	then output inBOTH;
	 IF (In1=0 and In2=1) 	then output NOmatch1;
	 IF (In1=1 and In2=0) 	then output NOmatch2;
	 IF (In1=1 OR In2=1) 	then output allRECS;
	 IF (In1+In2)=1 		then output NOmatch;
 RUN; 


/************************************************************************************************************/
/* remove the variable which is totally missing. then run the below code to replace the missing with -99*/
/************************************************************************************************************/
%Macro missing(data, missing_value);
PROC STDIZE DATA=&data.  OUT=Dev_test REPONLY MISSING=&missing_value.;
VAR _NUMERIC_;
RUN;
%Mend missing;
/************************************************************************************************************/



/***********************************************************************************************************/
/******************* Delete all file from work library ***********************************************************/;
%macro delete_all_file_from_library(libname);
			proc datasets library=&libname. kill; run; quit;
%mend delete_all_file_from_library;
/*%delete_all_file_from_library(libname=work);*/
/*****************************************************************************************************************/;






/************************************************************************************************************/
/* split the data based on input value of a variable and then generate the lift table of each input value */
/************************************************************************************************************/
%macro split_data(var, indata);
proc sql noprint; select count(distinct &var.) 	into: count_&var from &indata.; quit; %put &&count_&var.;
proc sql noprint; select distinct &var. into: all_invalue SEPARATED BY ',' 	from &indata.; quit; %put &all_invalue. ;
    %do i = 1 %to &&count_&var.; 
        %let invalue = %scan(%bquote(&all_invalue),&i,%str(,)); 
        %put &invalue.;
		data &invalue.;
		set &indata.;
		if &var. = "&invalue.";
		run;
		Title "&var. = &invalue.";
		%KS(in_data=&invalue., bad=GBI_Tag,score=Reverse_scale_Score, bin_ks=1, weight=wgt, no_bin=10);
		proc delete data = &invalue.;run;
    %end;
%mend split_data;
/*ods html body = "zone.xls";*/
/*%split_data(var=zone, indata=dev_scored);*/
/*%split_data(var=zone, indata=val_scored);*/
/*%split_data(var=zone, indata=oot1_scored);*/
/*%split_data(var=zone, indata=oot2_scored);*/
/*ods html close;*/
/************************************************************************************************************/




/************************************************************************************************************/
/******************** calculate concordance *****************************************************************/
/************************************************************************************************************/
proc sql ;
      create table q as
      select one.x as pred1 ,
             z.x   as pred0 ,
             case when one.x < z.x then 1 when one.x = z.x then 0 else -1 end as concordant
        from abc as one , abc as z;
quit ;
/* Self Join */
proc sql; create table cds as select a.*, b.* from abc a, xyz b;quit; 
/***********************************************************************************************************/



/***********************************************************************************************************/
/************************* PDO Equifax *********************************************************************/
%macro odds_scale(logodds,basepts,baseodds,pdo,score);
*********Scales the logodds to a certain odds scale and produces final score********;
* logodds = variable that contains sum of points in logodds;
* basepts = Base points - score at which to set the given odds below;
* baseodds= Base odds;
* pdo     = points to double odds;
* score = name of the resulting score;
 
&Score. = Round((&pdo./log(2))*(&logodds. - log(&baseodds.)) + &basepts.,1);
 
     If &Score. < 1   Then &Score. = 1;
Else If &Score. > 999 Then &Score. = 999;
%mend;
/***********************************************************************************************************/;




/*******************************************************************************************************************/
/******************* INFO VAL MACRO ********************************************************************************/
/*******************************************************************************************************************/
/*Please note GBI_Tag is dependent variable in this code */
/*Dataset name has to be DEVBASE and dependent variable name has to be GBI_TAG. Please drop indeterminate if any. */
/*remove the variable which is totally missing. then run the below code to replace the missing with -999 */
/*******************************************************************************************************************/
/**************** User Input *********************************************/
/*%let indata  = Dev;			*/
/* input data name with libname */
/*%let dep_var = GBI_tag;  	*/
/* mention name of dependent variable. without indeterminate */
/*%let weight  = wgt; 		*/
/* weight variable. if there is no weight then put 1 */
/*%let output	 = C:\Users\amit.kumar\Desktop\SAS code - TTD Model\Information_value.xls; */
/* Output report file with path */
/*%let delete_all_file = 1;	*/
/* 1= delete all files in work, 0=do not delete */
/**/
/* Numeric variables list */
/*%let BinCVarList = */
/*AMOUNT_FINANACED*/
/*BRANCH_CODE*/
/**/
/*;*/
/**/
/*%put &BinCVarList;*/
/* */
/* Character variables list */
/*%let BinDVarList = */
/*PINCODE*/
/*CITY*/
/*PROFILE*/
/*MODEL*/
/**/
/*;*/
/**/
/*%put &BinDVarList;*/

/*******************************************************************************************************************/
/**************** Do not change below ******************************************************************************/
/*******************************************************************************************************************/
%Macro Info_val_report();

data DEVBASE_Missing;
set  &indata. ;
	wgt		=	&weight.;
	GBI_tag = 	&dep_var.;
/* keep only required variables */
keep GBI_tag wgt &BinCVarList.  &BinDVarList.;
run;

/* Missing value treatment: Assign -999 for missing value  */
PROC STDIZE DATA= DEVBASE_Missing  OUT=DEVBASE REPONLY MISSING = -999; VAR _NUMERIC_; RUN;

/****************************************************************************************/
proc freq data = DEVBASE noprint;
	tables GBI_tag / out = BadsCount;
	weight wgt ;
run;
 
proc sql noprint; select  count into:NoofBads  from BadsCount where GBI_TAG=1;quit;
proc sql noprint; select  count into:NoofGoods from BadsCount where GBI_TAG=0;quit;

%put &NoofBads;   
%put &NoofGoods;  
%let TotalPop   = %SYSEVALF(&NoofGoods + &NoofBads);
%put &TotalPop.;

/* 
Info val for Numeric variable:
	1. Using proc rank to find maximum 20 bins for each continuous variable 
   	2. Find the infoval of that variable 
*/
 
%macro MakeBinsC(Varname);
/* Raw binning of variable by using proc rank */
proc rank data =  DevBase(keep = &varname GBI_tag wgt) out = CoarseRanked groups = 20;
                var &Varname;
                ranks RankVar;
run;

data CoarseRanked;
set CoarseRanked;
	_tot_=1;
run;

/* binwise Total, Min, Max  */
proc summary SUM data = CoarseRanked nway;
	class RankVar;
	weight wgt;
	var  &Varname GBI_tag _tot_ ;
	output out = &varname  (Drop = _FREQ_ _TYPE_ )
						MIN(&Varname) 	= Low 
						MAX(&Varname) 	= High 
						SUM(GBI_tag) 	= BandNoofBads
						sum(_tot_) 		= TotalinBand; 
run;
 

data &varname	(drop = Low High rename=(tempLow = Low tempHigh=High));
set  &varname;

	length tempLow  $32.;
	length tempHigh $32.;
	tempLow  = Low||"";
	tempHigh = High||"";

run;
 
data &varname ;
set  &varname ;

	length variable $32;
	Variable = "&varname";
	TP 				= TotalinBand/&TotalPop		;
	BandNoofGoods 	= TotalinBand - BandNoofBads	;
	GP 				= BandNoofGoods/&NoofGoods	;
	BP 				= BandNoofBads/&NoofBads	;

			if GP*BP 	ne 	0 then 	WoE = log((BandNoofGoods/&NoofGoods)/(BandNoofBads/&NoofBads));
	else 	if GP 		= 	0 then 	WoE = -2; 
	else 							WoE =  2;

	InfovalBand 	= (GP - BP)*WoE;
	WoeRound 		= floor(WoE*10);
	retain cumm_infoval 0; 
	cumm_Infoval	= cumm_Infoval + InfovalBand;
	BadRate 		= BandNoofBads/TotalinBand;
	/*BadRateRound 	= floor(BadRate/5);*/
run;
 
proc means data = &varname;
	var  InfovalBand;
	output 	out = inf&varname SUM(InfovalBand) = Infoval; 
run;
 
data inf&varname	(drop = _type_ _freq_);
set inf&varname;
	length variable $32;
	Variable = "&varname";
run;

%mend MakeBinsC;
 
/* 
Info val for char variable:
	1. Form bins for each discrete variable (no. of bins = no. of distinct values
   	2. Find the infoval of that variable 
*/
 
%macro MakeBinsD(Varname);
%global VarType;
proc sort data =  DevBase(keep = &varname GBI_tag wgt) out = CoarseRanked;
	by &varname;
run;
 
data CoarseRanked (drop = Pr&varname);
set  CoarseRanked;

	_tot_=1;
			if _n_ = 1 					then 	RankVar = 0;
	else 	if &varname ne Pr&varname 	then	RankVar = RankVar + 1;
	else 										RankVar = RankVar;

	Pr&varname = &varname;
	retain Pr&varname RankVar;
	
run;
 
PROC SQL;
	CREATE TABLE ToFindType AS 
		SELECT 
			NAME,
			TYPE 
		FROM DICTIONARY.COLUMNS
	WHERE LIBNAME=%UPCASE("Work") AND MEMNAME=%UPCASE("CoarseRanked");
QUIT;
 
data TempToFindType;
set  ToFindType;
	if upcase(NAME) = "&varname" then call symput('VarType',type);
run;

%put &VarType;

%if &VarType = "num" %then %do;
	proc summary SUM data = CoarseRanked nway;
		class RankVar;
		weight wgt;
		var  &Varname GBI_tag _tot_ ;
		output out = &varname (Drop = _FREQ_ _TYPE_) 
								MIN(&Varname) 	= Low 
								MAX(&Varname) 	= High 
								SUM(GBI_tag) 	= BandNoofBads
								SUM(_tot_) 		= TotalinBand; 
	run;
 
	data &varname	(drop = Low High rename=(tempLow = Low tempHigh=High));
	set &varname;

		length tempLow $32.;
		length tempHigh $32.;
		tempLow 	= Low||"";
		tempHigh 	= High||"";

	run;
 
%end;
%else %do;
	data TempCoarseRanked(keep = Low High RankVar );
	set  CoarseRanked;

		by RankVar;
		length Low $32.;
		length High $32.;
		if first.RankVar;
		Low 	= &varname;
		High	= &varname;

	run;
 
	proc summary SUM data = CoarseRanked nway;
		class RankVar;
		var GBI_tag _tot_ ;
		weight wgt;
		output out = &varname (	Drop = _FREQ_ _TYPE_ ) 
								SUM(GBI_tag) = BandNoofBads
								SUM(_tot_) 		= TotalinBand; 
	run;
 
	data  &varname;
	merge &varname TempCoarseRanked;
		by RankVar;
	run; 
%end;
 
data &varname ;
set  &varname ;

	length variable $32;
	Variable = "&varname";
	TP 				= TotalinBand/&TotalPop;
	BandNoofGoods 	= TotalinBand - BandNoofBads;
	GP 				= BandNoofGoods/&NoofGoods;
	BP 				= BandNoofBads/&NoofBads;
			if GP*BP 	ne 0 	then 	WoE = log((BandNoofGoods/&NoofGoods)/(BandNoofBads/&NoofBads));
    else 	if GP 		= 0 	then 	WoE = -2; 
	else 								WoE =  2;

	InfovalBand 	= (GP - BP)*WoE;
	WoeRound 		= floor(WoE*10);
	retain cumm_infoval 0; 
	cumm_Infoval + InfovalBand;
	BadRate 		= BandNoofBads/TotalinBand;
/*	BadRateRound 	= floor(BadRate/5);*/
run;
 
proc means data = &varname;
	var  InfovalBand;
	output out = inf&varname SUM(InfovalBand) = Infoval; 
run;
 
data inf&varname(drop = _type_ _freq_);
set inf&varname;
	length variable $32;
	Variable = "&varname";
run;
 
%mend MakeBinsD;
 
%macro CallMakeBinsWithList;
	%let j = 1;
	%do %while(%scan(&BinCVarList, &j) ne %str());
    	%let var = %scan(&BinCVarList, &j);
             %MakeBinsC(&var);       
    	%put &var;
    	%let j = %eval(&j+1);
	%end;
	%put &j;
 
	%let j = 1;
	%do %while(%scan(&BinDVarList, &j) ne %str());
    	%let var = %scan(&BinDVarList, &j);
             %MakeBinsD(&var);       
    	%put &var;
    	%let j = %eval(&j+1);
	%end;
	%put &j;
%mend CallMakeBinsWithList;
 
%CallMakeBinsWithList;
 
%let BinVarList = &BinCVarList &BinDVarList;
 
/* 
	Append the datasets created for each variable which have GP,BP,bandinfoval etc. 
   	Also append the datasets created for each variable that have the variable name and its infoval 
*/ 
 
%macro ForAppending;
	%let i = 1;
	%do %while(%scan(&BinVarList, &i) ne %str());
    	%let var = %scan(&BinVarList, &i);
             &var
    	%let i = %eval(&i+1);
	%end;
	%put &i;
%mend ForAppending;
 
%macro ForAppendingInfoval;
	%let i = 1;
	%do %while(%scan(&BinVarList, &i) ne %str());
    	%let var = %scan(&BinVarList, &i);
             inf&var
    	%let i = %eval(&i+1);
	%end;
	%put &i;
%mend ForAppendingInfoval;
 
/*****************************************************************************************************************/;
/*****************************************************************************************************************/;
/* Information value at bin level */
data appended;
set  %ForAppending;
run;

proc sql; 
	create 	table 	Bin_Infoval 	as 
			select 
					RankVar			as Rank,
					variable		as variable,
					Low				as Low,
					High			as High,
					TotalinBand		as Total,
					BandNoofGoods 	as Good,
					BandNoofBads 	as Bad,
					BadRate			as Bad_rate 			format percent10.3,
					WoE				as WoE					format 10.8,
					BP				as Pct_Bad  			format 10.4,
					TP				as Pct_Total			format 10.4,
					GP				as Pct_Good				format 10.4,
					InfovalBand		as Bin_Info_Val			format 10.4
			from appended;
quit;

/* Information value at overall level */
data AllInfoVals;
set  %ForAppendingInfoval;
run;

proc sql; 
	create table overall_infoval as 
		select 
			variable, 
			Infoval as Overall_Info_Val format percent10.0
		from AllInfoVals order by Infoval desc;
quit;
/*****************************************************************************************************************/;
/****Export appended and Allinfovals to excel and study trend ********/
ods tagsets.excelxp file="&output." style=statistical options(sheet_name='overall_infoval' 	sheet_interval='Proc');
	proc print data = overall_infoval; run;
ods tagsets.excelxp  				style=statistical options(sheet_name='Bin_Infoval' 		sheet_interval='Proc');
	proc print data = Bin_Infoval; run;
ods _all_ close;
/*****************************************************************************************************************/;
/******************* Delete all file from work library ***********************************************************/;
%macro del_lib();
	%if &delete_all_file. = 1 %then %do;
			proc datasets library=work kill; run; quit;
	%end;
%mend del_lib;

%del_lib();
/*****************************************************************************************************************/;
/*****************************************************************************************************************/;
%Mend Info_val_report;
/*****************************************************************************************************************/;
/*%Info_val_report;*/
/*********** END OF INFO VAL MACRO ******************************************************************************/;
/*****************************************************************************************************************/;




/**********************************************************************************************************************/
/************** Macro: Characteristic Analysis ************************************************************************/
/**********************************************************************************************************************/
/****** User Input ****************************************************************************************************/
/**********************************************************************************************************************/
/*%let Dev_data     	= local.ttd_dev; */				/* Development dataset name */
/*%let Val_data     	= local.ttd_val; */				/* Validation  dataset name */
/*%let dep_var  		= gbi_tag;  	*/ 		/* Dependend/bad Variable (bad=0/1) name */
/*%let Weight			= wgt;	*/		 		/* Weight variable name, */
/*%let No_Var			= 3;	*/		 		/* Total Number of variable for Characteristic Analysis*/
/*%let CA_Output		= C:\Users\amit.kumar\Desktop\SAS code - TTD Model\CA.xls; */ /* Output file name with location*/

/* List of variables for Characteristic Analysis */
/*%let CA_Var_list	= GENDER	CO_BORROWER BANK_ACCOUNT_STATUS	; */
/**********************************************************************************************************************/
/**********************************************************************************************************************/
/************** Do not change  ************************************************************************/
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
/************End Of CA macro *********************************************************************************************/
/**********************************************************************************************************************/






/**********************************************************************************************************************/
/******** Weight of Evidence Calculation from Bin variable ************************************************************/
/**********************************************************************************************************************/
/****** User Input ****************************************************************************************************/
/**********************************************************************************************************************/
/*%let lib	     	= work;	*/			/* Library name. Please define the Library */
/*%let Dev_data     	= Dev;	*/			/* Development dataset name */
/*%let Val_data     	= Val;	*/			/* Validation  dataset name */
/*%let dep_var  		= gbi_tag;*/  	 	/* Dependend/bad Variable (bad=0/1) name */
/*%let Weight_var		= wgt;*/			 	/* Weight variable name, if no weight then put 1 */
/*%let No_Var			= 6;*/			 	/* Total Number of variable for Characteristic Analysis*/
/*%let ext			= _B;*/				/* Suffix of bin variable */
/* List of variables for Characteristic Analysis */
/*%let WoE_Var_list	= GENDER	CO_BORROWER BANK_ACCOUNT_STATUS	GROSS_SAL RATE_OF_INTEREST AMOUNT_FINANACED; */
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


/**********************************************************************************************************/
/*************Calculate Score *****************************************************************************/
%Macro Scoring(indata, scoring_logic);
data &indata._scored;
set &indata.;
	LOG_ODD= &scoring_logic.;
	score=exp(log_odd)/(1+exp(log_odd));
	Reverse_Score=1-score;
	format scale_score Reverse_scale_Score 10.;
	scale_score = round(score*1000,1);
	Reverse_scale_Score=round(Reverse_Score*1000,1);
run;
%Mend Scoring;
/******************************************************************************************************/







/******************************************************************************************************/
/****KS Macro *****************************************************************************************/
/******************************************************************************************************/

%macro KS(in_data, bad, score, bin_ks, weight, no_bin );
		%let input_data     = &in_data.;/* Development_scored Validation_scored OOT_scored write name of input data */
		%let depdended_var  = &bad.;  	/* write name of dependent variable.  (bad=0/1) */
		%let good           = Good ;  	/* mention depdended_var=1 refer to good or bad */
		%let bad            = Bad;    	/* mention depdended_var=0 refer to good or bad */
		%let score_variable	= &score. ; /* Reverse_scale_Score scale_score */
		%let logic			= &bin_ks.; /* 1=bin wise KS table, rest only max KS */
		%let weight 		= &weight;	/* Weight variable. If no weight then give 1 */
		%let bin 			= &no_bin;	/* No of bin in lift table */
		%put &score_variable. ;

		%KS_CALCULATION(&score_variable.); /* KS calculation */
%mend KS;


/* KS calculation macro */
%macro KS_CALCULATION(var);

		data &input_data._1;
		set  &input_data.;
			wgt=&weight.;
		run;

		proc freq data = &input_data._1 noprint;
			tables &depdended_var. / out = BadsCount;
			weight wgt ;
		run;
		 
		proc sql noprint; select  count /*(&depdended_var.)*/ into:total_bad  	from BadsCount where &depdended_var.=1;quit;
		proc sql noprint; select  count /*(&depdended_var.)*/ into:total_good  	from BadsCount where &depdended_var.=0;quit;
		%let total   = %SYSEVALF(&total_bad + &total_good);
		%put &total.;
		%put &total_bad.;
		%put &total_good.; 

		proc rank data = &input_data._1 (keep = &var. &depdended_var. wgt) group=&bin. out=data1 ties=mean;
		        var &var.;
		        ranks rank_var;
		run;
		/**/
		data data1;
		set data1;
			_tot_=1;
		run;

		/* binwise Total, Min, Max  */
		proc summary SUM data = data1 nway;
			class rank_var;
			weight wgt;
			var  &var. &depdended_var. _tot_ ;
			output out = data2  (Drop = _FREQ_ _TYPE_  )
								MIN(&var.) 				= min_var 
								MAX(&var.) 				= max_var 
								mean(&var.)				= mean_var
								SUM(&depdended_var.) 	= nbr_&bad.
								sum(_tot_) 				= Total_bin; 
		run;
		 

		data data2;
		set  data2;
			nbr_&good. 	= Total_bin - nbr_&bad.	;
		run;

		 
		    proc sql noprint;
		        create table data3 as
		            select
		                *,
		                (nbr_&good./&total_good.)  	as pct_&good.,
		                (nbr_&good/Total_bin)   	as &good._rate,
		                (nbr_&bad./&total_bad.)   	as pct_&bad.,
		                (nbr_&bad./Total_bin)     	as &bad._rate
		        from data2;
		    quit;

		    proc sort data = data3; by rank_var; run;

		    data data4;
		    set  data3 ;
		        by rank_var;
		        if first.rank_var;
				retain cum_pct_&bad. cum_pct_&good. cum_lift 0		;
				cum_pct_&bad. 		= cum_pct_&bad. + pct_&bad.  	;
				cum_pct_&good.		= cum_pct_&good.+ pct_&good. 	;
				KS 					= abs(cum_pct_&good. - cum_pct_&bad.);
				pct_total			= (Total_bin/&total.) 				;
				lag_cum_pct_&bad.	= lag(cum_pct_&bad.)			;
				Gini				= ((sum(cum_pct_&bad.,lag_cum_pct_&bad.))/2)*(pct_total);
		/*		Lift				= (&bad._rate)*(&tot_bad_rate.) ;*/
		/*		cum_lift			= cum_lift+Lift					;*/
		/*		Gain_index			= Lift - 1						;*/
		/*		cum_gain			= (cum_gain + Gain_index)		;*/
		/*		Gain				= abs(cum_gain)					;*/


				label 
					rank_var 		=	'Rank' 
					min_var			=	'Min Score'
					max_var			=	'Max Score'			
					pct_total		=	'%Total'
					nbr_&good.		=	"# &good."
					&good._rate		=	"&good. rate"
					pct_&good.		=	"% &good."
					cum_pct_&good.	=	"Cum % &good."
					nbr_&bad.		=	"#&bad."
					&bad._rate		=	"Observed &bad. rate"
					mean_var		= 	'Expected Bad Rate'
					pct_&bad.		=	"% &bad."
					cum_pct_&bad.	=	"Cum % &bad."
		;
		    run;
		/* GINI INDFEX */
		/*	proc sql; select ((sum(Gini)-(0.5))/(0.5)) as Gini_index from data4; quit;*/

			proc sql; select max(KS) as KS FORMAT=PERCENT7.2 from data4; quit;

		    proc sql noprint;
		        create table KS as select
		            rank_var		Format=Comma20.0,
		            min_var			Format=Comma20.0,
		            max_var			Format=Comma20.0,
					Total_bin		Format=Comma20.0 as Total,
					nbr_&bad.		Format=Comma20.0,
					nbr_&good.		Format=Comma20.0,
					&bad._rate		FORMAT=PERCENT7.1 ,
					mean_var		FORMAT=PERCENT7.1 ,
				/*	&good._rate		FORMAT=PERCENT7.2,*/
					pct_total		FORMAT=PERCENT7.0,
		            pct_&bad.		FORMAT=PERCENT7.0,
		            pct_&good. 		FORMAT=PERCENT7.0,
					cum_pct_&bad.  	FORMAT=PERCENT7.0,
					cum_pct_&good. 	FORMAT=PERCENT7.0,
					KS 				FORMAT=PERCENT7.2

		    from data4;
		    quit;

		%if &logic. = 1 %then %do;
			proc print data = KS label ;	run;
		%end;

		proc delete data = data1 data2 data3 data4 KS BadsCount; run;

%mend KS_CALCULATION;

%KS(in_data=dev, bad=ab_ivpr_bad_pl, score=score, bin_ks=1, weight=1, no_bin=10 );
/******************************************************************************************************/
/****KS Macro End *************************************************************************************/
/******************************************************************************************************/


/******************************************************************************************************/
/*********** Logistic Model MACRO with KS table *********************************/;
/******************************************************************************************************/
%macro model(dev_data, val1_data,val2_data,val3_data, bad_var, model, score_var, Full_KS, weight_var, ks_bin);

proc logistic data=&dev_data &model. namelen=32;	/*&model.= descending */
model &bad_var. = &varlist. /lackfit;
Weight &weight_var.;
ods output parameterestimates=estimate; /* generate parameter estimate in dataset */
run;

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
proc sql; select final_eq into: final_equation from estimate4;quit; 	%put Logistic Equation: &final_equation.;

/* delete the dataset */;
proc delete data = estimate estimate1 estimate2 estimate3 estimate4; run;

/* macro for calculating the score and generating the lift table*/
%macro score(indata);
/* if dataset name is blank then it will not run */;
%if &indata. ne  %then %do;
/* score calculation */;
data &indata._scored;
set &indata.;
	LOG_ODD= &final_equation.;
	score=exp(log_odd)/(1+exp(log_odd));
	Reverse_Score=1-score;
	format scale_score Reverse_scale_Score 10.;
	scale_score = round(score*1000,1);
	Reverse_scale_Score=round(Reverse_Score*1000,1);
run;
/* generating the lift table*/;
Title "&indata."; 
%KS(in_data=&indata._scored,bad=&bad_var.,score=&score_var., bin_ks=&Full_KS., weight=&weight_var. , no_bin=&ks_bin.);
%end;
%mend score;

/* score */;
%score(&dev_data.);
%score(&val1_data.);
%score(&val2_data.);
%score(&val3_data.);

/* VIF */; 			
Title "&dev_data."; 
proc reg data=&dev_data.; 	
	model &bad_var. = 	&varlist./ VIF; 
	weight &weight_var.;
run; quit;

/* Correlation */ 	
Title "&dev_data."; 
proc corr data=&dev_data. NOSIMPLE NOPROB; 	
	var	&varlist. ; 
	weight &weight_var.;	
run; quit;

%mend model;

/**********************************************************************************************************/;
/************************ End Logistic Macro ***************************************************************/;
/**********************************************************************************************************/;

data test;
set card.cc_201606 (rename=ACCTOPENDATE=ACCTOPENDATE_char);

		/* ACCTOPENDATE : 16.06.2007 */
		d = substr(ACCTOPENDATE,1, 2);
		m = substr(ACCTOPENDATE,4, 2);
		y = substr(ACCTOPENDATE,7, 4);
		format datevar newvar4 date9.;
		datevar = mdy(m,d,y);
	 
		dd=trim(scan(ACCTOPENDATE, 1, '.'));
		mm=trim(scan(ACCTOPENDATE, 2, '.'));
		yyyy=trim(scan(ACCTOPENDATE, 3, '.'));

	format ACCTOPENDATE date9.;
	ACCTOPENDATE = mdy(trim(scan(ACCTOPENDATE_char, 2, '.')),trim(scan(ACCTOPENDATE_char, 1, '.')),trim(scan(ACCTOPENDATE_char, 3, '.')));
	drop ACCTOPENDATE_char;

run;

%macro chat_to_date(varname);
	format _&varname._ date9.;
	_&varname._ = mdy(trim(scan(&varname., 2, '.')),trim(scan(&varname., 1, '.')),trim(scan(&varname., 3, '.')));
	drop &varname.;
	rename _&varname._= &varname.;
%mend;

data test;
set card.cc_201606;
	%chat_to_date(ACCTOPENDATE);
run;

proc freq data = test noprint;
tables ACCTOPENDATE / out=FreqCount  ;
run;

/**** dataline sample*Qaa2021**********************/
data person; backup 
input ob $ ac $ ku $ po $;
datalines;
4558 9866 2392 2075
9154 6813 6676 1145
0938 5161 6084 7081
8495 4338 2649 9634
6458 3033 5362 4841;
run;
/*****Q18*******************************************************************/

options nocenter macrogen  MFILE symbolgen   mprint  mlogic  merror serror ls=144 ps=77 source2;
proc logistic data=dev descending;
 		model cb_bad_pl=
				cvt_achd_max_bal_current
				cnt_achl_netinc
				cit_achd_cr_txn_db_woe
				cb_achl_paytype_das
				cb_achl_indus_seg
				cb_ivpr_evr_lv_0plus_3m
				cnt_bur_mxdelq_3m_woe
				crt_achl_curr_emp
				
				 ;
 		CODE file = 'pprob.sas';	/* File pprob.sas saved in "C:\Users\1567478\pprob.sas" */
	run;

data dev1;
   set dev;
   %include pprob;
run;


proc freq data=dev1 noprint;
	tables P_cb_bad_pl1*cb_bad_pl  / measures;
	output out=somersd(keep=_SMDRC_)	 smdrc;
	run;

Title "Somers' D R|C";
proc print data = somersd; run;



/*Transpose*/
	proc transpose data=long1 out=wide1 prefix=faminc;
	    by famid ; 	    
	    var faminc;
	    id year; /*Var name*/ 
	run;

