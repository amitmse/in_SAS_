
options mprint mlogic;

/*******************************************************************************************************************/
/******************* INFO VAL MACRO ********************************************************************************/
/*******************************************************************************************************************/
/*Please note GBI_Tag is dependent variable in this code */
/*Dataset name has to be DEVBASE and dependent variable name has to be GBI_TAG. Please drop indeterminate if any. */
/*remove the variable which is totally missing. then run the below code to replace the missing with -999 */
/*******************************************************************************************************************/

/**************** User Input **************************************************************************************/;

%let out = C:\Users\Lenovo\Downloads\fwdproject\test;
libname local  "&out.";



data Dev;
set local.ttd_dev;
run;

%let indata  			= Dev;			/* input data name with libname */
%let libname 			= work;
%let dep_var 			= GBI_Tag;  		/* mention name of dependent variable. without indeterminate */
%let weight  			= wgt; 			/* weight variable. if there is no weight then put 1 */
%let output	 			= &out.; 		/* Output report file with path */
%let delete_all_file 	= 0;			/* 1= delete all files in work, 0=do not delete */

/* Numeric variables list */
%let BinCVarList = 
ADVEMI
AMOUNT_FINANACED
APPLICANT_AGE
PROCESSING_FEES
RATE_OF_INTEREST
SOURCE_CODE
TENURE
;
%put &BinCVarList;
 
/* Character variables list */
%let BinDVarList = 
APPLICANT_TYPE
APPLICATION_STATUS
AREA
GENDER
GUARANTOR
INSURANCE_TYPE


;
%put &BinDVarList;

/*******************************************************************************************************************/
/*******************************************************************************************************************/
/**************** Do not change below ******************************************************************************/
/*******************************************************************************************************************/

%Macro Info_val_report();

data DEVBASE /*DEVBASE_Missing*/ ;
set  &indata. ;
	wgt		=	&weight.;
	GBI_tag = 	&dep_var.;
/* keep only required variables */
keep GBI_tag wgt &BinCVarList.  &BinDVarList.;
run;

/* Missing value treatment: Assign -999 for missing value  */
/*PROC STDIZE DATA= DEVBASE_Missing  OUT=DEVBASE REPONLY MISSING = -999; VAR _NUMERIC_; RUN;*/

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
%let badrate   = %SYSEVALF(&NoofBads./&TotalPop. );
%put &badrate.;

/*Info val for Numeric variable:
	1. Using proc rank to find maximum 20 bins for each continuous variable 
   	2. Find the infoval of that variable 
*/
 
%macro MakeBinsC(Varname);
/* Raw binning of variable by using proc rank */
proc rank data =  DevBase(keep = &varname GBI_tag wgt) out = CoarseRanked groups = 10 ;
                var &Varname;
                ranks RankVar;
run;

data CoarseRanked;
set CoarseRanked;
	_tot_=1;
	if RankVar = . then RankVar= -1 ;
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
	else 	if GP 		= 	0 then 	WoE = 0; 
	else 							WoE = 0;

	InfovalBand 	= (GP - BP)*WoE;
	WoeRound 		= floor(WoE*10);
	retain cumm_infoval cum_pct_bad cum_pct_good 0; 
	cumm_Infoval	= cumm_Infoval + InfovalBand;
	BadRate 		= BandNoofBads/TotalinBand;

	cum_pct_bad 		= cum_pct_bad + BP   	;
	cum_pct_good		= cum_pct_good+ GP 		;
	format KS percent10.4	;
	KS 					= abs(cum_pct_good - cum_pct_bad);

	
	Length Histogram_WOE  $20;
		len_woe		=	abs(round(7*WoE,1))-1;

		if len_woe > 14 then len_woe=14;

		if woe >0 then do;
					if len_woe <0 then 			Histogram_WOE = "+";
			else 								Histogram_WOE = "+"||repeat("*",len_woe);
		end;
		else do;
					if len_woe < 0 then 		Histogram_WOE = "+";
			else 								Histogram_WOE = repeat("*",len_woe)||"+";
		end;

			Length  Histogram_BadRate $120;
if &badrate. > 0.3 then do;
			len_BadRate	=	abs(round(10*BadRate,1));
end;
else do;
			len_BadRate	=	abs(round(100*BadRate,1));
end;
					if len_BadRate <= 0 then 	Histogram_BadRate = "+";
			else 								Histogram_BadRate = repeat("*",len_BadRate)||"+";
format Type $4.;
			Type ="num";

		drop cum_pct_bad cum_pct_good len_woe len_BadRate;
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
    else 	if GP 		= 0 	then 	WoE = 0; 
	else 								WoE = 0;

	InfovalBand 	= (GP - BP)*WoE;
	WoeRound 		= floor(WoE*10);
	retain cumm_infoval cum_pct_bad cum_pct_good	0; 
	cumm_Infoval + InfovalBand;
	BadRate 		= BandNoofBads/TotalinBand;

	cum_pct_bad 		= cum_pct_bad + BP   	;
	cum_pct_good		= cum_pct_good+ GP 		;
	format KS percent10.4	;
	KS 					= abs(cum_pct_good - cum_pct_bad);

	Length Histogram_WOE  $20;
		len_woe		=	abs(round(7*WoE,1))-1;

		if len_woe > 14 then len_woe=14;
		if len_woe > 14 then len_woe=14;

		if woe >0 then do;
					if len_woe <0 then 			Histogram_WOE = "+";
			else 								Histogram_WOE = "+"||repeat("*",len_woe);
		end;
		else do;
					if len_woe < 0 then 		Histogram_WOE = "+";
			else 								Histogram_WOE = repeat("*",len_woe)||"+";
		end;

					Length  Histogram_BadRate $120;
if &badrate. > 0.3 then do;
			len_BadRate	=	abs(round(10*BadRate,1));
end;
else do;
			len_BadRate	=	abs(round(100*BadRate,1));
end;
					if len_BadRate <= 0 then 	Histogram_BadRate = "+";
			else 								Histogram_BadRate = repeat("*",len_BadRate)||"+";
format Type $4.;
			Type ="char";

		drop cum_pct_bad cum_pct_good len_woe len_BadRate;

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
		create table cont as
			select 
				name,
				label 
/*				type*/
			from dictionary.columns
			where upcase(libname)=upcase("&libname.") and upcase(memname)=upcase("&indata.");
	quit;

	proc sql; 
	create 	table 	Bin_Infoval 	as 
			select 
					a.RankVar			as Rank			,
					a.variable			as variable		,
					a.Low				as Low			,
					a.High				as High			,
					a.TotalinBand		as Total		,
					a.BandNoofGoods 	as Good			,
					a.BandNoofBads 		as Bad			,
					a.BadRate			as Bad_rate 			format percent10.3	,
					a.WoE				as WoE					format 20.8			,
					a.BP				as Pct_Bad  			format percent10.4	,
					a.TP				as Pct_Total			format percent10.4	,
					a.GP				as Pct_Good				format percent10.4	,
					a.InfovalBand		as Bin_Info_Val			format percent10.8	,
					a.KS				as KS					,
					a.Histogram_WOE		as Histogram_WOE		,
					a.Histogram_BadRate	as Histogram_BadRate	,
					b.label				as Label				,
					a.type				as Type			

			from appended as a 
			left join cont as b on upcase(a.variable) = upcase(b.name)
			order by variable, RankVar ;
quit;

proc sql; create table mx_ks as select distinct variable, max(ks) as KS from Bin_Infoval group by variable; quit;

/* SAS code*/
filename woe "&output.\woe_code.sas";

data _null_; 
   set Bin_Infoval;                                                                                                                          
   file woe; 
	variablew=trim(variable)||"W";

	if _n_=1 then do ;
	      put '/************--------------- WEIGHT-OF-EVIDENCE CODES FOR ------------------*************************/' ;
		  put ' ';
	      put '/**Chech the SAS code for scientific no.(Minimum/Maximum). update the number from excel output**/' ;
		  put '/****************************************************************************************************/';
		  put ' ';
	end ;

	If type = "num" then do;
	put @2 " IF " @10 Low @20 " <= " @30 variable @65 " <= " @75 High @90 "   THEN " @100 variablew @130  "  = " @135 WoE  @155 " ; "	;
	end;
run;

data _null_; 
   set Bin_Infoval;                                                                                                                          
   file woe mod; 
	variablew	=trim(variable)||"W";
	Highw		="'"||trim(High)||"'";
	If type = "char" then do;
	put @2 " IF " @10 variable @45 " in ( " @53 Highw @90 " ) THEN " @100 variablew @130  "  = " @135 WoE  @155 " ; "	;
	end;
run;

filename ca "&output.\ca_code.sas";

data _null_; 
   set Bin_Infoval;                                                                                                                          
   file ca; 
	variablew	=trim(variable)||"B";
	if _n_=1 then do ;
	      put '/*************----------------- Characteristic Analysis CODES FOR ------------***********************/' ;
		  put ' ';
		  put '/**Chech the SAS code for scientific no.(Minimum/Maximum). update the number from excel output**/' ;
		  put '/****************************************************************************************************/';
	      put ' ' ;
	end ;
	If type 	= "num" then do;
	put @2 " IF " @10 Low @20 " <= " @30 variable @65 " <= " @75 High @90 "   THEN " @100 variablew @130  "  = " @135 Rank  @155 " ; "	;
	end;
run;

data _null_; 
   set Bin_Infoval;                                                                                                                          
   file ca mod; 
	variablew	=trim(variable)||"b";
	Highw		="'"||trim(High)||"'";
	If type = "char" then do;
	put @2 " IF " @10 variable @45 " in ( " @53 Highw @90 " ) THEN " @100 variablew @130  "  = " @135 Rank  @155 " ; "	;
	end;
run;


/* Information value at overall level */
data AllInfoVals;
set  %ForAppendingInfoval;
run;

proc sql; 
	create table overall_infoval as 
		select 
			a.variable, 
			a.Infoval 	as 	Overall_Info_Val 	format percent10.4,
			b.KS		as	KS					format percent10.4
		from AllInfoVals as a 
		left join mx_ks  as b on upcase(a.variable) = upcase(b.variable)
		order by Infoval desc;
quit;
/*****************************************************************************************************************/;
/****Export appended and Allinfovals to excel and study trend ********/
/*
ods tagsets.excelxp file="&output." style=statistical options(sheet_name='overall_infoval' 	sheet_interval='Proc');
*/
ods html file = "&output.\overall_infoval.xls";
	proc print data = overall_infoval; run;
ods html close;

/*
ods tagsets.excelxp  				style=statistical options(sheet_name='Bin_Infoval' 		sheet_interval='Proc');
*/
ods html file = "&output.\Bin_Infoval.xls";
	proc print data = Bin_Infoval; run;
ods html close;

/*ods _all_ close;*/
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
/***************** Call Info Val macro ***************************************************************************/;
%Info_val_report;
/*********** END OF INFO VAL MACRO ******************************************************************************/;
/*****************************************************************************************************************/;


 

