

/***********************************************************************************************************************************/;
/*** Exploratory Data Analysis (EDA) provides basic distribution of data i.e., count, missing, unique, sum, mean, STD, percentile **/;
/***********************************************************************************************************************************/;

options nomprint nomlogic nosymbolgen;

/****************************************************************************************/ 
/* refer to : http://www2.sas.com/proceedings/sugi31/059-31.pdf */
/* PROGRAM: better_means */ 
/* AUTHORS: Myra A. Oltsik and Peter Crawford */ 
/* ORIGINAL DATE: 12/20/05 */ 
/* PURPOSE: Create a dataset with PROC MEANS statistics, with each record being */ 
/* one variable. Print stats if needed, too. Fixes ODS problems. */ 
/* */ 
/* NOTE: This macro has special handling for N, SUMWGT, KURT and SKEW. */ 
/* Also: STDEV, Q1, MEDIAN, Q3 are referred as STD, P25, P50, P75. */ 
/****************************************************************************************/ 
/****************************************************************************************/ 
/* MACRO PARAMETERS: */ 
/* required: none */ 
/* optional: print -- whether or not to print results to output */ 
/* data -- dataset name to be analysed */ 
/* sort -- sort order choice of the file of MEANS, by VARNUM or NAME */ 
/* stts -- indicate which statistics should included in the output */ 
/* varlst -- list of variables for means if not all numeric vars in file */ 
/* clss -- variable(s) for a class statement */ 
/* wghts -- variable for a weight statement */ 
/* defaults: */ 
/* data -- &syslast (most recently created data set) */ 
/* print -- Y */ 
/* sort -- VARNUM */ 
/* stts -- _ALL_ */ 
/* varlst -- _ALL_ */ 
/* */ 
/* Created Macro Variables: */ 
/* locals -- see inline comments at %local statement */ 
/* Creates Data Sets */ 
/* results are written to &data._means */ 
/* many data sets are created in the work library all prefixed _better_ */ 
/* but unless the testing option is set, the work data stes are deleted */ 
/* */ 
/* SAMPLES: */ 
/* %better_means(data=test); print all default statistics in a dataset */ 
/* %better_means(data=sashelp.class,stts=MEAN SUM); print only MEAN and SUM stats */ 
/* %better_means(data=sashelp.gnp,print=N,sort=NAME,stts=MIN MAX,varlst=INVEST */ 
/* EXPORTS); suppress list printing, limit output statistics and variables, and */ 
/* sort on NAME */ 
/* %better_means(data=sasuser.shoes,clss=PRODUCT); run all stats by PRODUCT field */ 
/* %better_means(data=sasuser.weighted,wghts=WGT); run all stats weighted on WGT */ 
/****************************************************************************************/;

%macro better_means( 
 					data 	= &syslast , 
 					print 	= Y,
					code	= Y,
 					sort 	= VARNUM, 
 					stts 	= _ALL_, 
 					varlst 	= _ALL_, 
 					clss 	= , 
 					wghts 	= , 
 					testing	= no, /* any other value will preserve the _better_: data sets */ 
						/****************************************************************************************/ 
						/* PROVIDE THE COMPLETE PROC MEANS STATISTIC LIST (FROM ONLINE-DOC) IF NONE STATED. */ 
						/****************************************************************************************/ 
 					_stts 	= N MEAN STD MIN MAX CSS CV LCLM NMISS 
 								P1 P5 P10 P25 P50 P75 P90 P95 P99 
								QRANGE RANGE PROBT STDERR SUM SUMWGT KURT SKEW T UCLM USS VAR 
 ); 

	data _better_miss_nonmiss_;
	set &data.;
	run;

 %local 
			 vLexist /* EXISTENCE OF LABELS ON INPUT DATASET */ 
			 s 		/* POINTER TO STATISTIC IN THE STATISTICS LIST */ 
			 stato 	/* HOLDER OF AN INDIVIDUAL STATISTIC NAME :- 
						 USED IN STATISTIC TABLE NAME, AND 
						 USED IN THE IN= VARIABLE DATASET OPTION 
			 		*/ 
			 full 	/* INDICATOR IN OUTPUT LABEL WHEN ALL STATS USED.*/ 
 ; 
/****************************************************************************************/ 
/* PUT STATS AND VAR PARAMETER LIST INTO UPPER CASE. */ 
/****************************************************************************************/ 
 %let varlst 	= %upcase(&varlst); 
 %let stts 		= %upcase(&stts); 
 %let data 		= _better_miss_nonmiss_ ; /* RESOLVE &syslast, WHEN DEFAULTED */ 
/****************************************************************************************/ 
/* GET THE NAMES/NUMBERS OF ALL VARIABLES INTO A LOOKUP FORMAT IF SORT ORDER = VARNUM. */ 
/****************************************************************************************/ 
 %if &sort eq VARNUM %then %do; 
	 proc contents data= &data out= _better_cols noprint; 	 run; 
	 data _better_cntl; 
		 retain 
			 FMTNAME '_bm_VN' 
			 TYPE 'I' 
			 HLO 'U' 
	 	; 
	 set _better_cols( keep= NAME VARNUM rename=( VARNUM=LABEL )); 
		 START = upcase( NAME) ; 
	 run; 
	 proc format cntlin= _better_cntl;  run; 
 %end; 

 /****************************************************************************************/ 
/* PROCESS STATISTICS CONDITIONS / COMBINATIONS */ 
/****************************************************************************************/ 
 %if &stts = _ALL_ or %length(&stts) = 0 %then %do; 
	 %let stts = &_stts ; 
	 %let full = FULL STATS; 
 %end; 
 %if %length(&wghts) %then %do; 
	 %* remove KURT and Skew when weights are present; 
	 %let stts = %sysfunc( tranwrd( &stts, KURT, %str( ) )); 
	 %let stts = %sysfunc( tranwrd( &stts, SKEW, %str( ) )); 
	 %let full = STATS ; 
 %end; 
 %else %do; 
	 %* remove SUMWGT when no weights present ; 
	 %let stts = %sysfunc( tranwrd( &stts, SUMWGT, %str( ) )); 
	 %let full = STATS ; 
 %end; 
/******************************************************************************************/
 	 proc sql;
		create table _better_cont as
			select 
				varnum,
				name,
				label, 
				type
			from dictionary.columns
			where upcase(libname)=upcase("work") and upcase(memname)=upcase("&data.");
	quit;

	proc sql; select count(NAME) into: Total_num	from _better_cont	where type = "num"; quit;  
			%put Total numeric variable &Total_num.;

	proc sql; select count(NAME) into: Total_Char	from _better_cont	where type = "char"; quit;  
			%put Total Char variable &Total_Char.;

	proc sql; select NAME into: varlst separated by ' ' from _better_cont	where type = "num"; quit; 
 /****************************************************************************************/ 
/* RUN PROC MEANS ON VARIABLES WITH OUTPUT FILE FOR EACH STATISTIC REQUESTED. MERGE */ 
/* DATASET OF LIST OF NUMERIC VARIABLES AND THEIR VARNUM. */ 
/****************************************************************************************/ 

%if &Total_num. >0 %then %do;

		 proc means data= &data noprint missing QMETHOD=P2; 
			 %if &varlst ne _ALL_ & %length(&varlst) %then %do; 
			 	var &varlst; 
			 %end; 
			 %if %length(&clss) %then %do; 
			 	class &clss; 
			 %end; 
			 %if %length(&wghts) %then %do; 
			 	weight &wghts; 
			 %end; 
			 %let s = 1 ; 
			 %let stato = %scan( &stts, 1 ); 
			 %do %while( %length(&stato) > 0 ); /* USING %LENGTH() FOR &STATO WORDS SIGNIFICANT TO %IF/%WHILE */ 
			 	output out= _better_&stato &stato= ; 
			 	%let s = %eval( &s +1 ); 
			 	%let stato = %scan( &stts, &s ); 
			 %end; 
		 run; 
		 
		data _better_means1; 
			 length _BETTER_ $32. /* STATS IDENTITY */ 	; 
		set /* ALL THOSE OUTPUT DATASETS FROM PROC MEANS */ 
			 %let stato = %scan( &stts, 1 ); 
			 %let s = 1 ; 
			 %do %while( %length(&stato) gt 0 ); 
			 	_better_&stato( in= _in_&stato ) /* NEED IN= VARIABLE TO IDENTIFY INPUT DATA */ 
			 	%let s = %eval( &s +1 ); 
			 	%let stato = %scan( &stts, &s ); 
			 %end; 
			 ; 
			 by _TYPE_ &clss; 
			 %let stato = %scan( &stts, 1 ); /* GENERATE _BETTER_ TO IDENTIFY EACH ROW OF RESULTS */ 
			 %let s = 1 ; 
			 %do %while( %length(&stato) > 0 ); 
			 	if _in_&stato then _BETTER_ = "%upcase( &stato )" ; else 
			 	%let s = %eval( &s +1 ); 
			 	%let stato = %scan( &stts, &s ); 
			 %end; 
			 ; 
		 run; 
		 
		 proc transpose data=_better_means1 out=_better_means2; 
		 	by _TYPE_ &clss ; 
		 	id _BETTER_ ; 
		 run;

		 /****************************************************************************************/ 
		/* FROM SAS FAQ # 1806: MACRO TO CHECK IF THE VARIABLE EXISTS IN A DATASET. */ 
		/****************************************************************************************/ 
		 %macro varcheck(varname,dsname); 
			 %local dsid vindex rc; 
			 %let dsid = %sysfunc(open(&dsname,is)); 
			 
			 %if &dsid EQ 0 %then %do; 
				 %put ERROR: (varcheck) The data set "&dsname" could not be found; 
			 %end; 
			 %else %do; 
				 %let vindex = %sysfunc(varnum(&dsid,&varname)); 
			 %end; 
			 
			 %let rc = %sysfunc(close(&dsid)); 
			 &vindex 
		 %mend varcheck; 
		 
		 %let vLexist = %varcheck(_LABEL_,_better_means2); 

		 /****************************************************************************************/ 
		/* CREATE BASIS FOR OUTPUT DATASET BASED ON DIFFERENT CONDITIONS AND PARAMETER CHOICES. */ 
		/****************************************************************************************/ 
		 %macro inL( list, seek )/ des= "Return TRUE, if seek in list, blank delimited"; 
		 	%sysfunc( indexw( &list, &seek )) 
		 %mend inL ; 

		 %macro now( fmt= datetime21.2 ) / des= "Timestamp"; 
		 	%sysfunc( datetime(), &fmt ) 
		 %mend now; 
		 
		 data _better_means_out; 
				 	length  _TYPE_ 8 ; 
				 	retain /* TO FIX ORDER OF THE FIRST FEW */ 		 
							&clss 
				 			%if &sort eq VARNUM %then %do; 
				 				VARNUM 
				 			%end; 
				 			NAME 
				 			%if &vLexist ne 0 %then %do; /* ADD IF TRANSPOSED DATASET CONTAINS THE LABEL VARIABLE */ 
				 				LABEL 
				 			%end; 
				 			%if %inL(&stts,N) %then %do; /* ADD % NOT MISSING IF STATISTIC "N" REQUESTED */ 
				 				N 
				 				PCT_POP 
				 				PCT_DEN 
				 			%end; 
				 	; 
			 set _better_means2(rename=( 
										_NAME_ = NAME 
					 					%if &vLexist ne 0 %then %do; 
					 						_LABEL_ = LABEL 
					 					%end; 
					 				)); 
					 %if %inL(&stts,N) %then %do; 
					 	format 
					 	PCT_POP percent.4 
					 	; 
						 if NAME = "_FREQ_" then do; 
							 PCT_DEN = N ; 
							 delete; 
						 end; 
					 	else do; 
							 if PCT_DEN then PCT_POP = N / pct_den ; 
						 end; 
					 	drop 			 PCT_DEN 			 ; 
					 %end; 
					 %else %do; 
					 	if NAME = "_FREQ_" then delete; 
					 %end; 
					 %if &sort eq VARNUM %then %do; 
					 	VARNUM = input(NAME,_bm_VN.); 
					 %end; 
					 NAMEU = upcase(NAME) ; 

					length _NAME1_ $32 ; Format _NAME1_ $32.;
						_NAME1_ = NAME;
					drop NAME;
						rename _NAME1_ = NAME;
			 run; 

		 /****************************************************************************************/ 
		/* CREATE FINAL DATASET WITH ALL STATISTICS, SORTED AS REQUESTED ON INVOCATION. */ 
		/****************************************************************************************/ 
		 proc sort data= _better_means_out out= _better_means_outs	(/*label= "&FULL FOR &data %NOW" */ drop= /*LABEL*/ varnum NAMEU 
				 %if %length(&clss) = 0 %then %do; 
				 _TYPE_ 
				 %end; 
				 ); 
				 by _TYPE_ &clss &sort; 
			 run; 


		%macro nlevels(var);
			ods output nlevels=Levels;
			proc freq data=dev nlevels ;
			   	tables &var./ noprint;
				run;

			data Levels;
				set Levels;
					format  name $32.;
					length 	name $32;
					name = TableVar;
					keep name NLevels;
				run;

			proc append data = Levels 	base= nlevels force ; 	run;
		%Mend nlevels;

		proc sql; select NAME into: NVar separated by ' ' from _better_cont	where type = "num"; quit;
		%do i = 1 %to &Total_num.; 
			%let _NVar_ = %scan(&NVar., &i.); 
			%nlevels(&_NVar_.);
		%end;
		
		proc sql; create table _better_means_outs as select a.*,b.NLevels from _better_means_outs as a left join nlevels as b on a.name=b.name;quit;
		proc delete data = nlevels Levels; run;
%END;

/**********************************************************************************************/
/* Calculate missing for char variables */
/**********************************************************************************************/

 %macro Missing_char();
			proc sql; select NAME into: Varname separated by ' ' from _better_cont	where type = "char"; quit;  
				%put Char Variable list &Varname.;
		    %do i = 1 %to &Total_Char.; 								
		        %let Char_Var = %scan(&Varname., &i.); 		
		        %put &Char_Var.; 							
		        %Char_missing_Cal(&Char_Var.); 						
		    %end;
%mend Missing_char;

/* calculate missing & non-missing for char variables */
%Macro Char_missing_Cal(Var);
			proc sql;
				create table _better_test as 
					select 
							"&Var." 							as name 		format  $32. 	length 32, 
							count(&Var.) 						as N 			format	BEST12.,
							count(distinct &Var.) 				as NLevels 		format	BEST12.,
							Nmiss(&Var.) 						as NMISS 		format	BEST12.
					from &data. ;
				
			quit;

			proc append data = _better_test 	base= _better_char_miss force ; 	run;

%mend Char_missing_Cal;

%if &Total_Char. >0 %then %do;
	%Missing_char;
%end;
/**********************************************************************************************/
 
%if &Total_Char. >0 and &Total_num. >0 %then %do;
		data 	_better_means_all;
			set 	_better_means_outs	_better_char_miss;
					Total 		= N + NMISS;
					PCT_MISSING = NMISS/Total;
			/*		drop varnum LABEL;*/
			RUN;

		proc sql;
			create table _better_all_type as 
				select 
					a.*,
					b.varnum,
					b.label,
					b.type
				from _better_means_all as a 
				left join _better_cont as b on upcase(a.name) = upcase(b.name)
				order by type, varnum;
			quit;

		data _better_all_type;
					retain  VARNUM NAME RENAME_VAR type LABEL N NMISS Total NLevels PCT_MISSING Missing_treatment MEAN
					STD MIN P1 P5 P10 P25 P50 P75 P90 P95 P99 MAX ;
			set  _better_all_type;

				Format Missing_treatment $54.;
				If type = "num" then do;
							if 						STD = 0	or NLevels eq 1				then 	Missing_treatment = "Drop - Constant";
					Else	if 						P99	= P1							then 	Missing_treatment = "Drop - P1 eq P99";
					Else	if 						PCT_MISSING >  &drop_pct. 			then 	Missing_treatment = "Drop - Missing 100%";
				/*	Else	if 	&default_pct. <= 	PCT_MISSING <  &drop_pct. 			then 	Missing_treatment = "default value";*/
				/*	Else	if 				0 <		PCT_MISSING <= &default_pct. 		then 	Missing_treatment = "Replace by Mean";*/
					Else 																		Missing_treatment = "Keep";	
				END;
				Else 	if type = "char" then do;
							if 						PCT_MISSING >  &drop_pct. 			then 	Missing_treatment = "Drop - Missing 100%";
				/*	Else	if 				0 <		PCT_MISSING <= &drop_pct.			then 	Missing_treatment = "default value";*/
					Else 																		Missing_treatment = "Keep";
				end;
				format RENAME_VAR $4.;
				if length(NAME) >31 then RENAME_VAR ='Yes'; else RENAME_VAR='No'; 
					label RENAME_VAR = 'if yes then reduce the variable name';
					/*		drop pct_pop;*/
					keep VARNUM NAME RENAME_VAR type LABEL N NMISS Total NLevels PCT_MISSING Missing_treatment MEAN
					STD MIN P1 P5 P10 P25 P50 P75 P90 P95 P99  MAX;
			run;
%end;

%Else %if &Total_num. >0 %then %do;
		data 	_better_means_all;
			set 	_better_means_outs;
					Total 		= N + NMISS;
					PCT_MISSING = NMISS/Total;
			/*		drop varnum LABEL;*/
			RUN;

		proc sql;
			create table _better_all_type as 
				select 
					a.*,
					b.varnum,
					b.label,
					b.type
				from _better_means_all as a 
				left join _better_cont as b on upcase(a.name) = upcase(b.name)
				order by type, varnum;
			quit;

		data _better_all_type;
					retain  VARNUM NAME RENAME_VAR type LABEL N NMISS Total NLevels PCT_MISSING Missing_treatment MEAN
					STD MIN P1 P5 P10 P25 P50 P75 P90 P95 P99  MAX ;
			set  _better_all_type;

				Format Missing_treatment $54.;
							if 						STD = 0	or NLevels eq 1				then 	Missing_treatment = "Drop - Constant";
					Else	if 						P99	= P1							then 	Missing_treatment = "Drop - P1 eq P99";
					Else	if 						PCT_MISSING >  &drop_pct. 			then 	Missing_treatment = "Drop - Missing 100%";
				/*	Else	if 	&default_pct. <= 	PCT_MISSING <  &drop_pct. 			then 	Missing_treatment = "default value";*/
				/*	Else	if 				0 <		PCT_MISSING <= &default_pct. 		then 	Missing_treatment = "Replace by Mean";*/
					Else 																		Missing_treatment = "Keep";	
				format RENAME_VAR $4.;
				if length(NAME) >31 then RENAME_VAR ='Yes'; else RENAME_VAR='No'; 
					label RENAME_VAR = 'if yes then reduce the variable name';
				/* drop pct_pop;*/
					keep VARNUM NAME RENAME_VAR type LABEL N NMISS Total NLevels PCT_MISSING Missing_treatment MEAN 
					STD MIN P1 P5 P10 P25 P50 P75 P90 P95 P99  MAX;
			run;
%end;

%Else %if &Total_Char. >0 %then %do;
		data 	_better_means_all;
			set 	_better_char_miss;
					Total 		= N + NMISS;
					PCT_MISSING = NMISS/Total;
			RUN;

		proc sql;
			create table _better_all_type as 
				select 
					a.*,
					b.varnum,
					b.label,
					b.type
				from _better_means_all as a 
				left join _better_cont as b on upcase(a.name) = upcase(b.name)
				order by type, varnum;
			quit;

		data _better_all_type;
					retain  VARNUM NAME RENAME_VAR type LABEL N NMISS Total NLevels PCT_MISSING Missing_treatment ;
			set  _better_all_type;

				Format Missing_treatment $54.;
							if 			NLevels 	eq 1			then 	Missing_treatment = "Drop - Constant";
					Else	if 			PCT_MISSING >  &drop_pct.	then 	Missing_treatment = "Drop - Missing 100%";
					Else 													Missing_treatment = "Keep";
				format RENAME_VAR $4.;
				if length(NAME) >31 then RENAME_VAR ='Yes'; else RENAME_VAR='No'; 
					label RENAME_VAR = 'if yes then reduce the variable name';
				/*	drop pct_pop;*/
			run;
%end;

proc sql;
		create table &data._means as 
				select 	*
						/*			VARNUM,*/
						/*			NAME,*/
						/*			RENAME_VAR,*/
						/*			type,*/
						/*			LABEL,*/
						/*			N,*/
						/*			NMISS,*/
						/*			Total,*/
						/*			PCT_MISSING,*/
						/*			Missing_treatment,*/
						/*			MEAN,*/
						/*			STD,*/
						/*			MIN,*/
						/*			P50,*/
						/*			P75,*/
						/*			P90,*/
						/*			P95,*/
						/*			P99,*/
						/*			MAX*/
			from _better_all_type;
		quit;

proc export data	=	&data._means outfile="&output." dbms = csv replace; run;

/* MISSING VALUE TREATMENT */

%if &code = Y %then %do; 
		filename drop "&SAS_CODE_LOCATION.drop_list.sas";                                                                                             

		data _null_; 
		   set &data._means		end=last ;                                                                                                                          
			   file drop; 
						if _n_ = 1 then put ' %let drop_list = ';
						if Missing_treatment = "Drop"  then do; 
			   			put 	NAME	;
						end;
			            if last then put ";" ;
			run;

		filename nkeep "&SAS_CODE_LOCATION.Num_var_keep_list.sas";                                                                                             

		data _null_; 
		   set &data._means		end=last ;                                                                                                                          
			   file nkeep; 
						if _n_ = 1 then put ' %let Num_var_keep_list = ';
						if Missing_treatment = "Keep"  and type = "num" then do; 
			   			put 	NAME	;
						end;
			            if last then put ";" ;
			run;

		filename ckeep "&SAS_CODE_LOCATION.char_var_keep_list.sas";                                                                                             

		data _null_; 
		   set &data._means		end=last ;                                                                                                                          
			   file ckeep; 
						if _n_ = 1 then put ' %let char_var_keep_list = ';
						if Missing_treatment = "Keep"  and type = "char" then do; 
			   			put 	NAME	;
						end;
			            if last then put ";" ;
			run;
%end;

/****************************************************************************************/ ;
/* IF PRINTED OUTPUT IS REQUESTED, DO SO HERE. */ ;
/****************************************************************************************/ ;
%if &print = Y %then %do; 
	proc print data=&data._means; 
			title3 "MEANS FOR &data"; 
			%if %length(&clss) > 0 %then %do; 
			by _TYPE_; 
			%end; 
 		run; 
%end; 
 
%if &testing = no %then %do; 
/****************************************************************************************/ 
/* CLEAN UP REMAINING TEMPORARY DATASETS. */ 
/****************************************************************************************/ 
	 proc datasets lib= work nolist; 
		 delete _better_: ; 
		 run; quit; 
%end; 
%mend better_means; 

/****************************************************************************************************************/
/***END of macro ************************************************************************************************/
/****************************************************************************************************************/
/*****User input ************************************************************************************************/

Libname local "C:\data\";

%let drop_pct 			= 0.998;
%let output			= C:\data\EDA.csv ;
/*%let SAS_CODE_LOCATION	= C:\data\ ;*/

%better_means( 
 				data 	=  dev					, 
 				varlst 	= 					, 
 				clss 	= 					,
 				wghts 	= 					,
				code	= N					,
				print 	= N			
);

/*********************************************************************************************/

/*proc contents data = temp; run;*/
/*data Dev;*/
/*	set sashelp.zipcode;*/
/*	run;*/
