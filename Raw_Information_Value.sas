/**/


/***---
      libin 	= input libname; if not defined in autoexec.sas, define above
      indata 	= input data set name
      dv 		= good-bad flag
      target 	= 1 if logistic regression will be modeling 1, or 0 if logistic regression will be modeling 0 
      keeplst 	= list of candidate numeric predictors
      binproc 	= RANK (if "proc rank" is used to determine initial bins; this procedure does not allow appropriate 
					weighting of observations in "proc rank", but the
                	rest of procedures appropriately weighs the observations) 
                	or 
				  DATA (if data step is used insted; this approach allows appropriate weighing of observations 
					in all the procedures used)
      miss 		= deault letter missing value assigned to missing value = . (used to handle situations where 
					"proc transreg" does not put all
             		missing value = . in one variable bin'' make sure that this letter is not assigned to any 
					other variable as default missing value 
      weight 	= weight variable (leave blank if not applicable)
      modlabel 	= model label
      woeout 	= output file name containing the dummy-variable codes
	  Raw_bin	= 1:Information value based on raw binning 
	  CA_CODE	= 1:SAS code for Characteristic Analysis
	  out_data 	= output dataset which will have WOE and CA variables
---***/

%MACRO WOE(	) ;

options  LINESIZE=MAX;

/*  %if %sysfunc(exist(autobin_binwise)) %then %do; proc delete data = autobin_&woeout. ; run; %end;*/

  data &indata._ ;
  set &libin..&indata (keep = &keeplst. &dv. &weight.) end=last;
		    %if &target. = 1 %then %do ;  dv 	= &dv ; 	%end ; 	%else %do ; dv 		= (&dv = 0) ;	%end ;
		    %if &weight. ne  %then %do ;  weight = &weight ;%end ; 	%else %do ;	weight 	= 1 ;		    %end ;

			obs = 1 ;
		    tot_obs + weight ;
		    tot_dvs + dv*weight ;

		    if last then do ;
		      	call symput('tot_obs',left(put(tot_obs,best15.))) ;
		      	call symput('tot_dvs',left(put(tot_dvs,best15.))) ;
		    end ;

  run ;

  %let tot_nodvs 	= %sysevalf(&tot_obs - &tot_dvs) ;
  %let size 		= %sysevalf(&tot_obs/&group,ceil) ;

  %put TOT_OBS = &tot_obs TOT_DV = &tot_dvs TOT_NODV = &tot_nodvs SIZE = &size ;

  proc sql  ;
	    select name	into :char_varlst separated by ' '	from dictionary.columns	
		where 
				memname = "%upcase(&indata._)" 	and	
				libname = 'WORK' 				and	
				type 	= 'char' 				and 
	          	upcase(name) not in ("%upcase(&dv)",'DV',"%upcase(&weight)",'WEIGHT','OBS','TOT_OBS','TOT_DVS')
	    order by name ;
  quit ;

  %let charvar = &sqlobs ;


  proc sql  ;
	    select name	into :Num_varlst separated by ' '	from dictionary.columns	
		where 
				memname = "%upcase(&indata._)" 	and	
				libname = 'WORK' 				and	
				type 	= 'num' 				and 
	          	upcase(name) not in ("%upcase(&dv)",'DV',"%upcase(&weight)",'WEIGHT','OBS','TOT_OBS','TOT_DVS')
	    order by name ;
  quit ;

  %let Numvar = &sqlobs ;

  	proc sql noprint; select  sum(dv) 	into:Total_Bad  from &indata._ ;quit;
	proc sql noprint; select  count(dv) into:TotalPop 	from &indata._ ;quit;

			%let Total_Good = %SYSEVALF(&TotalPop - &Total_Bad);				
			%let badrate  	= %SYSEVALF(&Total_Bad./&TotalPop. );

					%put &TotalPop.;
					%put &Total_Bad;   
					%put &Total_Good;  
					%put &badrate.;


/************************************************************************************/
/*********NUMERIC VARAIBELS *********************************************************/
/************************************************************************************/

  	%if &Numvar. >0 %then %do;
  		%do i = 1 %to &Numvar. ;    
  			%let var = %upcase(%scan(&Num_varlst,&i)) ;
				proc rank data 	= &indata._ group=&group 
						out 	= regout2(keep = obs &var weight &dv dv rank_var );
						var 	&var ;
			        	ranks 	rank_var ;
			    run ;

				data raw_test1;
				set regout2;
					_tot_=1;
					if rank_var <= .Z  then rank_var= -1 ;
				run;
 
				proc summary SUM data = raw_test1 nway;
					class rank_var;
					weight weight;
					var  &var. dv _tot_  ;
					output out = raw_test2  (Drop = _FREQ_ _TYPE_ )
										MIN(&var.) 	= Low 
										MAX(&var.) 	= High 
										SUM(dv) 	= BandNoofBads
										sum(_tot_) 	= TotalinBand; 
				run;
				
				data raw_test3 ;
					retain rank_var	Var Low	High TotalinBand BandNoofGoods BandNoofBads  	
							TP	GP	BP WoE Info_Val BadRate KS Histogram_WOE Histogram_BadRate;
				set  raw_test2 ;

					length Var $32;
					Var = "&var.";
					BandNoofGoods 	= TotalinBand - BandNoofBads	;
					TP 				= TotalinBand/&TotalPop.		;
					GP 				= BandNoofGoods/&Total_Good.	;
					BP 				= BandNoofBads/&Total_Bad.	;

							if GP*BP 	ne 	0 then 	WoE = log((BandNoofGoods/&Total_Good.)/(BandNoofBads/&Total_Bad.));
					else 	if BandNoofGoods <= .Z or BandNoofGoods = 0 or BandNoofBads <= .Z or BandNoofBads =0 then WoE = .; 
					
					Info_Val 		= (GP - BP)*WoE;
					if BandNoofGoods <= .Z or BandNoofGoods = 0 or BandNoofBads <= .Z or BandNoofBads =0 then Info_Val = .;  
					retain cum_pct_bad cum_pct_good 0; 
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

						drop cum_pct_bad cum_pct_good len_woe len_BadRate;
				run;
				proc append data = raw_test3 base = NUM_Raw_binwise force ;	run ;
				proc delete data = raw_test1 raw_test2 raw_test3 regout2 ; run;
  		%end ;
	%end;

/************************************************************************************/
/*********CHARACTER VARAIBELS *******************************************************/
/************************************************************************************/

	%if &charvar. > 0 %then %do;
		%do c = 1 %to &charvar. ;    
  			%let var = %upcase(%scan(&char_varlst.,&c.)) ;
				proc sql; create table _char_var1 as 
					select  
						&var. Length 50,
						sum(wgt) 								as total,
						sum(dv*wgt) 							as bad,
						(calculated total - calculated bad ) 	as Good,
						(calculated bad)/(calculated total)  	as bad_rate format percent8.2
					from &indata._ 
					group by &var.
					order by bad_rate desc  ;
				quit;
			
				data _char_raw1;
					set _char_var1;

						_tot_=1;
						rank_var+1;
						length Var $32;
						Var 			= "&var.";
						Low				= &var.	;
						High			= &var.	;

						rename 	total 	= TotalinBand
								bad	  	= BandNoofBads
								Good	= BandNoofGoods
								bad_rate= BadRate
						;
					run;

				data _char_raw2 ;
					retain rank_var	Var Low	High TotalinBand BandNoofGoods BandNoofBads  	
							TP	GP	BP WoE Info_Val BadRate KS Histogram_WOE Histogram_BadRate;
				set  _char_raw1 (drop= _tot_ &var.);

					TP 				= TotalinBand/&TotalPop.		;
					GP 				= BandNoofGoods/&Total_Good.	;
					BP 				= BandNoofBads/&Total_Bad.	;

							if GP*BP 	ne 	0 then 	WoE = log((BandNoofGoods/&Total_Good.)/(BandNoofBads/&Total_Bad.));
					else 	if BandNoofGoods <= .Z or BandNoofGoods = 0 or BandNoofBads <= .Z or BandNoofBads =0 then WoE = .; 
					
					Info_Val 		= (GP - BP)*WoE;
					if BandNoofGoods <= .Z or BandNoofGoods = 0 or BandNoofBads <= .Z or BandNoofBads =0 then Info_Val = .;  
					format KS percent10.4	;
					retain cum_pct_bad cum_pct_good 0; 
					cum_pct_bad 		= cum_pct_bad + BP   	;
					cum_pct_good		= cum_pct_good+ GP 		;					
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
					if &badrate. > 0.3 then do; len_BadRate	= abs(round(10*BadRate,1)); 	end;
					else do; 					len_BadRate	=	abs(round(100*BadRate,1));	end;
					if len_BadRate <= 0 then 	Histogram_BadRate = "+";
					else 						Histogram_BadRate = repeat("*",len_BadRate)||"+";

					drop cum_pct_bad cum_pct_good len_woe len_BadRate;
				run;
				proc append data = _char_raw2 base = Char_Raw_binwise force ;	run ;
				proc delete data = _char_var1 _char_raw1 _char_raw2 ; run;
		%end;
	%end;

/**********end of char **************************************************************/
/************************************************************************************/
/*********Output print in excel*****************************************************/
/*********RAW bin ******************************************************************/

 	%if &excel. =2 %then %do; ods tagsets.excelxp file = "&output.Information_value.xls"; %end;
  	%if &Raw_bin = 1 %then %do;
		%if %sysfunc(exist(NUM_Raw_binwise)) %then %do;
			  proc sql; 
				create table NUM_Raw_Overall as 
					select 
							distinct Var			as Var,
							sum(Info_Val) 			as Info_Val,   
							max(ks) 				as KS format percent10.4,
							"NUM"					as VAR_TYPE length 32 
					from  NUM_Raw_binwise 
					group by Var
					order by Info_Val desc; 
			 quit;
			 proc append data = NUM_Raw_Overall base=Raw_Overall; quit;
			 Proc delete data = NUM_Raw_Overall ; run;
		%END;
		%if %sysfunc(exist(Char_Raw_binwise)) %then %do;
			 proc sql; 
				create table Char_Raw_Overall as 
					select 
							distinct Var			as Var,
							sum(Info_Val) 			as Info_Val,   
							max(ks) 				as KS format percent10.4,
							"CHAR"					as VAR_TYPE length 32
					from  Char_Raw_binwise 
					group by Var
					order by Info_Val desc; 
			 quit;
			 proc append data = Char_Raw_Overall base=Raw_Overall force; quit;
			 Proc delete data = Char_Raw_Overall; run;
		%END;	

		%if %sysfunc(exist(Char_Raw_binwise)) %then %do;
			%if &excel. =1 %then %do; ods html file = "&output.Raw_binwise_infoval_Char.xls"; %end;
			%if &excel. =2 %then %do;
		 		ods tagsets.excelxp options(orientations = 'Landscape' Fitpage='yes' pages_fitwidth ='1'
				pages_fitheight='100' foreground = 'whilte' font_size = "xx-small" sheet_name= "Raw_binwise_infoval_Char");
				Title 'Binwise Information value based on raw binning for Char var';
			%end;
			proc print data = Char_Raw_binwise noobs;  run ;
			%if &excel. =1 %then %do; ods html close; %end;
			proc delete data = Char_Raw_binwise; run;
		%end;

		%if %sysfunc(exist(NUM_Raw_binwise)) %then %do;
			%if &excel. =1 %then %do; ods html file = "&output.Raw_binwise_infoval_Num.xls"; %end;
			%if &excel. =2 %then %do;
		 		ods tagsets.excelxp options(orientations = 'Landscape' Fitpage='yes' pages_fitwidth ='1'
					pages_fitheight='100' foreground = 'whilte' font_size = "xx-small" sheet_name= "Raw_binwise_infoval_Num");
				Title 'Binwise Information value based on raw binning for Numeric Variable';
			%end;
			proc print data = NUM_Raw_binwise noobs;  run ;
			%if &excel. =1 %then %do; ods html close; %end;
			proc delete data = NUM_Raw_binwise; run;
		%end;
  %end;

	%if &excel. =1 %then %do;  ods html file = "&output.overall_infoval.xls"; %end;
	%if &excel. =2 %then %do;
		ods tagsets.excelxp options(orientations = 'Landscape' Fitpage='yes' pages_fitwidth ='1'
			pages_fitheight='100' foreground = 'whilte' font_size = "xx-small" sheet_name= "overall_infoval");
	%end;	Title 'Overall Information value based on Raw and Monotonic binning';
	 		proc print data = Raw_Overall noobs;  run ;
	%if &excel. =1 %then %do;	ods html close;				%end;
	%if &excel. =2 %then %do;	ods tagsets.excelxp close;	%end;

	run;
	proc delete data = &indata._ ;run;

/**********************************************************************************************/ 
%MEND WOE ;
/**********************************************************************************************/
/**********************************************************************************************/
/* Test data */
Libname local "Y:\HK_US_PX_01\data\Abhishek\temp\To Amit";

options mprint mlogic symbolgen;


%let output = Y:\HK_US_PX_01\data\Abhishek\temp\To Amit\;

/*%let output = E:\AMIT\TVS\;*/
libname local  "&output.";

/*proc contents data = local.c_ivprep_3;run;*/

/*%let keep_var =	 */
/*cb_ivpr__OFFUS_PYMT_BAL_L1M_0*/
/*cb_ivpr__OS_AVG_AUM_L3M_PCT_0*/
/*cb_ivpr__OS_AVG_INC_L3M_PCT_0*/
/*;*/

/*ods html body = 'cont.xls';*/
/*proc contents data = local.ttd_dev; run;*/
/*ods html close;*/

data val;
set val;
/*	where ci_samp_flag = 'oot'  ;*/
	wgt=1;
/*	keep cb_ivpr_bad wgt &keep_var.;*/
run;

/********************************************************************************/
/* Call Macro */	
%let libin		=work			;	/* libname of input data i.e work/anything*/
%let indata		=val			;	/* input dataset name	*/
%let dv			=cb_ivpr_bad	;	/* dependend (good/bad) variable name, 1 should be bad on concern thing */
%let target		=1				;	/* what to be modeled either 1 or 0, model for good or bad */
%let binproc	=Rank   		;	/* method to create a raw bin either Rank or data. Choose anyone*/
%let miss		=.				;	/* deault letter missing value assigned to missing value = .*/
%let weight		=wgt			;	/* weight variable (leave blank if not applicable)*/
%let group		=10				;	/* Max number of split in a variable */
%let modlabel	=HK				;	/* Project name */
%let Raw_bin	=1				;	/* 1: Information value based on raw binning*/
%let CA_CODE	=1				;	/* 1:SAS code for Characteristic Analysis*/
%let out_data	=dev			;	/* output dataset which will have WOE and CA variables*/
%let Lib_output	=local			;	/* Save overall infoval dataset. it will be used in logistic iteratation code*/
%let excel		=2				;	/*1=ods(output in multiple excel file), 2=tagset(output in single excel file)*/
%let keeplst	=&keep_var.		;	/* Numeric variable list	*/ 
/********************************************************************************/

proc printto log="Y:\HK_US_PX_01\data\Abhishek\temp\To Amit\autobin.txt";
%WOE() ;
proc printto;run;

/********************************************************************************/


/*	%let _SC = %substr(%sysfunc(reverse(&HLF.)),1,1);*/
/*	%if %bquote(&_SC) = \ %then %do; 	%let filepath = &HLF&HLN  	;%end;*/
/*	%else %do;							%let filepath = &HLF.\&HLN  ;%end;*/
