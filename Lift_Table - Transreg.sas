

%MACRO Lift_Table(	) ;

	options  LINESIZE=MAX;

		%let var = &keeplst.;
	  	data &indata._ ;
	  		set &libin..&indata(keep = &keeplst &dv &weight) end=last;

			    %if &target = 1 %then %do ; dv 		= &dv ; 		%end ; 
				%else %do ; 				dv 		= (&dv = 0) ;	%end ;
			    %if &weight ne %then %do ;  weight 	= &weight ;	    %end ;
			    %else %do ;		      		weight 	= 1 ;		    %end ;

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

	  	proc reg data = &indata._ 	noprint	;
	    	model 	dv = ;
	    	output 	out=regout pred=meandv ;
	    	weight weight ;
	  	run ;

		proc rank data 	= regout group=&group 
					out 	= regout2(keep = obs &var weight &dv dv meandv rank_var);
					var 	&var ;
		        	ranks 	rank_var ;
		run ;

		proc transreg data = regout2  noprint;
		      model 	identity(dv) = monotone(rank_var) / additive ;
		      output	out	=	transout dap ;
		      id 	&var ;
		      weight weight ;
		run ;

		proc freq data = transout noprint;
			tables Trank_var*dv /missing norow nocol nopercent nocum out=test;
		weight weight;
		run;

		proc transpose data=test out=test1 (drop= _NAME_ _LABEL_ Rename =(_0 = Good _1 = Bad));
		    by Trank_var 	;
		    id dv			;
		    var COUNT		;
		run;

		proc sql;
		create table test2 as 
				select 
					Trank_var,  
					min(&var.) as min_score, 
					max(&var.) as max_score 
				from transout 
				group by Trank_var
				order by Trank_var;
		quit;

		proc sql;
		create table test3 as 
					select 
						a.Trank_var,
						a.GOOD,
						a.BAD,
						b.min_score, 
						b.max_score 
					from test1 a 
					left join  test2 b on a.Trank_var=b.Trank_var
					order by min_score;
				quit;

		proc sql noprint; select  sum(BAD) 	into:Total_Bad  from test3 ;quit;
		proc sql noprint; select  sum(GOOD) into:Total_Good from test3 ;quit;

			%let TotalPop = %SYSEVALF(&Total_Bad + &Total_Good);
			%let badrate  = %SYSEVALF(&Total_Bad./&TotalPop. );
				%put &TotalPop.;
				%put &Total_Bad;   
				%put &Total_Good;  
				%put &badrate.;

		data test4 ;
		set  test3;
		 drop Trank_var;
		run;

		data Lift_Table;
			retain 	min_score max_score Total Pct_tot GOOD Pct_Good BAD Pct_Bad Bad_Rate KS  Gini;
		set  test4;

				Length Var $50;
				format KS percent10.4	;
				retain cum_pct_bad cum_pct_good 0 			;
				Total 			= sum(Good,Bad)					;
				Bad_Rate 		= Bad/Total						;
				Pct_tot			= Total/&TotalPop. 				;

				if Bad  <= .Z   then 	Pct_bad =0; 
				else 					Pct_Bad	= Bad/&Total_Bad;
				if Good <= .Z   then 	Pct_Good=0; 
				else 					Pct_Good= Good/&Total_Good;

				cum_pct_bad 		= sum(cum_pct_bad, Pct_Bad) 		;
				cum_pct_good		= sum(cum_pct_good, Pct_Good) 		;
				KS 					= abs(cum_pct_good - cum_pct_bad)	;

				lag_cum_pct_bad		= lag(cum_pct_bad)					;
				Gini				= ((sum(cum_pct_bad,lag_cum_pct_bad))/2)*(Pct_tot);
			
				lag_Bad_Rate 	= lag(Bad_Rate);
				        if _n_=1 then inc_monotonic =0;
		        else    if lag_Bad_Rate > Bad_Rate then inc_monotonic= 1; 
				else 									inc_monotonic= 99;
		                if _n_=1 then dec_monotonic =0;
		        else    if lag_Bad_Rate < Bad_Rate then dec_monotonic= 1; 
				else 									dec_monotonic= 99;

				drop Var cum_pct_bad cum_pct_good lag_Bad_Rate ;

		run;

		proc sql /*noprint*/; 
			create table _summary_	as 
					select 						
						max(KS) 						as KS 	FORMAT=PERCENT7.2	,
						abs((sum(Gini)-(0.5))/(0.5)) 	as Gini												
					from Lift_Table; 
		quit;

		proc delete data = dev_ regout regout2 test test1 test2 test3 test4 transout;run;

/**********************************************************************************************/ 
%MEND Lift_Table ;
/**********************************************************************************************/
/**********************************************************************************************/

options mprint mlogic symbolgen;

%let output = Y:\BW_PL_B_01\data\DEV2\	;

libname local  "&output.";


/*
	proc freq data=local.vca_scored_n    ;
		tables    ci_samp_flag
		 	/nocol norow nopercent missing;
		run;
*/


%let keep_var =	  
	cs_score_BW_PLB_PD_N_scale
	;

/********************************************************************************/
/* Call Macro */	
%let libin		=work			;	/* libname of input data i.e work/anything*/
%let indata		=dev			;	/* input dataset name	*/
%let dv			=cb_achl_bad	;	/* dependend (good/bad) variable name, 1 should be bad on concern thing */
%let target		=1				;	/* what to be modeled either 1 or 0, model for good or bad */
%let weight		=wgt			;	/* weight variable (leave blank if not applicable)*/
%let group		=15				;	/* Max number of split in a variable */
%let keeplst	=&keep_var.		;	/* Numeric variable list	*/ 



data dev;
set local.vca_scored_n;
	where ci_samp_flag = 'oot';
	wgt=1;
	keep cb_achl_bad wgt &keep_var.;
run;

/*
	proc means data = dev n NMISS;
		var &keep_var.;
		run;
*/

/********************************************************************************/

%Lift_Table() ;

/********************************************************************************/

