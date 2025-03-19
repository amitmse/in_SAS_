

/*************************************************************************/
/*************************************************************************/

%Macro profile();
		proc sort data = &indata.(keep = &score. &varlist.) out=test;
			by &score.;
			where &score. ne .;
			run;

		proc sql noprint; select  count(*)	into:Total_Obs  from test;quit;
		%put &Total_Obs.;

		data test1 ;		
			set test ;
				rank_var+1;		
				r=rank_var/&Total_Obs.;
				length quantile $32;
				if r <= 0.2 then quantile = "Lowest_score_00_to_20_pct"; else 
				if r <= 0.4 then quantile = "Score_20_to_40_pct"; else 
				if r <= 0.6 then quantile = "Score_40_to_60_pct"; else 
				if r <= 0.8 then quantile = "Score_60_to_80_pct"; else 
				if r <= 1   then quantile = "Highest_score_80_to_100_pct";

				/*keep &score. &varlist. rank_var r quantile;*/
			run ;

		proc means data = test1 noprint;
			class quantile;
			var &varlist.;
			output out=test2 (drop=_TYPE_ _FREQ_ ) mean=;
			run;

		data test2;
			set test2;
				if quantile  = ' ' then quantile = 'Overall';
			run;

		proc transpose data =test2 out= test3 ; id quantile;run;
		data test3;
			retain  _NAME_	_LABEL_ Lowest_score_00_to_20_pct Score_20_to_40_pct Score_40_to_60_pct Score_60_to_80_pct Highest_score_80_to_100_pct;
			set test3;				
				label _NAME_					= "Variable";
				label _LABEL_					= "Variable Description";
				label Lowest_score_00_to_20_pct 		= "Lowest score range: 00% to 20%";
				label Score_20_to_40_pct			= "Medium score range: 20% to 40%";
				label Score_40_to_60_pct			= "Medium score range: 40% to 60%";
				label Score_60_to_80_pct			= "Medium score range: 60% to 80%";
				label Highest_score_80_to_100_pct 		= "Highest score range: 80% to 100%";
				rename _NAME_ = Variable _LABEL_ = Label;
			run;

		proc export data	= test3
			outfile	= "&output_path\profile.csv"
			dbms	= csv
			replace label ;
		run;
		/*ods html file = "&output_path\profile.xls";	proc print data = test3 noobs LABEL;  run ;		ods html close;*/
		proc delete data = test1 test2 test3;run;

%Mend profile;

/*************************************************************************/
/*************************************************************************/
libname a 'C:\XXX_01\temp';

%let score 		= bscore;
%let varlist 		= var_1 var_2;
%let indata		= scored_dev;
%let output_path	= C:\XXX_01\temp;
/*************************************************************************/
/*************************************************************************/
%profile();
