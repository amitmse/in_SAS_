/**/
proc printto log="C:\Users\Lenovo\Downloads\fwdproject\test\autobin.txt";

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
 
  data 	regout ;
  set 	regout ;
    %do v = 1 %to &Numvar ;
      	%let var 	= %scan(&Num_varlst,&v) ;
      	if &var <= .Z then &var = .&miss ;
    %end ;
  run ;

/************************************************************************************/
/*********NUMERIC VARAIBELS *********************************************************/
/************************************************************************************/



  	%if &Numvar. >0 %then %do;
  		%do i = 1 %to &Numvar. ;    
  			%let var = %upcase(%scan(&Num_varlst,&i)) ;

	    	%if %upcase(&binproc) = RANK %then %do ;
				proc rank data 	= regout group=&group 
						out 	= regout2(keep = obs &var weight &dv dv meandv rank_var);
						var 	&var ;
			        	ranks 	rank_var ;
			    run ;
	    	%end ; 

			%else 	%if %upcase(&binproc) = DATA %then %do ;
				    proc sort data = regout ;
				    	by &var ;
				    run ;

				    data regout2 ;
						if _n_ 	 = 1 		then rank_var = 1 ;
					    if &var <= .Z 		then rank_var = &var ;
					set regout end = last ;
					    lagval = lag(&var) ;
					    wobs + weight ;
					    if ((wobs - weight) ge &size) and (&var. ne lagval) and (&var. > .Z) then do ;
							rank_var + 1 ;
							wobs = weight ;
					    end ;
					    if rank_var <= .Z  then rank_var = &var ;
					    keep obs &var weight dv &dv. meandv rank_var ;
				      run ;

	    	%end ;

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
						min(&var.) as min, 
						max(&var.) as max 
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
					b.min, 
					b.max 
				from test1 a 
				left join  test2 b on a.Trank_var=b.Trank_var
				order by min;
			quit;

			proc sql noprint; select  sum(BAD) 	into:Total_Bad  from test3 ;quit;
			proc sql noprint; select  sum(GOOD) into:Total_Good from test3 ;quit;
				%let TotalPop = %SYSEVALF(&Total_Bad + &Total_Good);
				%let badrate  = %SYSEVALF(&Total_Bad./&TotalPop. );
					%put &TotalPop.;
					%put &Total_Bad;   
					%put &Total_Good;  
					%put &badrate.;

			 data  missing_data nonmissing_data;
			 set  test3;
				 if min <= .Z  and max <= .Z  then output missing_data;
				 else output nonmissing_data;
			 drop Trank_var;
			 run;


				  	data  missing_data;
				  	set   missing_data end=last;
					  	retain g b 0;
						  	g=sum(good,g);
						  	b=sum(bad,b); 
					  	drop good bad;
					  	rename g=good b=bad;
					  	if last=1;
				  	run;

					data test4;
					set missing_data nonmissing_data;
					run;

			data test5;
				retain 	Rank Var min max GOOD Pct_Good BAD Pct_Bad Bad_Rate Total 
						Pct_tot Woe Info_Val KS Histogram_WOE Histogram_BadRate;
			set  test4;

					Length Var $50;
					format Woe Info_Val 12.8; 
					format KS percent10.4	;
					retain Rank cum_pct_bad cum_pct_good 0 			;

					Var 			= "&var."						;
					Rank			= Rank+1;
					Total 			= sum(Good,Bad)					;
					Bad_Rate 		= Bad/Total						;
					Pct_tot			= Total/&TotalPop. 				;

					if Bad  <= .Z   then 	Pct_bad =0; 
					else 					Pct_Bad	= Bad/&Total_Bad;
					if Good <= .Z   then 	Pct_Good=0; 
					else 					Pct_Good= Good/&Total_Good;

					cum_pct_bad 	= sum(cum_pct_bad, Pct_Bad) 		;
					cum_pct_good	= sum(cum_pct_good, Pct_Good) 		;
					KS 				= abs(cum_pct_good - cum_pct_bad)	;

					if Bad  <= .Z   or Good <= .Z  or Bad=0  or Good=0	then 	WoE 		=.; 
					else 														Woe 		=(log(Pct_Good / Pct_Bad));
					if Bad  <= .Z   or Good <= .Z  or Bad=0  or Good=0	then 	Info_Val 	=.; 
					else 														Info_Val 	=(Pct_Good - Pct_Bad)*(Woe);

					lag_Bad_Rate 	= lag(Bad_Rate);
					        if _n_=1 then inc_monotonic =0;
			        else    if lag_Bad_Rate > Bad_Rate then inc_monotonic= 1; 
					else 									inc_monotonic= 99;
			                if _n_=1 then dec_monotonic =0;
			        else    if lag_Bad_Rate < Bad_Rate then dec_monotonic= 1; 
					else 									dec_monotonic= 99;

					Length Histogram_WOE  $20;
						len_woe		=	abs(round(7*WoE,1))-1;
						if len_woe > 14 then len_woe=14;
						if woe >0 then do;
							if len_woe <0 then 	Histogram_WOE = "+";
						else 					Histogram_WOE = "+"||repeat("*",len_woe);
						end;
						else do;
							if len_woe < 0 then Histogram_WOE = "+";
						else 					Histogram_WOE = repeat("*",len_woe)||"+";
						end;

						Length  Histogram_BadRate $120;
							if &badrate. > 0.3 then do;
								len_BadRate	=	abs(round(10*Bad_Rate,1));
							end;
							else do;
								len_BadRate	=	abs(round(100*Bad_Rate,1));
							end;
								if len_BadRate <= 0 then Histogram_BadRate = "+";
							else						 Histogram_BadRate = repeat("*",len_BadRate)||"+";

					drop cum_pct_bad cum_pct_good lag_Bad_Rate len_woe len_BadRate ;

			run;

			proc sql noprint; 
					select 	
						sum(inc_monotonic), 
						sum(dec_monotonic) 
						into :sum_inc_monotonic, :sum_dec_monotonic  
					from test5; 
			Quit;
						%put &sum_inc_monotonic.;
						%put &sum_dec_monotonic.;

			data test6;
	    	set  test5 end=last;

				Length Max_lag_for_min position 8;
				Max_lag_for_min=lag(max);
				if Max_lag_for_min	<= .Z  then Max_lag_for_min=min; 
				else Max_lag_for_min=Max_lag_for_min;

				Max_lag_min_char=STRIP(tranwrd(put(Max_lag_for_min,best32.),'E-','0'));
				Max_char=STRIP(tranwrd(put(max,best32.),'E-','0'));

				Format Break_Risk_Rank 4.;
				if 	(&sum_inc_monotonic. <=&group.) or (&sum_dec_monotonic. <=&group.) then Break_Risk_Rank=0; 
				else 														  				Break_Risk_Rank=1;
						if _N_ = 1 	then 	position=1;
				else 	if last=1 	then 	position=3;
				else 						position=2;

				if min <= .Z  or max  <= .Z  then var_missing=1; else var_missing=0;
				if Bad <= .Z  or Good <= .Z  or Bad=0  or Good=0 then Good_Bad_missing=1; else Good_Bad_missing=0;
				second_line=0;
				drop inc_monotonic dec_monotonic;
	    	run;

			proc sql noprint; select  max(var_missing) 		into:Num_var_missing  	from test6 ;quit;
			proc sql noprint; select  max(Good_Bad_missing) into:Num_Good_Bad_missing 	from test6 ;quit;
					%put	&var. &Num_var_missing.;
					%put 	&var. &Num_Good_Bad_missing.;  

			%if &Num_var_missing. > 0 and &Num_Good_Bad_missing. > 0 %then %do;
				data test6;
				set  test6;
					if _N_ = 2 then second_line=1; else second_line=0;
				run;
	    		proc append base = NUM_var_GB_missing data = test6 force ;	run ;
			%end;
			%else %if &Num_var_missing. > 0 %then %do;
				data test6;
				set  test6;
					if _N_ = 2 then second_line=1; else second_line=0;
				run;
	    		proc append base = Num_var_missing data = test6 force ;	run ;
			%end;
			%else %if &Num_Good_Bad_missing. > 0 %then %do;
	    		proc append base = NUM_GB_missing data = test6 force ;	run ;
			%end;
			%else %do;
				proc append base = NUM_nonmissing data = test6 force ;	run ;
			%end;
			proc delete data = 	transout test test1 test2 test3 test4 test5 test6 missing_data nonmissing_data; run;
	
	/*raw Info val*/
			%if &Raw_bin. = 1 %then %do;
				data raw_test1;
				set regout2;
					_tot_=1;
					if rank_var <= .Z  then rank_var= -1 ;
				run;

				proc summary SUM data = raw_test1 nway;
					class rank_var;
					weight weight;
					var  &var. dv _tot_ ;
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
			%end;
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
						sum(GBI_TAG*wgt) 						as bad,
						(calculated total - calculated bad ) 	as Good,
						(calculated bad)/(calculated total)  	as bad_rate format percent8.2
					from regout 
					group by &var.
					order by bad_rate desc  ;
				quit;

				data low_risk high_risk normal_miss normal_nomiss;
				set _char_var1;
							if &var. 	= '' 	then 	output normal_miss ;
					else 	if good		= 0		then 	output high_risk ;
					else 	if bad		= 0		then 	output low_risk;
					else 								output normal_nomiss;
				run;

				proc sort data = normal_nomiss; by bad_rate;run;

				data normal_nomiss;
				set  normal_nomiss;
					_sq_num_+1;
				run;

				proc sql; select max(_sq_num_) into : high_risk_rank from normal_nomiss;quit; 
					%put &high_risk_rank.;

				data low_risk;
				set  low_risk;
					_sq_num_=1;
				run;

				data high_risk;
				set  high_risk;
					_sq_num_= &high_risk_rank.;
				run;

				data _char_var2;
				set normal_miss low_risk normal_nomiss high_risk;
					if &var. ne '';
				run;

				%if %sysfunc(exist(normal_miss)) 	%then %do; proc delete data = normal_miss ;  	run; %end;
				%if %sysfunc(exist(low_risk)) 		%then %do; proc delete data = low_risk ; 	 	run; %end;
				%if %sysfunc(exist(normal_nomiss)) 	%then %do; proc delete data = normal_nomiss; 	run; %end;
				%if %sysfunc(exist(high_risk)) 		%then %do; proc delete data = high_risk ; 		run; %end;

				proc sql;
					create table _char_var3 as 
					select 
							a.&var,
							a.weight,
							a.&dv,
							a.dv,
							a.meandv,
							b._sq_num_
					from regout a
					left join _char_var2 b on a.&var.=b.&var.;
				quit;


	    	%if %upcase(&binproc) = RANK %then %do ;
				proc rank data 	= _char_var3 group=&group 
						out 	= _char_var4 (keep = &var _sq_num_ weight &dv dv meandv rank_var);
						var 	_sq_num_ ;
			        	ranks 	rank_var ;
			    run ;
	    	%end ; 

			%else 	%if %upcase(&binproc) = DATA %then %do ;
				    proc sort data = _char_var3 ;
				    	by _sq_num_ ;
				    run ;

				    data _char_var4 ;
						if _n_ 	 	 = 1 		then rank_var = 1 ;
					    if _sq_num_ <= .Z 		then rank_var = _sq_num_ ;
					set _char_var3 end = last ;
					    lagval = lag(_sq_num_) ;
					    wobs + weight ;
					    if ((wobs - weight) ge &size) and (_sq_num_ ne lagval) and (_sq_num_ > .Z) then do ;
							rank_var + 1 ;
							wobs = weight ;
					    end ;
					    if rank_var <= .Z  then rank_var = _sq_num_ ;
					    keep  &var _sq_num_ weight dv &dv. meandv rank_var ;
				      run ;

	    	%end ;

		    proc transreg data = _char_var4  noprint;
			      model 	identity(dv) = monotone(rank_var) / additive ;
			      output	out	=	_char_var5 dap ;
			      id 	_sq_num_ ;
			      weight weight ;
		    run ;

			proc sort data= _char_var5 nodupkey out = _char_var6 (keep= _sq_num_ Trank_var);
			by _sq_num_ Trank_var;
			where _sq_num_ ne .;
			run;

			proc sql; 
				create table _char_var7 as 
				select 
					a.*,
					compress(CATS(b.Trank_var, '')) length 10 as Trank_var
					from 		_char_var4 a
					left join 	_char_var6 b on a._sq_num_=b._sq_num_;
			quit;

			proc freq data = _char_var7 noprint;
				tables Trank_var*dv /missing norow nocol nopercent nocum out=_char_freq1;
			weight weight;
			run;

			proc transpose data=_char_freq1 out=_char_freq2 (drop= _NAME_ _LABEL_ Rename =(_0 = Good _1 = Bad));
			    by Trank_var 	;
			    id dv			;
			    var COUNT		;
			run;

			data _char_var7;
			set  _char_var7;
				&var._ = quote(trim(&var.));
			run;

			Proc sql; 
				select count(distinct Trank_var) into: tot_freq 
				from _char_var7 where Trank_var ne '.';
			quit;
			%put &tot_freq.;

			Proc sql; 
				select distinct Trank_var into: tot_freq_val separated by ' ' 
				from _char_var7 where Trank_var ne '.'; 
			Quit;
			%put &tot_freq_val.;

			
			%Macro min_max_char();
				%if %sysfunc(exist(test7)) %then %do; proc delete data = test7 ; run;%end;
				%do v = 1 %to &tot_freq.;
			   		%let freq_val = %scan(&tot_freq_val.,&v.,' ') ; %put &freq_val.;
				   	proc sort data = _char_var7 nodupkey out=_&v. (keep=&var._ Trank_var);
				   		by &var._;
				   		where Trank_var eq "&freq_val.";
					run;
					proc sql; select count(distinct &var._) into: nbr from _&v.;quit; %put &nbr.;
					
					%let last_var = %SYSFUNC(COMPRESS(col&nbr.)); %put &last_var. ;

					proc transpose data=_&v. out=_&v._ (drop=_NAME_);
							    by Trank_var 	;
							    var &var._		;
					run;

					data _&v._	;
					set  _&v._	;
						length MAX MIN $32767 ;
						MAX= catx('', of col1 - &last_var.);
						MIN= MAX;
						keep Trank_var MAX MIN ;
					run;
					Proc append data	= _&v._ 	base=_char_var8 force; quit;
					proc delete data 	= _&v. _&v._ ; run;
				%end ;
				proc sort data = _char_var8 nodupkey; by Trank_var; run;
				proc sql; 
						create table _char_freq3 as 
							select 
								a.*,
								b.MIN,
								b.MAX	
					from 		_char_freq2 a
					left join 	_char_var8  b on a.Trank_var=b.Trank_var
					order by Trank_var
					;
				quit;

			%mend min_max_char;
			%min_max_char();

			proc sql noprint; select  sum(BAD) 	into:Total_Bad  from _char_freq3 ;quit;
			proc sql noprint; select  sum(GOOD) into:Total_Good from _char_freq3 ;quit;
				%let TotalPop = %SYSEVALF(&Total_Bad + &Total_Good);
				%let badrate  = %SYSEVALF(&Total_Bad./&TotalPop. );
					%put &TotalPop.;
					%put &Total_Bad;   
					%put &Total_Good;  
					%put &badrate.;

			data _char_freq4;
				retain 	Rank Var min max GOOD Pct_Good BAD Pct_Bad Bad_Rate Total 
						Pct_tot Woe Info_Val KS Histogram_WOE Histogram_BadRate;
			set  _char_freq3;

					Length Var $50;
					format Woe Info_Val 12.8; 
					format KS percent10.4	;
					retain Rank cum_pct_bad cum_pct_good 0 			;

					Var 			= "&var."						;
					Rank			= Rank+1;
					Total 			= sum(Good,Bad)					;
					Bad_Rate 		= Bad/Total						;
					Pct_tot			= Total/&TotalPop. 				;

					if Bad  <= .Z   then 	Pct_bad =0; 
					else 					Pct_Bad	= Bad/&Total_Bad;
					if Good <= .Z   then 	Pct_Good=0; 
					else 					Pct_Good= Good/&Total_Good;

					cum_pct_bad 	= sum(cum_pct_bad, Pct_Bad) 		;
					cum_pct_good	= sum(cum_pct_good, Pct_Good) 		;
					KS 				= abs(cum_pct_good - cum_pct_bad)	;

					if Bad  <= .Z   or Good <= .Z  or Bad=0  or Good=0	then 	WoE 		=.; 
					else 														Woe 		=(log(Pct_Good / Pct_Bad));
					if Bad  <= .Z   or Good <= .Z  or Bad=0  or Good=0	then 	Info_Val 	=.; 
					else 														Info_Val 	=(Pct_Good - Pct_Bad)*(Woe);

					lag_Bad_Rate 	= lag(Bad_Rate);
					        if _n_=1 then inc_monotonic =0;
			        else    if lag_Bad_Rate > Bad_Rate then inc_monotonic= 1; 
					else 									inc_monotonic= 99;
			                if _n_=1 then dec_monotonic =0;
			        else    if lag_Bad_Rate < Bad_Rate then dec_monotonic= 1; 
					else 									dec_monotonic= 99;

					Length Histogram_WOE  $20;
						len_woe		=	abs(round(7*WoE,1))-1;
						if len_woe > 14 then len_woe=14;
						if woe >0 then do;
							if len_woe <0 then 	Histogram_WOE = "+";
						else 					Histogram_WOE = "+"||repeat("*",len_woe);
						end;
						else do;
							if len_woe < 0 then Histogram_WOE = "+";
						else 					Histogram_WOE = repeat("*",len_woe)||"+";
						end;

						Length  Histogram_BadRate $120;
							if &badrate. > 0.3 then do;
								len_BadRate	=	abs(round(10*Bad_Rate,1));
							end;
							else do;
								len_BadRate	=	abs(round(100*Bad_Rate,1));
							end;
								if len_BadRate <= 0 then Histogram_BadRate = "+";
							else						 Histogram_BadRate = repeat("*",len_BadRate)||"+";

					drop cum_pct_bad cum_pct_good lag_Bad_Rate len_woe len_BadRate ;

			run;

			proc sql noprint; 
					select 	
						sum(inc_monotonic), 
						sum(dec_monotonic) 
						into :sum_inc_monotonic, :sum_dec_monotonic  
					from _char_freq4; 
			Quit;
						%put &sum_inc_monotonic.;
						%put &sum_dec_monotonic.;

			data _char_freq5;
	    	set  _char_freq4 end=last;
				
				Format Break_Risk_Rank 4.;
				if 	(&sum_inc_monotonic. <=&group.) or (&sum_dec_monotonic. <=&group.) then Break_Risk_Rank=0; 
				else 														  				Break_Risk_Rank=1;
				Length position 8;
						if _N_ = 1 	then 	position=1;
				else 	if last=1 	then 	position=3;
				else 						position=2;

				if min = ''  or max  = ''  then var_missing=1; 
				else 							var_missing=0;
				if Bad <= .Z  or Good <= .Z  or Bad=0  or Good=0 then 	Good_Bad_missing=1; 
				else 													Good_Bad_missing=0;
				second_line=0;
				Length Max_lag_for_min position 8;
				Max_lag_for_min=.;
				Max_lag_min_char='';
				Max_char='';

				drop inc_monotonic dec_monotonic;
	    	run;

			proc sql noprint; select  max(var_missing) 		into:Char_var_missing  		from _char_freq5; quit;
			proc sql noprint; select  max(Good_Bad_missing) into:Char_Good_Bad_missing 	from _char_freq5; quit;
					%put	&var. &Char_var_missing.;
					%put 	&var. &Char_Good_Bad_missing.;  

			%if &Char_var_missing. > 0 and &Char_Good_Bad_missing. > 0 %then %do;
				data _char_freq5;
				set  _char_freq5;
					if _N_ = 2 then second_line=1; else second_line=0;
				run;
	    		proc append base = Char_var_GB_missing data = _char_freq5 force ;	run ;				
			%end;
			%else %if &Char_var_missing. > 0 %then %do;
				data _char_freq5;
				set  _char_freq5;
					if _N_ = 2 then second_line=1; else second_line=0;
				run;
	    		proc append base = Char_var_missing data = _char_freq5 force ;	run ;
			%end;
			%else %if &Char_Good_Bad_missing. > 0 %then %do;
	    		proc append base = Char_GB_missing data = _char_freq5 force ;	run ;
			%end;
			%else %do;
				proc append base = Char_nonmissing data = _char_freq5 force ;	run ;
			%end;
			
			/*raw Info val*/
			%if &Raw_bin = 1 %then %do;
				data _char_miss _char_non_miss;
				set _char_var1;
					if &var. = '' then output _char_miss;	else output _char_non_miss;
				run;

				data _char_non_miss;
				set  _char_non_miss;
					if &var. ne '' then do;
						rank_var+1;
					end;
					where &var. ne '';
				run;

				data _char_miss;
				set  _char_miss;
					rank_var=-1;
				run;

				%if %sysfunc(exist(_char_miss)) %then %do; 
					data _char_raw1;
					set _char_non_miss _char_miss ;
						_tot_=1;
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
					proc delete data = _char_non_miss	_char_miss; run;
				%end;
				%else %do;
					data _char_raw1;
					set _char_non_miss;

						_tot_=1;
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
					proc delete data = _char_non_miss ; run;
				%end;
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
				proc delete data = _char_raw1 _char_raw2 ; run;
			%end;
			proc delete data = 	_char_var1 _char_var2 _char_var3 _char_var4 _char_var5 _char_var6 _char_var7 _char_var8 _char_freq1 _char_freq2 _char_freq3 _char_freq4 _char_freq5 ; 
			run;
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

/*		Proc delete data = NUM_Raw_Overall Char_Raw_Overall; run;*/

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
/**************************************************************************/
/********** Auto Binning ******************************************************/

	%macro data_chk(miss_data_ , comm , export);
		%if %sysfunc(exist(&miss_data_.)) %then %do;
			%if &export. = 0 %then %do;
				%if &excel. =1 %then %do; ods html file = "&output.MB_binwise_infoval_&miss_data_..xls";			%end;
				%if &excel. =2 %then %do;
					ods tagsets.excelxp options(orientations = 'Landscape' Fitpage='yes' pages_fitwidth ='1'
					pages_fitheight='100' foreground = 'whilte' font_size = "xx-small" sheet_name= "MB_binwise_&miss_data_._infoval");
				%end;
					Title "Binwise Information value based on Monotonic binning: &comm.";
		  			proc print data = &miss_data_. (drop=Max_lag_for_min Max_lag_min_char Max_char position var_missing Good_Bad_missing second_line) noobs;  
					run ;

					%if &excel. =1 %then %do; 	ods html close; 	%end;
			%End;

			%Else %if &export. = 1 %then %do;
				data expo;
				set	&miss_data_. (drop=Max_lag_for_min Max_lag_min_char Max_char position 
									var_missing Good_Bad_missing second_line);
				run;
				
				Proc export Data = expo 
					outfile = "&output.MB_binwise_infoval_&miss_data_..csv" 
					DBMS=CSV Replace;
				Run;
				Proc delete data = expo; run;
			%end;

					proc sql; 
						create table &miss_data_._ as 
							select 
								distinct var			as var,
								sum(Info_Val) 			as Info_Val,   
								max(ks) 				as KS format percent10.4,
								case
									when sum(Break_Risk_Rank) >0 then 'Yes'
									else 'No'
									end as Risk_rank_break,
								case
									when max(var_missing) = 1 and max(Good_Bad_missing)=1 	then 'Both var and Good/Bad'
									when max(var_missing) = 1 								then 'var missing'
									when max(Good_Bad_missing)=1 							then 'Good/Bad Missing'
									else 'No-Missing'
									end as Missing
							from  &miss_data_. 
							group by var
							order by Info_Val desc; 
	 				quit;
			proc append data = &miss_data_._ base = autobin_Overall  force ;	run ;
			proc delete data = &miss_data_._; run;
		%end;
	%mend data_chk;

	/* Numeric*/
		%data_chk(miss_data_ = NUM_var_GB_missing, 	comm=both missing variable and Good bad ,export=0);
		%data_chk(miss_data_ = NUM_var_missing	,	comm=variable missing					,export=0);
		%data_chk(miss_data_ = NUM_GB_missing	,	comm=Good bad missing					,export=0);
		%data_chk(miss_data_ = NUM_nonmissing	,	comm=No missing							,export=0);
	/* Character*/
		%data_chk(miss_data_ = Char_var_GB_missing, comm=both missing variable and Good bad ,export=1);
		%data_chk(miss_data_ = Char_var_missing	,	comm=variable missing					,export=1);
		%data_chk(miss_data_ = Char_GB_missing	,	comm=Good bad missing					,export=1);
		%data_chk(miss_data_ = Char_nonmissing	,	comm=No missing							,export=1);

/************************************************************************************/
/*********SAS code ******************************************************************/
/************************************************************************************/
		
filename NW0	"&output.MB_woe_varlist.sas";
	  data _null_ ;
	  set autobin_Overall end=last ;
	    file NW0;
		Length varw $50;
			varw	= trim(var)||"W";
		if _n_=1 then do ;
		put '/**WOE variable list----------------------------**/' ;
		put '%let woe_var_list = ' ;
		end;
		put varw;
		if last then do ;
	      put ' ; ' ;
	      put '/**********---------- END OF CODES ----------**********/' ;
	    end ;
		Run;

%MACRO NUM_WOE_CODE( missing_data, woe, var_list );
	%if %sysfunc(exist(&missing_data.)) %then %do;
	  data _null_ ;
	  set &missing_data. end=last ;
	    file &woe. ;
		Length varw $50;
			varw	= trim(var)||"W";
		if _n_=1 then do ;
		  put '/********************************************************************************/';
	      put '/**WEIGHT-OF-EVIDENCE CODES -----------------------------------------------*****/' ;
		  put ' ';
		  put '/**Before using this code, make sure it is correct----------------------------**/' ;
		  put '/**Chech the SAS code for scientific no.(Minimum/Maximum)---------------------**/' ;
		  put '/**Missing value in variable -------------------------------------------------**/' ;
		  put '/**Missing value in good and bad----------------------------------------------**/' ;
		  put '/**WOE will be missing when good or bad not available for a bin---------------**/' ;
		  put '/**Input variable should not be more than 31 character------------------------**/';
		  put '/**update the number from excel output----------------------------------------**/' ;
		  put '/** if you face any problem/issue/challange, plz mail me on amitmse@gmail.com **/';
		  put ' ';
		  put '/********************************************************************************/';
		  put ' ';
	    end ;
		if position in (1) then do;
			if max = . then do;
				put @2 " IF " @20 Max_lag_min_char @70 " <= " @80 var @120 " <= " @130 ".Z " @180 "   THEN " @190 varw @230  "  = " @240 WoE  @260 " ;"	;
			end;
			else do;
				put @2 " IF " @80 var @120 " <= " @130 Max_char @180 "   THEN " @190 varw @230  "  = " @240 WoE  @260 " ;"	;
			end;
		end;

		if second_line = 1 then do;
				put @2 " ELSE IF " @80 var @120 " <= " @130 Max_lag_min_char @180 "   THEN " @190 varw @230  "  = " @240 WoE  @260 " ;"	;
		end;

		if position in (2) then do;
			put @2 "ELSE IF " @20 Max_lag_min_char @70 " <= " @80 var @120 " <= " @130 Max_char @180 "   THEN " @190 varw @230  "  = " @240 WoE  @260 " ;"	;
		end;

		if position in (3) then do;
			put @2 "ELSE IF " @80 var @120 " >= " @130 Max_lag_min_char @180 "   THEN " @190 varw @230  "  = " @240 WoE  @260 " ;"	;
		end;

	    if last then do ;
	      put ' ' ;
	      put '/**********---------- END OF CODES ----------**********/' ;
	    end ;
	  run ;

		proc sql; create table woevar_data as select distinct var from &missing_data.; quit;

	  data _null_ ;
	  set woevar_data end=last ;
			    file &var_list. ;
				Length varw $50;
					varw	= trim(var)||"W";
				if _n_=1 then do ;
				put '/**WOE variable list----------------------------**/' ;
				put '%let ' " NUM_woe_&missing_data." '= ' ;
				end;
				put varw;
				if last then do ;
			      put ' ; ' ;
			      put '/**********---------- END OF VAR LIST ----------**********/' ;
			    end ;
		Run;
		proc delete data = woevar_data; run;
	%end;
%mend NUM_WOE_CODE;

	filename NW1	"&output.MB_Num_woe_code_var_Good_Bad_missing.sas";	
	filename NW2 	"&output.MB_Num_woe_code_var_missing.sas";
	filename NW3	"&output.MB_Num_woe_code_Good_Bad_missing.sas";
	filename NW4	"&output.MB_Num_woe_code_Nomissing.sas";

	filename NL1	"&output.MB_Num_woe_varlist_var_Good_Bad_missing.sas";	
	filename NL2 	"&output.MB_Num_woe_varlist_var_missvar_listing.sas";
	filename NL3	"&output.MB_Num_woe_varlist_Good_Bad_missing.sas";
	filename NL4	"&output.MB_Num_woe_varlist_Nomissing.sas";


	%NUM_WOE_CODE( missing_data=Num_var_GB_missing, woe=NW1, var_list=NL1 );
	%NUM_WOE_CODE( missing_data=Num_var_missing, 	woe=NW2, var_list=NL2 );
	%NUM_WOE_CODE( missing_data=Num_GB_missing, 	woe=NW3, var_list=NL3 );
	%NUM_WOE_CODE( missing_data=Num_nonmissing, 	woe=NW4, var_list=NL4 );

/****************************************************************************************/
	/* Woe for Character variables */
/****************************************************************************************/

	%MACRO CHAR_WOE_CODE(missing_data, woe, var_list );
		%if %sysfunc(exist(&missing_data.)) %then %do;
			data _null_ ;
			set &missing_data.;
				file &woe. ;
				Length varw $50;
				varw	= trim(var)||"W";
				if position in (1) then do;
					if min = '' then do;
						put " IF " Var " IN ( ' ' ) THEN " varw " = " Woe " ; "	;
					end;
					else do;
						put "  IF " Var " IN ( " min ") THEN " varw " = " Woe " ; "	;
					end;
				END;
				else do;
					if min = '' then do;
						put " ELSE IF " Var " IN ( ' ' ) THEN " varw " = " Woe " ; "	;
					end;
					if min ne '' then do;
					put " ELSE IF " Var " IN ( " min ") THEN " varw " = " Woe " ; "	;
					end;
				end;
			run ;

			proc sql; 
			 create table Char_woevar_data as select distinct var from &missing_data.; 
			quit;

			data _null_ ;
			set Char_woevar_data end=last ;
			    file &var_list. ;
				Length varw $50;
				varw	= trim(var)||"W";
				if _n_=1 then do ;
					put '/**CHAR WOE variable list----------------------------**/' ;
					put '%let ' " Char_woe_&missing_data." '= ' ;
				end;
					put varw;
					if last then do ;
				      put ' ; ' ;
				      put '/**********---------- END OF VAR LIST ----------**********/' ;
				    end ;
			Run;
			proc delete data = Char_woevar_data; run;
		%end;
	%MEND CHAR_WOE_CODE;

	filename CW1	"&output.MB_Char_woe_code_var_Good_Bad_missing.sas";	
	filename CW2 	"&output.MB_Char_woe_code_var_missing.sas";
	filename CW3	"&output.MB_Char_woe_code_Good_Bad_missing.sas";
	filename CW4	"&output.MB_Char_woe_code_Nomissing.sas";

	filename CL1	"&output.MB_Char_woe_varlist_var_Good_Bad_missing.sas";	
	filename CL2 	"&output.MB_Char_woe_varlist_var_missvar_listing.sas";
	filename CL3	"&output.MB_Char_woe_varlist_Good_Bad_missing.sas";
	filename CL4	"&output.MB_Char_woe_varlist_Nomissing.sas";

	%CHAR_WOE_CODE( missing_data=Char_var_GB_missing, 	woe=CW1, var_list=CL1 );
	%CHAR_WOE_CODE( missing_data=Char_var_missing, 		woe=CW2, var_list=CL2 );
	%CHAR_WOE_CODE( missing_data=Char_GB_missing, 		woe=CW3, var_list=CL3 );
	%CHAR_WOE_CODE( missing_data=Char_nonmissing, 		woe=CW4, var_list=CL4 );


/*************Characteristic Analysis CODES **************************************************/
%MACRO NUM_CA_CODE( missing_data, CA_code , CA_varlist );
   %if %sysfunc(exist(&missing_data.)) %then %do;
	data _null_; 
	   set &missing_data. end=last ;                                                                                                                          
	   file &CA_code.; 
	   Length varB $50;
		varB	=trim(var)||"B";
		if _n_=1 then do ;
		  put '/********************************************************************************/';
	      put '/**Characteristic Analysis CODES----------------------------------------------**/' ;
		  put ' ';
		  put '/**Before using this code, make sure it is correct----------------------------**/' ;
		  put '/**Chech the SAS code for scientific no.(Minimum/Maximum)---------------------**/' ;
		  put '/**Missing value in variable -------------------------------------------------**/' ;
		  put '/**Missing value in good and bad----------------------------------------------**/' ;
		  put '/**WOE will be missing when good or bad not available for a bin---------------**/' ;
		  put '/**update the number from excel output----------------------------------------**/' ;
		  put '/** if you face any problem/issue/challange, plz mail me on amitmse@gmail.com **/';
		  put ' ';
		  put '/********************************************************************************/';
		  put ' ';
		end ;

		if position in (1) then do;
			if max = . then do;
				put @2 " IF " @20 Max_lag_min_char @70 " <= " @80 var @120 " <= " @130 ".Z " @180 "   THEN " @190 varB @230  "  = " @240 Rank  @260 " ;"	;
			end;
			else do;
				put @2 " IF " @80 var @120 " <= " @130 Max_char @180 "   THEN " @190 varB @230  "  = " @240 Rank  @260 " ;"	;
			end;
		end;
		if second_line = 1 then do;
				put @2 " ELSE IF " @80 var @120 " <= " @130 Max_lag_min_char @180 "   THEN " @190 varB @230  "  = " @240 Rank  @260 " ;"	;
		end;
		if position in (2) then do;
			put @2 "ELSE IF " @20 Max_lag_min_char @70 " <= " @80 var @120 " <= " @130 Max_char @180 "   THEN " @190 varB @230  "  = " @240 Rank  @260 " ;"	;
		end;
		if position in (3) then do;
			put @2 "ELSE IF " @80 var @120 " >= " @130 Max_lag_min_char @180 "   THEN " @190 varB @230  "  = " @240 Rank  @260 " ;"	;
		end;
		if last then do ;
	      put ' ' ;
	      put '/**********---------- END OF CODES ----------**********/' ;
	    end ;
	run;

	proc sql; create table ca_data as select distinct var from &missing_data.; quit;

	data _null_; 
	set ca_data end=last ;                                                                                                                          
		   file &CA_varlist.; 
		   Length varB $50;
			varB	=trim(var)||"B";
			if _n_=1 then do ;
			put '/**CA variable list----------------------------**/' ;
			put '%let ' " NUM_CA_&missing_data." '= ' ;
			end;
			put varB;
			if last then do ;
		      put ' ; ' ;
		      put '/**********---------- END OF CODES ----------**********/' ;
		    end ;
	Run;
	proc delete data = &missing_data. ca_data; run;
 %end;
	
%MEND NUM_CA_CODE;

%if &CA_CODE. = 1 %then %do;
	filename NCA0	"&output.MB_CA_varlist.sas";
	  data _null_ ;
	  set autobin_Overall end=last ;
	    file NCA0;
	   	Length varB $50;
		varB	=trim(var)||"B";
		if _n_=1 then do ;
		put '/**CA variable list----------------------------**/' ;
		put '%let CA_var_list = ' ;
		end;
		put varB;
		if last then do ;
	      put ' ; ' ;
	      put '/**********---------- END OF CODES ----------**********/' ;
	    end ;
		Run;

		filename NCA1	"&output.MB_Num_CA_code_var_Good_Bad_missing.sas";	
		filename NCA2	"&output.MB_Num_CA_code_var_missing.sas";
		filename NCA3	"&output.MB_Num_CA_code_Good_Bad_missing.sas";
		filename NCA4	"&output.MB_Num_CA_code_Nomissing.sas";

		filename NCL1	"&output.MB_Num_CA_Varlist_var_Good_Bad_missing.sas";	
		filename NCL2	"&output.MB_Num_CA_Varlist_var_missing.sas";
		filename NCL3	"&output.MB_Num_CA_Varlist_Good_Bad_missing.sas";
		filename NCL4	"&output.MB_Num_CA_Varlist_Nomissing.sas";

		%NUM_CA_CODE( missing_data=Num_var_GB_missing, 	CA_code=NCA1 ,  CA_varlist=NCL1	);
		%NUM_CA_CODE( missing_data=Num_var_missing, 	CA_code=NCA2 ,	CA_varlist=NCL2	);
		%NUM_CA_CODE( missing_data=Num_GB_missing, 		CA_code=NCA3 ,	CA_varlist=NCL3	);
		%NUM_CA_CODE( missing_data=Num_nonmissing, 		CA_code=NCA4 , 	CA_varlist=NCL4	);
/******************************************************************************************/

	%MACRO CHAR_CA_CODE(missing_data, CA_CODE, CA_var_list );
		%if %sysfunc(exist(&missing_data.)) %then %do;
			data _null_ ;
			set &missing_data.;
				file &CA_CODE. ;
				Length varw $50;
				varw	= trim(var)||"B";
				if position in (1) then do;
					if min = '' then do;
						put " IF " Var " IN ( ' ' ) THEN " varw " = " Rank " ; "	;
					end;
					else do;
						put "  IF " Var " IN ( " min ") THEN " varw " = " Rank " ; "	;
					end;
				END;
				else do;
					if min = '' then do;
						put " ELSE IF " Var " IN ( ' ' ) THEN " varw " = " Rank " ; "	;
					end;
					if min ne '' then do;
					put " ELSE IF " Var " IN ( " min ") THEN " varw " = " Rank " ; "	;
					end;
				end;
			run ;

			proc sql; 
			 create table Char_woevar_data as select distinct var from &missing_data.; 
			quit;

			data _null_ ;
			set Char_woevar_data end=last ;
			    file &CA_var_list. ;
				Length varw $50;
				varw	= trim(var)||"B";
				if _n_=1 then do ;
					put '/**CHAR CA variable list----------------------------**/' ;
					put '%let ' " Char_CA_&missing_data." '= ' ;
				end;
					put varw;
					if last then do ;
				      put ' ; ' ;
				      put '/**********---------- END OF VAR LIST ----------**********/' ;
				    end ;
			Run;
			proc delete data = Char_woevar_data; run;
		%end;
	%MEND CHAR_CA_CODE;

		filename CCA1	"&output.MB_Char_CA_code_var_Good_Bad_missing.sas";	
		filename CCA2 	"&output.MB_Char_CA_code_var_missing.sas";
		filename CCA3	"&output.MB_Char_CA_code_Good_Bad_missing.sas";
		filename CCA4	"&output.MB_Char_CA_code_Nomissing.sas";

		filename CCL1	"&output.MB_Char_CA_varlist_var_Good_Bad_missing.sas";	
		filename CCL2 	"&output.MB_Char_CA_varlist_var_missvar_listing.sas";
		filename CCL3	"&output.MB_Char_CA_varlist_Good_Bad_missing.sas";
		filename CCL4	"&output.MB_Char_CA_varlist_Nomissing.sas";

		%CHAR_CA_CODE( missing_data=Char_var_GB_missing, 	CA_CODE=CCA1, CA_var_list=CCL1 );
		%CHAR_CA_CODE( missing_data=Char_var_missing, 		CA_CODE=CCA2, CA_var_list=CCL2 );
		%CHAR_CA_CODE( missing_data=Char_GB_missing, 		CA_CODE=CCA3, CA_var_list=CCL3 );
		%CHAR_CA_CODE( missing_data=Char_nonmissing, 		CA_CODE=CCA4, CA_var_list=CCL4 );

%end;

/*********************************************************************/

filename DA	"&output.generate_sas_dataset_by_using_WOE_and_CA_code_from_autobinning.sas";	

/*		%let quote = "'";*/
		%let quote=%str(%");

		data _null_ ;
	     file DA;
		put '/**--------------it will generate WOE and CA variables--------------**/' ;
		put 'data	' "&out_data.;" 		;
		put 'set	' "&libin..&indata;"	;
			/* NUM variable: SAS code for WOE   */
		%if %sysfunc(fileexist("&output.MB_Num_woe_code_var_Good_Bad_missing.sas")) %then %do;
			put	'%include	' "&quote." "&output.MB_Num_woe_code_var_Good_Bad_missing.sas" "&quote.;";	
			put	'%include	' "&quote." "&output.MB_Num_woe_varlist_var_Good_Bad_missing.sas" "&quote.;";
		%end;
		%if %sysfunc(fileexist("&output.MB_Num_woe_code_var_missing.sas")) %then %do;
			put	'%include	' "&quote." "&output.MB_Num_woe_code_var_missing.sas" "&quote.;";
			put	'%include	' "&quote." "&output.MB_Num_woe_varlist_var_missing.sas" "&quote.;";
		%end;
		%if %sysfunc(fileexist("&output.MB_Num_woe_code_Good_Bad_missing.sas")) %then %do;
			put	'%include	' "&quote." "&output.MB_Num_woe_code_Good_Bad_missing.sas" "&quote.;";
			put	'%include	' "&quote." "&output.MB_Num_woe_varlist_Good_Bad_missing.sas" "&quote.;";
		%end;
		%if %sysfunc(fileexist("&output.MB_Num_woe_code_Nomissing.sas")) %then %do;
			put	'%include	' "&quote." "&output.MB_Num_woe_code_Nomissing.sas" "&quote.;";
			put	'%include	' "&quote." "&output.MB_Num_woe_varlist_Nomissing.sas" "&quote.;";
		%end;

			/* NUM VAR: SAS code for Characteristic Analysis */
		%if %sysfunc(fileexist("&output.MB_Num_CA_code_var_Good_Bad_missing.sas")) %then %do;
			put	'%include	' "&quote." "&output.MB_Num_CA_code_var_Good_Bad_missing.sas" "&quote.;";	
			put	'%include	' "&quote." "&output.MB_Num_CA_Varlist_var_Good_Bad_missing.sas" "&quote.;";	
		%end;
		%if %sysfunc(fileexist("&output.MB_Num_CA_code_var_missing.sas")) %then %do;
			put	'%include	' "&quote." "&output.MB_Num_CA_code_var_missing.sas" "&quote.;";
			put	'%include	' "&quote." "&output.MB_Num_CA_Varlist_var_missing.sas" "&quote.;";
		%end;
		%if %sysfunc(fileexist("&output.MB_Num_CA_code_Good_Bad_missing.sas")) %then %do;
			put	'%include	' "&quote." "&output.MB_Num_CA_code_Good_Bad_missing.sas" "&quote.;";
			put	'%include	' "&quote." "&output.MB_Num_CA_Varlist_Good_Bad_missing.sas" "&quote.;";
		%end;
		%if %sysfunc(fileexist("&output.MB_Num_CA_code_Nomissing.sas")) %then %do;
			put	'%include	' "&quote." "&output.MB_Num_CA_code_Nomissing.sas" "&quote.;";
			put	'%include	' "&quote." "&output.MB_Num_CA_Varlist_Nomissing.sas" "&quote.;";
		%end;
	/**********************************************************************************/
					/* CHAR variable: SAS code for WOE   */
		%if %sysfunc(fileexist("&output.MB_Char_woe_code_var_Good_Bad_missing.sas")) %then %do;
			put	'%include	' "&quote." "&output.MB_Char_woe_code_var_Good_Bad_missing.sas" "&quote.;";	
			put	'%include	' "&quote." "&output.MB_Char_woe_varlist_var_Good_Bad_missing.sas" "&quote.;";
		%end;
		%if %sysfunc(fileexist("&output.MB_Char_woe_code_var_missing.sas")) %then %do;
			put	'%include	' "&quote." "&output.MB_Char_woe_code_var_missing.sas" "&quote.;";
			put	'%include	' "&quote." "&output.MB_Char_woe_varlist_var_missing.sas" "&quote.;";
		%end;
		%if %sysfunc(fileexist("&output.MB_Char_woe_code_Good_Bad_missing.sas")) %then %do;
			put	'%include	' "&quote." "&output.MB_Char_woe_code_Good_Bad_missing.sas" "&quote.;";
			put	'%include	' "&quote." "&output.MB_Char_woe_varlist_Good_Bad_missing.sas" "&quote.;";
		%end;
		%if %sysfunc(fileexist("&output.MB_Char_woe_code_Nomissing.sas")) %then %do;
			put	'%include	' "&quote." "&output.MB_Char_woe_code_Nomissing.sas" "&quote.;";
			put	'%include	' "&quote." "&output.MB_Char_woe_varlist_Nomissing.sas" "&quote.;";
		%end;

			/* Char VAR: SAS code for Characteristic Analysis */
		%if %sysfunc(fileexist("&output.MB_Char_CA_code_var_Good_Bad_missing.sas")) %then %do;
			put	'%include	' "&quote." "&output.MB_Char_CA_code_var_Good_Bad_missing.sas" "&quote.;";	
			put	'%include	' "&quote." "&output.MB_Char_CA_Varlist_var_Good_Bad_missing.sas" "&quote.;";	
		%end;
		%if %sysfunc(fileexist("&output.MB_Char_CA_code_var_missing.sas")) %then %do;
			put	'%include	' "&quote." "&output.MB_Char_CA_code_var_missing.sas" "&quote.;";
			put	'%include	' "&quote." "&output.MB_Char_CA_Varlist_var_missing.sas" "&quote.;";
		%end;
		%if %sysfunc(fileexist("&output.MB_Char_CA_code_Good_Bad_missing.sas")) %then %do;
			put	'%include	' "&quote." "&output.MB_Char_CA_code_Good_Bad_missing.sas" "&quote.;";
			put	'%include	' "&quote." "&output.MB_Char_CA_Varlist_Good_Bad_missing.sas" "&quote.;";
		%end;
		%if %sysfunc(fileexist("&output.MB_Char_CA_code_Nomissing.sas")) %then %do;
			put	'%include	' "&quote." "&output.MB_Char_CA_code_Nomissing.sas" "&quote.;";
			put	'%include	' "&quote." "&output.MB_Char_CA_Varlist_Nomissing.sas" "&quote.;";
		%end;

	/**********************************************************************************/
		/* keep variable list: woe and ca */
			put	'/*Keep 
								&woe_var_GB_missing.  	&woe_var_missing. 	&woe_GB_missing.  	&woe_nonmissing.
								&CA_var_GB_missing.  	&CA_var_missing. 	&CA_GB_missing.  	&CA_nonmissing. 
				;*/';
		put	'RUN	;';
	run;

/*********************************************************************/
	data test7;
	set &libin..&indata;

	/* NUM VAR : SAS code for WOE variable  */
	%if %sysfunc(fileexist("&output.MB_Num_woe_code_var_Good_Bad_missing.sas")) %then %do;
				%include "&output.MB_Num_woe_code_var_Good_Bad_missing.sas";	
	%end;
	%if %sysfunc(fileexist("&output.MB_Num_woe_code_var_missing.sas")) %then %do;
				%include "&output.MB_Num_woe_code_var_missing.sas";
	%end;
	%if %sysfunc(fileexist("&output.MB_Num_woe_code_Good_Bad_missing.sas")) %then %do;
				%include "&output.MB_Num_woe_code_Good_Bad_missing.sas";
	%end;
	%if %sysfunc(fileexist("&output.MB_Num_woe_code_Nomissing.sas")) %then %do;
				%include "&output.MB_Num_woe_code_Nomissing.sas";
	%end;

	/* NUM VAR: SAS code for Characteristic Analysis */
	%if %sysfunc(fileexist("&output.MB_Num_CA_code_var_Good_Bad_missing.sas")) %then %do;
				%include "&output.MB_Num_CA_code_var_Good_Bad_missing.sas";	
	%end;
	%if %sysfunc(fileexist("&output.MB_Num_CA_code_var_missing.sas")) %then %do;
				%include "&output.MB_Num_CA_code_var_missing.sas";
	%end;
	%if %sysfunc(fileexist("&output.MB_Num_CA_code_Good_Bad_missing.sas")) %then %do;
				%include "&output.MB_Num_CA_code_Good_Bad_missing.sas";
	%end;
	%if %sysfunc(fileexist("&output.MB_Num_CA_code_Nomissing.sas")) %then %do;
				%include "&output.MB_Num_CA_code_Nomissing.sas";
	%end;
	/*********************************************************************************/

	/* CHAR VAR : SAS code for WOE variable  */
	%if %sysfunc(fileexist("&output.MB_Char_woe_code_var_Good_Bad_missing.sas")) %then %do;
				%include "&output.MB_Char_woe_code_var_Good_Bad_missing.sas";	
	%end;
	%if %sysfunc(fileexist("&output.MB_Char_woe_code_var_missing.sas")) %then %do;
				%include "&output.MB_Char_woe_code_var_missing.sas";
	%end;
	%if %sysfunc(fileexist("&output.MB_Char_woe_code_Good_Bad_missing.sas")) %then %do;
				%include "&output.MB_Char_woe_code_Good_Bad_missing.sas";
	%end;
	%if %sysfunc(fileexist("&output.MB_Char_woe_code_Nomissing.sas")) %then %do;
				%include "&output.MB_Char_woe_code_Nomissing.sas";
	%end;

	/* CHAR VAR: SAS code for Characteristic Analysis */
	%if %sysfunc(fileexist("&output.MB_Char_CA_code_var_Good_Bad_missing.sas")) %then %do;
				%include "&output.MB_Char_CA_code_var_Good_Bad_missing.sas";	
	%end;
	%if %sysfunc(fileexist("&output.MB_Char_CA_code_var_missing.sas")) %then %do;
				%include "&output.MB_Char_CA_code_var_missing.sas";
	%end;
	%if %sysfunc(fileexist("&output.MB_Char_CA_code_Good_Bad_missing.sas")) %then %do;
				%include "&output.MB_Char_CA_code_Good_Bad_missing.sas";
	%end;
	%if %sysfunc(fileexist("&output.MB_Char_CA_code_Nomissing.sas")) %then %do;
				%include "&output.MB_Char_CA_code_Nomissing.sas";
	%end;

	/*********************************************************************************/
	/* keep variable list: woe and ca */

	%include "&output.MB_woe_varlist.sas";
	%if %sysfunc(fileexist("&output.MB_CA_varlist.sas")) %then %do;		
				%include "&output.MB_CA_varlist.sas";
				Keep &woe_var_list. &CA_var_list. ;
	%end;
	%else %do;
		Keep &woe_var_list. ;
	%end;
	run;

	proc means data = test7 nmiss noprint; var &woe_var_list.; output out=miss_woe (drop=_TYPE_ _FREQ_ ) 		nmiss= ;run;
	proc transpose data =miss_woe out= miss_woe1 (rename=(_NAME_=var COL1=Missing_WOE_VAR));					run;
	%if &CA_CODE. = 1 %then %do;
		proc means data = test7 noprint nmiss; var &CA_var_list.; output out=miss_ca (drop=_TYPE_ _FREQ_ ) 	nmiss=; run;
		proc transpose data =miss_ca out= miss_ca1 (rename=(_NAME_=var  COL1=Missing_CA_VAR));run;
	%end;

	%if %sysfunc(exist(Raw_Overall))  and (&CA_CODE. =1 ) %then %do;
			proc sql;
					create table Overall as 
						select 
							a.Var,
							b.Info_Val			as Raw_Info_val				,
							a.Info_Val			as Autobin_Info_val			,
							b.KS				as Raw_KS					,
							a.KS				as Autobin_KS				,
							a.Risk_rank_break	as Autobin_Risk_rank_break	,
							a.Missing			as Missing_Raw_var			,
							case when length(a.var)>31 	then 'rename variable' 
									else 'no-need' end as Rename_Autobin_var format $32. ,
							c.Missing_WOE_VAR,
							d.Missing_CA_VAR						
						from autobin_Overall a 
						left join  Raw_Overall 	b on a.Var=b.Var
						left join  miss_woe1 	c on trim(a.Var)||"W" =c.Var 
						left join  miss_ca1  	d on trim(a.Var)||"B" =d.Var
						order by Autobin_Info_val desc;
			quit;
			proc delete data = miss_woe  miss_woe1 Raw_Overall miss_ca  miss_ca1;run;
	%end;

	%else %if %sysfunc(exist(Raw_Overall)) %then %do;
			proc sql;
					create table Overall as 
						select 
							a.Var,
							b.Info_Val			as Raw_Info_val				,
							a.Info_Val			as Autobin_Info_val			,
							b.KS				as Raw_KS					,
							a.KS				as Autobin_KS				,
							a.Risk_rank_break	as Autobin_Risk_rank_break	,
							a.Missing			as Missing_Raw_var			,
							case when length(a.var)>31 	then 'rename variable' 
									else 'no-need' end as Rename_Autobin_var format $32. ,
							c.Missing_WOE_VAR						
						from autobin_Overall a 
						left join  Raw_Overall 	b on a.Var=b.Var
						left join  miss_woe1 	c on trim(a.Var)||"W" =c.Var 
						order by Autobin_Info_val desc;
			quit;
			proc delete data = miss_woe  miss_woe1 Raw_Overall ;run;
	%end;

	%else %if &CA_CODE. = 1 %then %do;
			proc sql;
					create table Overall as 
						select 
							a.Var,
							a.Info_Val			as Autobin_Info_val			,
							a.KS				as Autobin_KS				,
							a.Risk_rank_break	as Autobin_Risk_rank_break	,
							a.Missing			as Missing_Raw_var			,
							case when length(a.var)>31 	then 'rename variable' 
									else 'no-need' end as Rename_Autobin_var format $32. ,
							c.Missing_WOE_VAR,
							d.Missing_CA_VAR						
						from autobin_Overall a 
						left join  miss_woe1 	c on trim(a.Var)||"W" =c.Var 
						left join  miss_ca1  	d on trim(a.Var)||"B" =d.Var
						order by Autobin_Info_val desc;
			quit;
			proc delete data =  miss_woe  miss_woe1 miss_ca  miss_ca1;run;
	%end;

	%else %do;
			proc sql;
					create table Overall as 
						select 
							a.Var,
							a.Info_Val			as Autobin_Info_val			,
							a.KS				as Autobin_KS				,
							a.Risk_rank_break	as Autobin_Risk_rank_break	,
							a.Missing			as Missing_Raw_var			,
							case when length(a.var)>31 	then 'rename variable' 
									else 'no-need' end as Rename_Autobin_var format $32. ,
							c.Missing_WOE_VAR
						from autobin_Overall a 
						left join  miss_woe1 	c on trim(a.Var)||"W" =c.Var
						order by Autobin_Info_val desc;
			quit;
			proc delete data =  miss_woe  miss_woe1;run;
	%end;

	%if &excel. =1 %then %do;  ods html file = "&output.overall_infoval.xls"; %end;
	%if &excel. =2 %then %do;
		ods tagsets.excelxp options(orientations = 'Landscape' Fitpage='yes' pages_fitwidth ='1'
			pages_fitheight='100' foreground = 'whilte' font_size = "xx-small" sheet_name= "overall_infoval");
	%end;	Title 'Overall Information value based on Raw and Monotonic binning';
	 		proc print data = Overall noobs;  run ;
	%if &excel. =1 %then %do;	ods html close;				%end;
	%if &excel. =2 %then %do;	ods tagsets.excelxp close;	%end;

	data &Lib_output..Overall;
	set  Overall;
	run;

  	proc delete data = 	&indata._  regout    test7 autobin_Overall Overall;
	run;

	%if %sysfunc(exist(NUM_Var_gb_missing)) %then %do;	proc delete data =NUM_Var_gb_missing;	run;	%end;
	%if %sysfunc(exist(NUM_var_missing)) 	%then %do;	proc delete data =NUM_var_missing; 		run;	%end;
	%if %sysfunc(exist(Num_GB_missing)) 	%then %do;	proc delete data =Num_GB_missing; 		run;	%end;
	%if %sysfunc(exist(NUM_Nonmissing)) 	%then %do;	proc delete data =NUM_Nonmissing ; 		run; 	%end;

	%if %sysfunc(exist(Char_Var_gb_missing)) %then %do;	proc delete data =Char_Var_gb_missing; 	run;	%end;
	%if %sysfunc(exist(Char_var_missing)) 	%then %do;	proc delete data =Char_var_missing; 	run;	%end;
	%if %sysfunc(exist(Char_GB_missing)) 	%then %do;	proc delete data =Char_GB_missing; 		run;	%end;
	%if %sysfunc(exist(Char_Nonmissing)) 	%then %do;	proc delete data =Char_Nonmissing ; 	run; 	%end;

/**********************************************************************************************/ 
%MEND WOE ;
/**********************************************************************************************/
/**********************************************************************************************/
/* Test data */
/*libname local  'C:\Users\Lenovo\Downloads\fwdproject\test';*/

options mprint mlogic symbolgen;

%let output = C:\Users\Lenovo\Downloads\fwdproject\test\;

/*%let output = E:\AMIT\TVS\;*/
libname local  "&output.";

%let keep_var =	 
ADVEMI
 AMOUNT_FINANACED
 APPLICANT_AGE
 APPLICANT_TYPE
 APPLICATION_STATUS
 AREA
 BANK_ACCOUNT_STATUS
 BRANCH_CODE
 BRANCH_NAME
 CHANNEL
 CITY
 CO_BORROWER
 CREDIT_SHIELD
 CUSTOMER_CATEGORY
 DEALER_CATEGORY
 DEALER_TYPE
 DEVIATION_LEVEL
 DEVIATIONFLAG
 DISTANCE_DEALER_CUSTOMER
 EMI
 EMPLOYMENT_TYPE
 GENDER
 GEO_LIMIT_APP
 GROSS_SAL
 GUARANTOR
 INSURANCE_TYPE
 INVOICE_VALUE
 LTV
 MAIN_DLR_CODE
 NEGATIVE_AREA_FLAG
 NET_SAL
 NO_OF_DEPENDENCE
 NO_OF_YEAR
 OTHER_CHARGES
 PAYMENTTYPE
 PDD_INSURANCE_STATUS
 PDD_INVOICE_STATUS
 PDD_RC_STATUS
 PROCESSING_FEES
 PROD_CODE
 PRODUCT_NAME
 PRODUCT_TYPE
 PRODUCTGROUP
 Prof_Grp
 PROFILE
 PROGRAM_DOCUMENT
 PROGRAM_NAME
 QUALIFICATION
 RATE_OF_INTEREST
 RESIDENCE_STATUS
 RESIDENCE_TYPE
 SOURCE_CODE
 STANDARD_OF_LIVING
 STATE
 TENURE
 TVR_FLAG
 TVR_STATUS
  ZONE

 MODEL
 PINCODE

/* GBI_Tag*/
/*  wgt*/

;
/*ods html body = 'cont.xls';*/
/*proc contents data = local.ttd_dev; run;*/
/*ods html close;*/

data dev;
set local.Ttd_dev;
	wgt=1;
	keep GBI_TAG wgt &keep_var.;
run;

/********************************************************************************/
/* Call Macro */	
%let libin		=work			;	/* libname of input data i.e work/anything*/
%let indata		=dev			;	/* input dataset name	*/
%let dv			=GBI_TAG		;	/* dependend (good/bad) variable name, 1 should be bad on concern thing */
%let target		=1				;	/* what to be modeled either 1 or 0, model for good or bad */
%let binproc	=Rank   		;	/* method to create a raw bin either Rank or data. Choose anyone*/
%let miss		=.				;	/* deault letter missing value assigned to missing value = .*/
%let weight		=wgt			;	/* weight variable (leave blank if not applicable)*/
%let group		=10				;	/* Max number of split in a variable */
%let modlabel	=FGB			;	/* Project name */
%let Raw_bin	=1				;	/* 1: Information value based on raw binning*/
%let CA_CODE	=1				;	/* 1:SAS code for Characteristic Analysis*/
%let out_data	=dev			;	/* output dataset which will have WOE and CA variables*/
%let Lib_output	= local			;	/* Save overall infoval dataset. it will be used in logistic iteratation code*/
%let excel		=2				;	/*1=ods(output in multiple excel file), 2=tagset(output in single excel file)*/
%let keeplst	=&keep_var.		;	/* Numeric variable list	*/ 
/********************************************************************************/
%WOE() ;
/********************************************************************************/
proc printto;run;
