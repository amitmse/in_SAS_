
Libname ddata 'C:\test_SC_CS';

/*
data ddata.model_score;
	set ddata.alt_data_v2;
			odds= 	-0.8872
					+var_1*-0.0126
					+var_2*0.3619
					+var_3*0.53
					+var_4*-0.9389
					+var_5*0.8833
					+var_6*-0.7118
				;
			score= round(-((odds + log(100))*30/log(2)) + 500,1);
			pa=1/(1+exp(-odds));

			where var_9 ne 1;
			dev_flag=(yyyymm>=2016 );
	Run;
*/

data    Point_Score;
      set ddata.model_score      ;

		PDO		= 30;
		ODDs		= 100;
		Base_Score	= 500;

		Factor 		= PDO/log(2);
		Offset 		= Base_score - round(Factor*log(odds),1);

		/*Point Score by Variable*/
		p_Intercept	= round(-0.8872*-Factor,1)			;		
		p_var_1		= round(var_1*-0.0126*-Factor,1)		;
		p_var_2		= round(var_2*0.3619*-Factor,1)			;
		p_var_3		= round(var_3*0.53*-Factor,1)			;
		p_var_4		= round(var_4*-0.9389*-Factor,1)		;
		p_var_5		= round(var_5*0.8833*-Factor,1)			;
		p_var_6		= round(var_6*-0.7118*-Factor,1)		;
		
		p_score = round(sum(	Offset,	p_Intercept, p_var_1, p_var_2, p_var_3, p_var_4, p_var_5, p_var_6),1);

		/*Drop PDO ODDs Base_Score	Factor Offset ;*/

		diff = score-p_score;
      run;

/*
	proc freq data = Point_Score;
		tables  p_var_1*var_1_scr    /missing norow nocol nopercent nocum list;
	run;


	proc print data = Point_Score (obs=10);
		var score p_score;
	run;
*/

proc freq data = Point_Score noprint;
	tables  
/*			p_var_1*var_1		*/
/*			p_var_2*var_2   	*/
/*			p_var_3*var_3		*/
/*			p_var_4*var_4		*/
/*			p_var_5*var_5		*/
/*			p_var_6*var_6		*/

			/missing norow nocol nopercent nocum list out=test;
	run;


%let point_score	= 
						p_var_1
/*						p_var_2*/
/*						p_var_3*/
/*						p_var_4*/

/*						p_var_5*/
/*						p_var_6*/
					;

%let var 			= 
						sumlim_fcrd_notrans
/*						cdlq_12m_fusec_ttrad*/
/*						count_6m_eeval*/
/*						var_4*/

/*						var_5*/
/*						mob_max_ttrad*/
					; 

Proc sql;
	select &point_score., min(&var.) as min, max(&var.) as max 
	from Point_Score
	group by 1
	order by 1;
	quit;


/*Total Limit*/
data test;
	do i = 1 to 29994000;
	limit = i;output;
	end;
	drop i;
	run;

data test;
	set test;

		PDO		= 30;
		ODDs		= 100;
		Base_Score	= 500;

		Factor 		= PDO/log(2);
		Offset 		= Base_score - round(Factor*log(odds),1);

		var_10		= log(limit+1);

		if var_10 >0 then 	var_1 = var_10**1.74433468734655; else 
										var_1 = 0;
		p_var_1	= round(var_1*-0.0126*-Factor,1);

run;


Proc sql;
	select p_var_1, min(limit) as min, max(limit) as max 
	from test
	group by 1
	order by 1;
	quit;
/****************************************************************/


/*Util*/
data test;
	do i = 1 to 300;
	util = i;output;
	end;
	drop i;
	run;

data test;
	set test;

		PDO						= 30;
		ODDs					= 100;
		Base_Score				= 500;

		Factor 					= PDO/log(2);
		Offset 					= Base_score - round(Factor*log(odds),1);

		p_util					= util/100;
		p_var_5					= round(p_util*0.8833*-Factor,1)			;

run;


Proc sql;
	select p_var_5, min(p_util) as min, max(p_util) as max 
	from test
	group by 1
	order by 1;
	quit;
