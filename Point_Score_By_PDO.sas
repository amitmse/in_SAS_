
Libname ddata 'Y:\SL_CC_A_01\data\New_data';

/*
data ddata.model_score;
	set ddata.alt_data_v2;
			odds= 	-0.8872
					+sumlim_fcrd_ttrad_t*-0.0126
					+cdlq_12m_fusec_ttrad_cap*0.3619
					+count_6m_eeval_cap*0.53
					+b_custcat_good*-0.9389
					+util_fcrd_ttrad*0.8833
					+mob_max_ttrad_t_cap*-0.7118
				;
			score= round(-((odds + log(100))*30/log(2)) + 500,1);
			pa=1/(1+exp(-odds));

			where b_mob_max_ttrad ne 1;
			dev_flag=(yyyymm>=2016 );
	Run;
*/

data    Point_Score;
      set ddata.model_score      ;

		PDO			= 30;
		ODDs		= 100;
		Base_Score	= 500;

		Factor 		= PDO/log(2);
		Offset 		= Base_score - round(Factor*log(odds),1);

		/*Point Score by Variable*/
		p_Intercept					= round(-0.8872*-Factor,1)							;		
		p_sumlim_fcrd_ttrad_t		= round(sumlim_fcrd_ttrad_t*-0.0126*-Factor,1)		;
		p_cdlq_12m_fusec_ttrad_cap	= round(cdlq_12m_fusec_ttrad_cap*0.3619*-Factor,1)	;
		p_count_6m_eeval_cap		= round(count_6m_eeval_cap*0.53*-Factor,1)			;
		p_b_custcat_good			= round(b_custcat_good*-0.9389*-Factor,1)			;
		p_util_fcrd_ttrad			= round(util_fcrd_ttrad*0.8833*-Factor,1)			;
		p_mob_max_ttrad_t_cap		= round(mob_max_ttrad_t_cap*-0.7118*-Factor,1)		;
		
		p_score = round(sum(	Offset,						p_Intercept,			
								p_sumlim_fcrd_ttrad_t,		p_cdlq_12m_fusec_ttrad_cap,	
								p_count_6m_eeval_cap,		p_b_custcat_good,	
								p_util_fcrd_ttrad,			p_mob_max_ttrad_t_cap
							),1);

		/*Drop PDO ODDs Base_Score	Factor Offset ;*/

		diff = score-p_score;
      run;

/*
	proc freq data = Point_Score;
		tables  p_sumlim_fcrd_ttrad_t*sumlim_fcrd_ttrad_t_scr    /missing norow nocol nopercent nocum list;
	run;


	proc print data = Point_Score (obs=10);
		var score p_score;
	run;
*/

proc freq data = Point_Score noprint;
	tables  
/*			p_sumlim_fcrd_ttrad_t*sumlim_fcrd_ttrad_t				*/
/*			p_cdlq_12m_fusec_ttrad_cap*cdlq_12m_fusec_ttrad_cap   	*/
/*			p_count_6m_eeval_cap*count_6m_eeval_cap					*/
/*			p_b_custcat_good*b_custcat_good							*/
/*			p_util_fcrd_ttrad*util_fcrd_ttrad						*/
/*			p_mob_max_ttrad_t_cap*mob_max_ttrad_t_cap				*/

			/missing norow nocol nopercent nocum list out=test;
	run;


%let point_score	= 
						p_sumlim_fcrd_ttrad_t
/*						p_cdlq_12m_fusec_ttrad_cap*/
/*						p_count_6m_eeval_cap*/
/*						p_b_custcat_good*/

/*						p_util_fcrd_ttrad*/
/*						p_mob_max_ttrad_t_cap*/
					;

%let var 			= 
						sumlim_fcrd_notrans
/*						cdlq_12m_fusec_ttrad*/
/*						count_6m_eeval*/
/*						b_custcat_good*/

/*						util_fcrd_ttrad*/
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

		PDO			= 30;
		ODDs		= 100;
		Base_Score	= 500;

		Factor 		= PDO/log(2);
		Offset 		= Base_score - round(Factor*log(odds),1);

		sumlim_fcrd_ttrad		= log(limit+1);

		if sumlim_fcrd_ttrad >0 then 	sumlim_fcrd_ttrad_t = sumlim_fcrd_ttrad**1.74433468734655; else 
										sumlim_fcrd_ttrad_t = 0;
		p_sumlim_fcrd_ttrad_t	= round(sumlim_fcrd_ttrad_t*-0.0126*-Factor,1);

run;


Proc sql;
	select p_sumlim_fcrd_ttrad_t, min(limit) as min, max(limit) as max 
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
		p_util_fcrd_ttrad		= round(p_util*0.8833*-Factor,1)			;

run;


Proc sql;
	select p_util_fcrd_ttrad, min(p_util) as min, max(p_util) as max 
	from test
	group by 1
	order by 1;
	quit;
