
/********************************************************************/
data sales_cal_weath (drop = date DT);
	/*https://www.lexjansen.com/nesug/nesug07/bb/bb16.pdf*/

		if 0 then set lib1.sales; /* "if 0 then set" is a short-cut way to specify the length of all variables
									rather than listing them individually in LENGTH or ATTRIB statements*/

		if _N_ = 1 then do;	/*Hash tables only need to be loaded once*/
			length Day_of_week 2 Holiday_indicator $8;
			declare hash cal(dataset:'lib1.calendar'); 	/*data "lib1.calendar" alias as "cal" and dump in hash*/

				cal.defineKey('Date');/* Merge key from "cal". Hash requires unique keys. Consequently, data is 
					automatically de-duped when loaded into a hash object (no warnings or errors). */

				/*List of var to be included in final merge table.To include all variables cal.defineData(all:'YES')*/
				cal.defineData('Day_of_week', 'Holiday_indicator'); 

			cal.defineDone();/*completes the initialization of the hash object*/
		end;
	set lib1.sales;
		/* Merge using key "sales_date" from sales data.  Condition "0" means that a match was found*/						
		if cal.find(key:sales_date) = 0	then output; /* This is inner join.
			Left join :
				if cal.find(key:sales_date) = 0 then do; 
							day_of_week = day_of_week;	holiday_indicator = holiday_indicator;	end;
				else do; 	day_of_week = .;			holiday_indicator = .;					
							same as above Call Missing(Day_of_week,Holiday_indicator);			end; 
			Multiple Key: Cal.find(key:sales_date, key:sales_hour);
							if key name same in the both table. cal.find()=0 then output;
			*/
	run;

/***************************************************/;

data country_1;
		input country_id country $ @@;
	datalines;
		11 Denmark 12 Norway 13 Sweden 14 Germany 
	run;

data country_2;
		input country_id country $ @@;
	datalines;
		15 India 16 China 17 Kenya 18 Brazil
	run;

data base;
	set country_1 country_2 ;
		drop country;
	run;

/* Without duplicate*/
	proc sort data = base nodupkey; by country_id; run;

	data Inner_Join;
			if 0 then set base country_1; /* For length*/
			if _N_ = 1 then do;
				/*length country $ 8;*/
				declare hash 	cal(dataset:'country_1');
								cal.definekey('country_id');
								cal.definedata('country'); 
								cal.definedone();
			end;
		set base;
			/*Inner Join */
				if cal.find(key:country_id) = 0	then output;
		run;


	data Left_Join;
			if _N_ = 1 then do;
				length country $ 8;
				declare hash 	cal(dataset:'country_1');
								cal.definekey('country_id');
								cal.definedata('country'); 
								cal.definedone();
			end;
		set base;
			/*Left Join */
				if cal.find(key:country_id) = 0 		then do; 
									country = country;	end;
						else do; 	country = '';  		end;	/*Call Missing(country)*/
		run;
/****************************************************************************************************************/
/* With duplicate hash doesn't work. No duplicate in merge data "country_1" and base data can have duplicate */
/****************************************************************************************************************/
	data dup_base;
		set country_1 country_2 country_1;
			drop country;
		run;

	data Inner_Join;
			if _N_ = 1 then do;
				length country $ 8;
				declare hash 	cal(dataset:'country_1');
								cal.definekey('country_id');
								cal.definedata('country'); 
								cal.definedone();
			end;
		set dup_base;	/*Duplicate data*/
			if cal.find(key:country_id) = 0 		then do; 
									country = country;	end;
						else do; 	country = '';  		end;
		run;

	data country_11;
		set country_1 country_1;
		IF _N_=5 then country="test";
		run;

	data Inner_Join;
			if _N_ = 1 then do;
				length country $ 8;
				declare hash 	cal(dataset:'country_11');	/*Duplicate data*/
								cal.definekey('country_id');
								cal.definedata('country'); 
								cal.definedone();
			end;
		set dup_base;
			if cal.find(key:country_id) = 0 		then do; 
									country = country;	end;
						else do; 	country = '';  		end;
		run;



/***************************************************************************************************/

/*For Memory allocation: https://support.sas.com/resources/papers/proceedings/pdfs/sgf2008/107-2008.pdf*/
/*http://www.sascommunity.org/wiki/Different_ways_of_merging_using_an_example*/

/***************************************************************************************************/
/*https://analytics.ncsu.edu/sesug/2010/FF06.Liu.pdf*/
/* Inner join with a hash table*/
		data trans_hash_patients(drop=rc);
			if 0 then set data.patients(drop=studyid);
			declare hash 	hh_pat(dataset:"data.patients(drop=studyid)");
			 				rc = hh_pat.defineKey("pid");
			 				rc = hh_pat.defineData("dob", "sex", "postcode");
			 				rc = hh_pat.defineDone();
			do until(eof);
				set data.transactions end=eof;
				 	call missing(dob, sex, postcode);
				 	rc	= hh_pat.find();
				 	age = intck("year", dob, visitdate);
				  	if rc=0 then output;
			end;
			stop;
		run;

/*Left outer join with a HASH table*/
		data trans_outJoin_hash_patients(drop=rc);
		 	if 0 then set data.patients(drop=studyid);
		 	declare hash 	hh_pat(dataset:"data.patients(drop=studyid)");
							rc = hh_pat.defineKey("pid");
							rc = hh_pat.defineData("dob", "sex", "postcode");
							rc = hh_pat.defineDone();
			do until(eof);
				set data.transactions end=eof;
				call missing(dob, sex, postcode);
				rc	= hh_pat.find();
				age = intck("year", dob, visitdate);
				output;
		 	end;
		 	stop;
		run; 

/********************************************************************/
data product;
		input product_id product $ @@;
	datalines;
		1001 Table 1002 Door 1003 Chair 1004 Sofa
	run;

data country;
		input country_id country $ @@;
	datalines;
		11 Denmark 12 Norway 13 Sweden 14 Germany 
	run;

data customer;
		input customer_id customer $ @@;
	datalines;
		101 Hans 102 Pia 103 Ole 104 Lis 
	run; 


data orders;
	do customer_id = 101 to 104;
		do country_id = 11 to 14;
			do product_id = 1001 to 1004;
				* Create 1.000.000 rows when doing the test;
				do id = 1 to 10;
					sale = Round(ranuni(0)*1000);
					output;
				end;
			end;
		end;
	end;
	drop id;
run;


/*************************************************/
proc sort data=orders;		by customer_id;run;
proc sort data=customer;	by customer_id;run;

data result;
	merge customer orders;
		by customer_id;
	run;

proc sort data=result;		by country_id;run;
proc sort data=country;		by country_id;run;

data result;
	merge country result;
		by country_id;
	run;

proc sort data=result; 		by product_id;run;
proc sort data=product;		by product_id;run;

data result;
	merge product result;
		by product_id;
	run;
/********************/

data hash_result;
	if _n_=1 then do;
			length product customer country $ 8;
			declare hash 	A(dataset:"country");
							A.definekey("country_id");
							A.definedata("country");
							A.definedone();
			declare hash 	B(dataset:"product");
							B.definekey("product_id");
							B.definedata("product");
							B.definedone();
			declare hash 	C(dataset:"customer");
							C.definekey("customer_id");
							C.definedata("customer");
							C.definedone();
	end;
	set orders;
		rc1=A.find();
		rc2=B.find();
		rc3=C.find();
		if rc1=0 and rc2=0 and rc3=0 then output;
		drop rc:;
run;






data test;
	if _n_=1 then do;
			
			declare hash 	A(dataset:"country");
							A.definekey("country_id");
							A.definedata("country");
							A.definedone();
			length country $ 8;
	end;
	set base;
		rc1=A.find();
		if rc1=0 then output;
		drop rc:;
run;




/************************************************************************/
/*https://support.sas.com/resources/papers/proceedings/proceedings/sugi31/244-31.pdf*/
/************************************************************************/
/*sequential access*/
data work.sequential;
 	set sashelp.class;
 		put _all_;output;
	run; 

/* sequential access: Direct access to specific row */ 
data work.direct;
	 do i=2, 3, 9;
	 	set sashelp.class point=i;
	 	put _all_; output;
	 end;
	 stop;
run;

/* explicit looping */	
data work.sequential;
		do until (eof);
			set sashelp.class end=eof;
			put _all_;
			output;
		end;
	run;


/* simulate an index on variable: age */
data work.class_index;
 	set sashelp.class;
 		row_id=_n_;
 		keep age row_id;
	run;

proc sort data=work.class_index;
 	by age row_id;
run;

data work.class_index;
	 	keep age rid;
	 	retain age rid;
	 	length rid $20;
	 set work.class_index;
	 	by age;
	 	if first.age then 	rid = trim(put(row_id,best.-L));else
	 						rid = trim(rid) || ',' ||trim(put(row_id,best.-L));
	 	if last.age then output;
	run;

data work.class (index=(age));
 set sashelp.class;
run; 


data work.sortedclass;
 set work.class;
 by age;
run;

/* direct access */
data work.direct;
	do age=13,14;
		 do until (eof);
			 set class key=age end=eof;
			 if _IORC_=0 then do; /* 0 indicates a match was found */
				 put _all_; output;
			 end;
			 else _ERROR_=0; /* if no match, reset the error flag and continue */
		 end;
	end;
	stop;
	run; 


%let large_obs = 500000;
data 	work.small ( keep = keyvar small: )
 		work.large ( keep = keyvar large: );
	 array keys(1:500000) $1 _temporary_;
	 length keyvar 8;
	 array smallvar [20]; 	retain smallvar 12;
	 array largevar [682]; 	retain largevar 55;
	 do _i_ = 1 to &large_obs ;
		 keyvar = ceil (ranuni(1) * &large_obs);
		 if keys(keyvar) = ' ' then do;
			 output large;
			 if ranuni(1) < 1/5 then output small;
			 keys(keyvar) = 'X';
		 end;
	 end;
run; 


/*MERGE WITH AN INDEX:
	As stated earlier, using an index can eliminate the need for sorting and that usually speeds things up. 
	Let us find out:*/

/* creating indexes */
proc datasets lib=work nolist;
 	modify small; index create keyvar;
 	modify large; index create keyvar;
	quit;

/* merge with indexes (no sorting) */
data work.match_merge_index;
 	merge 	work.large (in=a)
 			work.small (in=b);
 		by keyvar;
 		if a;
	run;

/* merge with memory table (no sorting or indexing required!) */
data work.hash_merge (drop=rc i);
 	/* Create it */
	 declare hash h_small ();/* Define it */
		 length keyvar smallvar1-smallvar20 8;
		 array smallvar(20);
		 rc = h_small.DefineKey(“keyvar” );
		 rc = h_small.DefineData(“smallvar1”,”smallvar2”,”smallvar3”,”smallvar4”,“smallvar17”,”smallvar18”);
		 rc = h_small.DefineDone ();
	 /* Fill it */
	 do until ( eof_small );
		 set work.small end = eof_small;
		 rc = h_small.add ();
	 end;
	 /* Merge it */
	 do until ( eof_large );
	 set work.large end = eof_large;
		 /* this loop initializes variables before merging from h_small */
		 do i=lbound(smallvar) to hbound(smallvar);
		 smallvar(i) = .;
	 end;
	 rc = h_small.find ();
	 output;
	 end;
run; 
