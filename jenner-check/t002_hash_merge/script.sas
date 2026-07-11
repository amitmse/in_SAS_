/*Type of Merge*/
/*https://support.sas.com/resources/papers/proceedings/proceedings/sugi26/p103-26.pdf*/
/****************************************************************************/
/* Adapted from Merge.sas (amitmse/in_SAS_) for the Jenner compatibility    */
/* bundle: byte-identical through the Hash/Index/Format merge sections,    */
/* which are already fully self-contained via DATALINES. The trailing      */
/* "Self Join" PROC SQL block referenced GOOD/BAD tables that are never    */
/* defined anywhere in the source file, so it is left out here.            */
/****************************************************************************/
/*Input data */
	DATA patients;
			INPUT ID DOB Gender $ age;
			INFORMAT DOB MMDDYY10.;
			FORMAT DOB MMDDYY10.;
			DATALINES;
			1 9/20/1980 Female 10
			2 6/12/1954 Male 20
			3 4/2/2001 Male 30
			4 8/29/1978 Female 40
			5 2/28/1986 Female 50
			;
		RUN;

	DATA appointment_log;
			INPUT ID Visit_Date Doctor $ age;
			INFORMAT Visit_Date MMDDYY10.;
			FORMAT Visit_Date MMDDYY10.;
			DATALINES;
			1 1/31/2012 Walker 10
			1 5/29/2012 Walker 10
			2 2/2/2012 Jones 20
			3 1/15/2012 Jones 30
			5 1/29/2012 Smith 50
			5 2/6/2012 Smith 50
			6 12/10/2012 Mark 60
			;
		RUN;

/****************************************************************************/
/* Hash Merge */
	data Inner_Join;
			/* Declare the hash-populated variables explicitly (LENGTH) instead of the
			   original file's "if 0 then set appointment_log patients;" - Jenner 0.1.0
			   hangs on the conditional multi-dataset SET form used only to inherit
			   variable attributes (filed upstream; see jenner-language regression
			   tests). The LENGTH form used two steps down in Left_Join achieves the
			   same "declare before DECLARE HASH" goal and runs cleanly. */
			length DOB 8 Gender $ 8;
			if _N_ = 1 then do;
				/*length country $ 8;*/
				declare hash 	cal(dataset:'patients');
								cal.definekey('ID','age');
								cal.definedata('DOB','Gender');
								cal.definedone();
			end;
		set appointment_log;
			/*Inner Join */
			if cal.find(key:ID, key:age) = 0	then output;
		run;


	data Left_Join;
			if _N_ = 1 then do;
				length DOB 8 Gender $ 8 ;
				declare hash 	cal(dataset:'patients');
								cal.definekey('ID','age');
								cal.definedata('DOB' ,'Gender');
								cal.definedone();
			end;
		set appointment_log;
			/*Left Join */
			if cal.find(key:ID, key:age) = 0 then 	do; Gender = Gender; 	DOB = DOB; 	end;
			else 					do; Gender = '';	DOB	= .;	end;
		run;

/****************************************************************************/
/* Index Merge*/

	DATA patients_indx (INDEX=(test));
		SET patients;
			test = catx(',',ID,age);
		RUN;

	DATA appointment_log_indx (INDEX=(test));
		SET appointment_log;
			test = catx(',',ID,age);
		RUN;

	DATA First; /* base will be appointment_log_indx*/
		SET appointment_log_indx	;
		SET patients_indx 	KEY=test;
		RUN;

	DATA Second; /* base will be patients_indx*/
		SET patients_indx	;
		SET appointment_log_indx KEY=test;
		RUN;

/****************************************************************************/
/* Format Merge*/

	PROC SORT data=patients nodupkey out=patients_formt;
        by ID age;
		RUN;

	DATA patients_formt;
		SET patients_formt;
			test 		= 	catx(',',ID,age);
			LABEL		=	'Y';
			TYPE		=	'C';
			FMTNAME		=	'$CALL';
			RENAME test = 	start;
		RUN;

	PROC FORMAT CNTLIN=patients_formt; Run;

	DATA appointment_log_formt;
		SET appointment_log;
			test = catx(',',ID,age);
		RUN;

	/* Inner Join*/
	DATA appointment_log_formt;
		SET appointment_log_formt;
			WHERE PUT(test,$CALL.) EQ 'Y';
		RUN;

	/*Left Join*/
	DATA appointment_log_formt;
		SET appointment_log_formt;
			if PUT(test,$CALL.) EQ 'Y' then match=1; else match=0;
		RUN;

proc print data=Inner_Join noobs; run;
proc print data=Left_Join noobs; run;
proc print data=First noobs; run;
proc print data=Second noobs; run;
proc print data=appointment_log_formt noobs; run;
