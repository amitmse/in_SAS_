



/* TTD model: Scaled Score by PDO */
%macro PDO_Score(indata);

/*%let pdo=200;*/
/*%let base_score=600;*/
/*%let odds=50;*/

data &indata._scored;
set  &indata.;

/* Below coefficient is for GOOD not for bad*/
/* Score */
/**/;
	LOG_ODD = (3.317277053)+ PRODUCT_TYPE_WOE*(2.730093946)+ INSURANCE_TYPE1_WOE*(1.761195018)+ 
			STATE_WOE*(0.17235483)+ APPLICANT_AGE_WOE*(1.882743603)+ LTV_WOE*(1.230591863)+ EMI_WOE*(1.371595761);

/* Scale score by PDO */
/*	score 	= offset + factor*ln(odds);*/
/*	score 	=   offset - ((woe*ß)+ a)*factor;*/
/*	http://stats.stackexchange.com/questions/10092/what-does-20-ln2-mean-in-logistic-regression*/
/*	offset	= 	600; Typically in credit scoring one would choose a baseline score e.g. 600*/
/*	Offset = 600 – {28.8539 ln (50)} = 487.123;   */
/*	http://bankmodel.blogspot.in/2012/09/a-common-pitfall-of-pdo-points-double.html*/
/*	Factor 	= 	20/ln(2) = 28.8539 ; 20 is PDO, if u increase PDO u will have higher no. of split in score */
/*	interpretation of the 20/ln(2) is that for a 20-point increase in score, the odds double.*/;
	

	Factor 					= 	200/log(2);
	Offset 					= 	600-Factor*log(60);
/* 	PDO Scale Score */
	PDO_Score 				= 	Offset + Factor*(LOG_ODD);

/* Point Score calculation for each variable*/

	PRODUCT_TYPE_chk		= PRODUCT_TYPE_WOE*(2.730093946);

	Intercept				= Factor*(3.317277053);
	PRODUCT_TYPE_Score		= Factor*PRODUCT_TYPE_WOE*(2.730093946);
	INSURANCE_TYPE1_Score 	= Factor*INSURANCE_TYPE1_WOE*(1.761195018);
	STATE_Score 			= Factor*STATE_WOE*(0.17235483);
	APPLICANT_AGE_Score 	= Factor*APPLICANT_AGE_WOE*(1.882743603);
	LTV_Score 				= Factor*LTV_WOE*(1.230591863);
	EMI_Score 				= Factor*EMI_WOE*(1.371595761);


/* Adjusted Pont Score: No negative point score */;

		Intercept_score = 742;

		iF  PRODUCT_TYPE =   "MC" 														then PRODUCT_TYPE_Score 	= 0;
ELSE 	IF  PRODUCT_TYPE in ("MO", "SC")												then PRODUCT_TYPE_Score 	= 214;

		if INSURANCE_TYPE in ('DOWN PAYMENT', '')										then INSURANCE_TYPE1_Score 	= 4;
else 	if INSURANCE_TYPE in ('WITH EMI')												then INSURANCE_TYPE1_Score 	= 0;
else 	if INSURANCE_TYPE in ('NOT APPLICABLE', 'PDC')									then INSURANCE_TYPE1_Score 	= 217;

		iF  STATE in ('WUP','JHARKHAND','PUNJAB','MP','RJ')  							then STATE_Score 			= 0; 
ELSE 	IF  STATE IN ('KOLKATA','DELHI','GJ','MH II','EUP','ORISSA','HARYANA','MH I')   then STATE_Score 			= 99; 
ELSE 																						 STATE_Score 			= 0;

		iF  		APPLICANT_AGE <= 28  												then APPLICANT_AGE_Score 	= 0;
ELSE 	IF  28 <= 	APPLICANT_AGE <= 36   												then APPLICANT_AGE_Score 	= 8;
ELSE 	IF  		APPLICANT_AGE >  36   												then APPLICANT_AGE_Score 	= 84;

	 	iF  LTV <= 68.52  																then LTV_Score 				= 71;
ELSE 	IF  LTV >  68.52  																then LTV_Score 				= 0;

 	 	iF  EMI <= 1745  																then EMI_Score 				= 64;
ELSE 	IF  EMI >  1745   																then EMI_Score				= 0;

Offset = -581;
PDO_Score_test = 
			Offset + 
			Intercept_score + 
			PRODUCT_TYPE_Score +
			INSURANCE_TYPE1_Score +
			STATE_Score +
			APPLICANT_AGE_Score +
			LTV_Score +
			EMI_Score
;

/* Check the calculation of PDO score by point score and overall level */
/*	PDO_Score_test = Offset + (Intercept 	+ PRODUCT_TYPE_Score 	+ INSURANCE_TYPE1_Score + STATE_Score + */
/*									 		+ APPLICANT_AGE_Score 	+ LTV_Score 			+ EMI_Score);*/
	test_1					= PDO_Score - PDO_Score_test;

run;

Title "&indata.";
/*Proc sql; select max(offset) as max_off, min(offset) as min_off, min(test_1) as min_test, max(test_1) as max_test */
/*from &indata._scored; quit;*/
%KS(in_data=&indata._scored,bad=GBI_Tag,score=PDO_Score, bin_ks=1, weight=wgt, no_bin=10);
/*proc freq data = &indata._scored;*/
/*tables*/
/*test_1*/
/*Factor*/
/*Offset*/
/**/
/*PRODUCT_TYPE_chk */
/*PRODUCT_TYPE_Score		*/
/*INSURANCE_TYPE1_Score 	*/
/*STATE_Score 			*/
/*APPLICANT_AGE_Score 	*/
/*LTV_Score 				*/
/*EMI_Score 				*/
/*;*/
/*run;*/


%mend PDO_Score;

ods html body = "test.xls";
%PDO_Score(dev);
%PDO_Score(val);
%PDO_Score(oot1);
%PDO_Score(oot2);
ods html close;

/**/
/*data development;*/
/*set dev val ;*/
/*run;*/
/**/
/*%include 'F:\AMIT\TVS\Work\Program\KS_Macro_with_Weight.sas';*/
/*ods html body = "KS_PDO.xls";*/
/*%KS(in_data=development,bad=GBI_Tag,score=PDO_Score, bin_ks=1, weight=wgt, no_bin=10);*/
/*ods html close;*/
