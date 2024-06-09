
/**********************************************************************************************************/;
/****** Vintage Analysis **********************************************************************************/;
/**********************************************************************************************************/;

libname share "C:\App Scorecard\SAS data";
libname local   "C:\AMIT\VS";

options obs=max mprint mlogic;

%let keep = DUE_AGMTNO CUTDATE FIRST_EMI_DATE EMI Bucket FUTURE_EMI OVERDUE_EMI DEMAND_EMI EMI_OUTSTANDING FUTURE_PRINC ;

data local.Coll_jan12_jun13;
set	 
local.coll_jan12_may12_complete(keep=&keep.)
local.coll_jun12_nov12_complete(keep=&keep.)
local.coll_dec12_complete(keep=&keep.)
local.coll_jan13_complete(keep=&keep.)
local.coll_feb13_complete(keep=&keep.)
local.coll_mar13_complete(keep=&keep.)
local.coll_apr13_complete(keep=&keep.)
local.coll_may13_complete(keep=&keep.)
local.coll_jun13_complete(keep=&keep.)
;

EMI_NEW 				= EMI;
BUCKET_NEW 				= BUCKET;

	If Bucket_New 	= . and EMI_NEW NE 0     	Then 	Bucket_New_1 = Round(SUM(Overdue_EMI,DEMAND_EMI)/EMI_NEW);
ELSE 	If Bucket_New 	= . and EMI_NEW = 0 		Then 	Bucket_New_1 = 0;
ELSE                                        				Bucket_New_1 = Bucket_New;

POS 				  	= Sum(Future_Princ, Overdue_EMI);	if POS<=0 then POS=0;

Run;

proc means  data = local.Coll_jan12_jun13 n nmiss; var FIRST_EMI_DATE; run;

/* To get unique First EMI date */
proc sort data =  local.Coll_jan12_jun13 (keep = DUE_AGMTNO FIRST_EMI_DATE) out=FIRST_EMI_DATE; 
by DUE_AGMTNO FIRST_EMI_DATE;
run;
/* To get the first EMI from dataset  */
data FIRST_EMI_DATE_NODUP;
set	 FIRST_EMI_DATE;
by DUE_AGMTNO;
if first.DUE_AGMTNO;
run;

proc means  data = FIRST_EMI_DATE_NODUP n nmiss; var FIRST_EMI_DATE; run;

/* Merge collection file with LOS file to tag the ACTIVE Accounts */
Proc Sql;
Create table local.COLL_LOGIN_DATE as 
Select 
		a.DUE_AGMTNO,
		a.Bucket_New_1,
		a.cutdate,
		a.POS,
		b.PRODUCT_TYPE,
		d.DISBURSAL_DATE,
		e.FIRST_EMI_DATE


From 		local.Coll_jan12_jun13 			as a 
Left Join 	local.scorecard_part_1_2_3_date as b on a.DUE_AGMTNO  	= b.AGMT_NO 
Left Join 	share.Los_jan11_jun13 			as d on a.DUE_AGMTNO  	= d.AGMT_NO
Left Join 	FIRST_EMI_DATE_NODUP 			as e on a.DUE_AGMTNO  	= e.DUE_AGMTNO 

where b.APPLICATION_STATUS="ACTIVE" ;

Quit;

/****************************************************************************/;
/* Calculate MOB */
data COLL_MOB;
set  local.COLL_LOGIN_DATE;
MOB_FIRST_EMI		= intck("month",FIRST_EMI_DATE, cutdate)+1; 	
if MOB_FIRST_EMI 	= -1 then MOB_FIRST_EMI=99;
if MOB_FIRST_EMI ne .;
run;


data COLL_DELQ;
set	 COLL_MOB;

/* DPD - CUMULATIVE*/
If Bucket_New_1 = 2 Then DPD_30=1; else DPD_30= 0;
If Bucket_New_1 = 3 Then DPD_60=1; else DPD_60= 0;
If Bucket_New_1 = 4 Then DPD_90=1; else DPD_90= 0;

/*OLD POS - DPD */
If DPD_30=1 Then DPD_30_POS_OLD=POS; else DPD_30_POS_OLD = 0;
If DPD_60=1 Then DPD_60_POS_OLD=POS; else DPD_60_POS_OLD = 0;
If DPD_90=1 Then DPD_90_POS_OLD=POS; else DPD_90_POS_OLD = 0;

/* DPD PLUS - CUMULATIVE*/
If 2 <= Bucket_New_1 <= 6 Then DPD_30PLUS=1; else DPD_30PLUS = 0;
If 3 <= Bucket_New_1 <= 6 Then DPD_60PLUS=1; else DPD_60PLUS = 0;
If 4 <= Bucket_New_1 <= 6 Then DPD_90PLUS=1; else DPD_90PLUS = 0;

/*OLD POS - DPD PLUS*/
If DPD_30PLUS=1 Then DPD_30PLUS_POS_OLD=POS; else DPD_30PLUS_POS_OLD = 0;
If DPD_60PLUS=1 Then DPD_60PLUS_POS_OLD=POS; else DPD_60PLUS_POS_OLD = 0;
If DPD_90PLUS=1 Then DPD_90PLUS_POS_OLD=POS; else DPD_90PLUS_POS_OLD = 0;

run;

proc freq data = COLL_DELQ;
tables DISBURSAL_DATE/missing;
format DISBURSAL_DATE	format monyy7.;
run;


/* POS - Vintage Analysis */
Proc Sql;
Create table VINTAGE_POS  as 
Select 
		 DISBURSAL_DATE	format monyy7.
		,MOB_FIRST_EMI 		as MOB_FIRST_EMI
		,cutdate 		format monyy7.
		,PRODUCT_TYPE
/*		,case when Zone in ('EAST', 'WEST','NORTH') then 'OTHER' else Zone end as zone*/
		,Sum(POS) 							as Total_POS_OLD 	  	Format=Comma20.0
		,Sum(DPD_30_POS_OLD) 				as DPD_30_POS_OLD 		Format=Comma20.0
		,Sum(DPD_60_POS_OLD) 				as DPD_60_POS_OLD 		Format=Comma20.0
		,Sum(DPD_90_POS_OLD) 				as DPD_90_POS_OLD 		Format=Comma20.0
		,Sum(DPD_30PLUS_POS_OLD) 			as DPD_30PLUS_POS_OLD 	Format=Comma20.0
		,Sum(DPD_60PLUS_POS_OLD) 			as DPD_60PLUS_POS_OLD 	Format=Comma20.0
		,Sum(DPD_90PLUS_POS_OLD) 			as DPD_90PLUS_POS_OLD 	Format=Comma20.0

From COLL_DELQ
where DISBURSAL_DATE ne .
Group By 1,2,3,4
;
Quit;

Proc Sql;
Create table VINTAGE_ACCOUNT	as 
Select 
		 DISBURSAL_DATE	format monyy7.
		,MOB_FIRST_EMI 		as MOB_FIRST_EMI
		,cutdate 		format monyy7.
		,PRODUCT_TYPE
/*		,case when Zone in ('EAST', 'WEST','NORTH') then 'OTHER' else Zone end as zone*/
		,Sum(1) 			as Total_ACCOUNT 	Format=Comma20.0
		,Sum(DPD_30) 		as DPD_30 			Format=Comma20.0
		,Sum(DPD_60) 		as DPD_60 			Format=Comma20.0
		,Sum(DPD_90) 		as DPD_90 			Format=Comma20.0
		,Sum(DPD_30PLUS) 	as DPD_30PLUS 		Format=Comma20.0
		,Sum(DPD_60PLUS) 	as DPD_60PLUS 		Format=Comma20.0
		,Sum(DPD_90PLUS) 	as DPD_90PLUS 		Format=Comma20.0

From COLL_DELQ
where DISBURSAL_DATE ne .
Group By 1,2,3,4
;
Quit;

proc freq data = VINTAGE_POS; tables zone; run;

ods html body = 'VINTAGE_POS.xls';
	proc print data = VINTAGE_POS;run;
ods html close;

ods html body = 'VINTAGE_ACCOUNT.xls';
	proc print data = VINTAGE_ACCOUNT;run;
ods html close;


