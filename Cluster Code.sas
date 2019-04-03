
/*
	Cluster Solution Guidelines 
	Number of clusters 	4 to 15 
	Maximum cluster size 	Less than 35% 
	Minimum cluster size 	More than 3% 
	Maximum RMS STD deviation 	Less than 1.4 
	Max distance seed to observation 	Less than 100 
	Max distance seed to observation 	(Max dist – Min dist)/(Min dist) < 5 
	Max distance b/w cluster centroids 	More than 1.4 
	Minimum variable R-Square 	More than 0.25 
	Minimum overall R-Sqaure 	More than 0.5 
	Approximate exp overall R-square 	(Appr exp R-Sq – Overall R-Sq) < 0.2 
*/


libname in "C:\Business\Trainings\Clustering\Data";

/****Creating development and validation datasets****/

data in.dev in.val;
set in.cluster_data;
if ranuni(7777) le 0.65 then output in.dev;
else output in.val;
run;
/*NOTE: There were 25733 observations read from the data set IN.CLUSTER_DATA.*/
/*NOTE: The data set IN.DEV has 16588 observations and 49 variables.*/
/*NOTE: The data set IN.VAL has 9145 observations and 49 variables.*/

/*NOTE - All the subsequent development work will now be done only on in.dev dataset. Validation dataset will be called upon
only at the time of validation****/

/****Univariate tests to check for outliers****/


proc univariate data=in.dev plot nextrobs=10;
var x1-x48;
run;

/****Outlier Treatment****/

data in.dev_out;
set in.dev;
x1=min(max(x1,1),206);
x4=min(max(x4,0),45);
x5=min(max(x5,0),5603800);
x6=min(max(x6,0),57450);
x7=min(max(x7,0),49899); 
x8=min(max(x8,0),102550); 
x9=min(max(x9,0),118882); 
x10=min(max(x10,0),1179310);
x11=min(max(x11,0),1165430);
x18=min(max(x18,0),16);
x19=min(max(x19,0),476);
x20=min(max(x20,0),5377797);
x21=min(max(x21,0),4859655);
x22=min(max(x22,100128),46996628);
x28=min(max(x28,0),6478135);
x34=min(max(x34,2),2000000);
x37=min(max(x37,0),4068766);
x38=min(max(x38,0),7431696);
x44=min(max(x44,-7631.45),179258);
x48=min(max(x48,0),4803680);
run;
/*NOTE: There were 16588 observations read from the data set IN.DEV.*/
/*NOTE: The data set IN.DEV_OUT has 16588 observations and 49 variables.*/

proc univariate data=in.dev_out plot nextrobs=10;
var x1 x4 x5 x6 x7 x8 x9 x10 x11 x18 x19 x20 x21 x22 x28 x34 x37 x38 x44 x48;
run;

/****Factor analysis****/

proc factor data = in.dev_out method=prin nfact=32 rotate=varimax out=fact_1;
var
x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x18 x19 x20 x21 x22 x28 x29 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x48;
run;

ods html body="C:\Business\Trainings\Clustering\Data\Factor\factor.xls";
proc factor data = in.dev_out method=prin nfact=21 rotate=varimax out=fact_fin;
var
x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x18 x19 x20 x21 x22 x28 x29 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x48;
run;
ods html close;

/***Variables identified from Factor analysis are:
x1 x2 x3 x6 x8 x11 x19 x20 x29 x34 x35 x36 x37 x38 x39 x40 x41 x44 x45 x46 x48 ****/

proc standard data = in.dev_out mean=0 std=1 out=in.devcluster;
var  
x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x18 x19 x20 x21 x22 x28 x29 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x48;
run;
/*NOTE: The data set IN.DEVCLUSTER has 16588 observations and 49 variables.*/

/****Cluster development****/

proc fastclus data =in.devcluster maxc=250 maxiter=100 delete=500 out=in.devsol_1;
var 
/*x1 */
x2 
/*x3 */
/*x6 */
/*x8 */
/*x11*/
x19
x20
/*x29*/
/*x34*/
/*x35*/
/*x36*/
/*x37*/
/*x38*/
/*x39*/
/*x40*/
/*x41*/
/*x44*/
x45
x46
/*x48*/
;
run;

/***Validation of cluster solution option 1****/

data val;
set in.val;
/*x1=min(max(x1,1),206);*/
/*x4=min(max(x4,0),45);*/
/*x5=min(max(x5,0),5603800);*/
/*x6=min(max(x6,0),57450);*/
/*x7=min(max(x7,0),49899); */
/*x8=min(max(x8,0),102550); */
/*x9=min(max(x9,0),118882); */
/*x10=min(max(x10,0),1179310);*/
/*x11=min(max(x11,0),1165430);*/
/*x18=min(max(x18,0),16);*/
x19=min(max(x19,0),476);
x20=min(max(x20,0),5377797);
/*x21=min(max(x21,0),4859655);*/
/*x22=min(max(x22,100128),46996628);*/
/*x28=min(max(x28,0),6478135);*/
/*x34=min(max(x34,2),2000000);*/
/*x37=min(max(x37,0),4068766);*/
/*x38=min(max(x38,0),7431696);*/
/*x44=min(max(x44,-7631.45),179258);*/
/*x48=min(max(x48,0),4803680);*/
run;
/*NOTE: There were 9145 observations read from the data set IN.VAL.*/
/*NOTE: The data set WORK.VAL has 9145 observations and 49 variables.*/

proc standard data = val mean=0 std=1 out=valcluster;
var  
x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x18 x19 x20 x21 x22 x28 x29 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x48;
run;
/*NOTE: The data set WORK.VALCLUSTER has 9145 observations and 49 variables.*/

data seed;
input x2 x19 x20 x45 x46;
cards;
 1.094851201     -0.176633466      -0.149899229      -0.870364751      -0.194286891
-0.688850592     -0.171770460      -0.002980813      -0.797800195      -0.390046232
-0.132431813     -0.266903788       3.589641735      -0.556184402      -0.192688165
-0.748542809     -0.241610850      -0.142300394       0.955772836      -0.393934673
 0.119090579     -0.272188826      -0.232151560      -0.155351232       1.777657260
 0.377382639      2.521733784      -0.449239668       0.109546174       0.026236283
 1.035058213     -0.244430456      -0.181036141       0.905930681      -0.406451909
;
run;
proc print data=seed;run;

proc fastclus data=valcluster out=val_result maxc=7 maxiter=0 seed=seed;
var x2 x19 x20 x45 x46;
run;






/****USING FASTCLUS PROCEDURE TO ADDRESS MULTICOLLINEARITY****/


/*Enter the initial set of variables to be used for clustering process*/

options compress=yes;

%let varlist = 
x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x18 x19 x20 x21 x22 x28 x29 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x48;

ods output RSquare = varlist_summary;
proc varclus data = in.dev_out
maxeigen = 0.95 /* default value is 1. We can change it to 0.7 or 0.9 depending upon the number of variable clusters 
we want to create*/
/*outstat = Output_data */
/* output data provides the correlation amongst the variables in the varlist*/
Short;
/* short commands just shortens the output and provides the relevant information*/
var &varlist.;
run;






/****Factor Analysis - Tests ****/

proc factor data = in.dev_out method=prin nfact=8 rotate=varimax out=fact_1;
var
x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x18 x19 x20 x21 x22 x28 x29 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x48;
run;



proc factor data=in.dev_out method=p rotate=v priors=one plot n=8 out=pc1sub reorder;
var 
x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x18 x19 x20 x21 x22 x28 x29 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x48;
run;








