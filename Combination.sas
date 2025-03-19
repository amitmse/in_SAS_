/***********************************************************************************/
	/* Create combination of variables */
/***********************************************************************************/

Proc iml;
		N 		= 	10;                        /* Total number of variables */          
		k 		= 	10;                        /* Combination. Change one by one*/
		idx 	= 	allcomb(N, k);                    
		Items 	=	{	"CPI"
						"CPI_L1"
						"Household_income"
						"Household_income_L1"
						"Policy_rate"
						"Policy_rate_L1"
						"Real_GDP"
						"Real_GDP_L1"
						"Unemployment"
						"Unemployment_L1" 
					};

		S 		= 	Items[ ,idx];
		S 		= 	shape(S, 0, k);
		print S[r=(char(1:nrow(S))) L="MEV Combination"];
	run;
	quit;


/********************************************************/
proc import datafile	= "C:\Docs\List_Combination.csv"
			out		= List_Combination
			dbms		= csv	replace;
			guessingrows= 3000;
	run;

data List_Combination_MEV;
	set List_Combination_MEV;
		All_Combinations = catx(' ', List_1, List_2, List_3, List_4,	List_5,	List_6,	List_7,	List_8,	List_9,	List_10);
	run;

proc sql noprint ;
	select distinct All_Combinations into : All_Combinations separated by '|' 
	from List_Combination_MEV ; 
	quit ;

%let Total_Combination =&sqlobs ; %put &Total_Combination.;

%Macro LM_Iterate();
 	%do v = 1 %to &Total_Combination.  ;
      	%let list 	= %scan(&All_Combinations,&v.,|) ;
    	%put &v. "---->" &list.;
    %end ;
%Mend;

%LM_Iterate();


