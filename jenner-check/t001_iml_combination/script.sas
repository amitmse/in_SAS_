/***********************************************************************************/
/* Create combination of variables                                                 */
/* Adapted from Combination.sas (amitmse/in_SAS_) for the Jenner compatibility     */
/* bundle: this is the PROC IML block byte-for-byte from the source file. The      */
/* second half of the original script (PROC IMPORT of a local CSV +                */
/* %LM_Iterate macro loop over its rows) needed a real input file to exist on      */
/* disk, so it isn't included here - everything below runs standalone.            */
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
	quit;
