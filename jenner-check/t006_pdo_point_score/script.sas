/* Adapted from Point_Score_By_PDO_Method.sas (amitmse/in_SAS_) for the Jenner        */
/* compatibility bundle: the "Point Score by Variable" scoring DATA step is byte-     */
/* identical to the source. The source read "ddata.model_score" from an external     */
/* libname (C:\test_SC_CS, not available here), so this bundle supplies a small mock  */
/* model_score dataset with the same column shape the step expects (an odds/log-odds  */
/* value plus the 6 model variables var_1-var_6). The source file's later sections    */
/* (a 30-million-row synthetic PROC SQL sweep over "limit", and a 300-row "util"       */
/* sweep) were exploratory scratch work generating lookup tables and are not included -*/
/* the point-scoring DATA step below is the complete, reusable technique.             */

data model_score;
	input var_1 var_2 var_3 var_4 var_5 var_6 odds;
	datalines;
	12  0.5  3  1  22  0.10  1.35
	45  1.2  1  0  15  0.42  0.62
	8   0.3  5  1  30  0.05  2.10
	60  2.1  2  0  10  0.55  0.41
	25  0.8  4  1  18  0.20  1.05
	;
run;

data    Point_Score;
      set model_score      ;

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

      run;

proc print data=Point_Score noobs;
	var var_1-var_6 odds Factor Offset p_score;
run;
