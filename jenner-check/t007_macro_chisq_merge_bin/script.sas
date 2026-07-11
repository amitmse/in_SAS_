/**********************************************************************************************************/;
/****** Chi-Squaree: Dependence Assessment *****************************************************************/;
/****** http://www.lexjansen.com/nesug/nesug07/sa/sa10.pdf			***************************/;
/****** https://ctspedia.org/wiki/pub/CTSpedia/StatToolsTopic052/Chisquare.sas	***************************/;
/**********************************************************************************************************/;
/* Adapted from Chi-Square Macro.sas (amitmse/in_SAS_) for the Jenner compatibility    */
/* bundle: the %chisq / %chi macros are byte-identical to the source. The source read  */
/* "test.ttd_dev" from an external libname (C:\Users\Lenovo\Downloads\fwdproject\test, */
/* not available here), so this bundle supplies a small mock dataset with the same     */
/* column shape the macro expects (LTV + a 0/1 GBI_TAG dependent variable) in place of */
/* the libname read - the macro logic and its call are unmodified.                     */

Options mprint mlogic symbolgen;

%let EPS = 1.0e-30;
%let X = LTV;
%let Y = GBI_TAG;
%let DONE = 0;

/* Mock data standing in for "test.ttd_dev" (an external libname not available here) -
   same column shape the macro reads: a continuous LTV variable and a 0/1 GBI_TAG
   dependent variable. */
data ttd_dev;
	input LTV GBI_TAG;
	datalines;
	42 1
	55 1
	61 0
	70 0
	48 1
	65 0
	38 1
	72 0
	58 1
	80 0
	45 1
	68 0
	52 1
	75 0
	40 1
	;
run;

data XYZ;
set ttd_dev;
	keep &x. &y.;
run;

proc rank 	data 	= XYZ 	groups	= 10 	out 	= XYZ;
			var 	  &x ;
			ranks 	  rank_var ;
run ;

%macro chisq(n11,n12, n21,n22, chisq);
	&chisq = 	(&n11+&n12+&n21+&n22)*(&n11*&n22 - &n12+&n21)**2
				/(&n11+&n12+&EPS)/ (&n21+&n22+&EPS)/(&n11+&n21+&EPS)/(&n12+&n22+&EPS);
%mend chisq;


%MACRO chi(indata=);
	%let DONE = 0;

		data abc;
		set &indata.;
			&x._c	=	rank_var ;
			r&x		=	&x.*1;
		run;

		proc summary data = abc missing nway min max mean sum;
			class &x._c ;
			var r&x. &y ;
			output out= _tmp mean(r&x.)=xmean min(r&x.)=xmin max(r&x.)=xmax N=N sum(&y.)=y1sum;
		run;

		data _tmp;
		set _tmp;
			n		=_freq_;
			y0sum	=n-y1sum;
		run;

		%do %while (&DONE = 0) ;
				data _tmp (drop = pre_y1sum pre_y0sum _TYPE_ _FREQ_);
				set _tmp;
					retain pre_y1sum pre_y0sum;
					format pval 32.4;
					if _n_ = 1 then pval=0;
					else do ;
						%chisq(n11=pre_y1sum,n12=pre_y0sum, n21=y1sum,n22=y0sum, chisq=QP);
						pval = 1- probchi(QP,1);
					end;
					pre_y1sum=y1sum;
					pre_y0sum=y0sum;
				run;

				data _tmp ;
				set _tmp end= eof;
						retain loc 0;
						retain pval_max 0;
					if pval_max < pval then do;
						loc=_n_;
						pval_max=pval;
					end;

					if eof then do;
						if pval_max < 0.05 then call symput("DONE",1);
					end;
					DROP LOC pval_max;
				run;

		%end;
%MEND;



%chi(indata=XYZ);

proc print data=_tmp noobs; run;
