
/**********************************************************************************************************/;
/****** Chi-Squaree ***************************************************************************************/;
/****** http://www.lexjansen.com/nesug/nesug07/sa/sa10.pdf			***************************/;
/****** https://ctspedia.org/wiki/pub/CTSpedia/StatToolsTopic052/Chisquare.sas	***************************/;
/**********************************************************************************************************/;

Options mprint mlogic symbolgen;



Libname test 'C:\Users\Lenovo\Downloads\fwdproject\test';


%let EPS = 1.0e-30;
%let X = LTV;
%let Y = GBI_TAG;
%let DONE = 0;

data XYZ;
set test.ttd_dev;
keep &x. &y.;
run;

proc rank 	data 	= XYZ 	group	= 10 	out 	= XYZ;
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


