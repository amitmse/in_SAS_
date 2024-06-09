/**********************************************************************************************************/;
/****** Decision Tree *************************************************************************************/;
/*********************************************************************************************************/;

/**********************************************************************************************************/;
options nocenter macrogen  MFILE nosymbolgen   nomprint  nomlogic  merror serror ls=144 ps=77 nosource2;
/*********************************************************************************************************/;

%Macro _Decision_Tree_ (data=, seed=4321 , maxdepth = , maxbranch =  , minobs = , wt = , ivlist = , codefile = , rulefile= , output=, val_prop=  , dv =);

		data DT;
			set &data.;
				%if &wt. ne %then %do ;  	wgt 	= &wt. 	;%end ;
		    	%else %do ;		      		wgt 	= 1 	;%end ;
			run;
		
		proc hpsplit 	data					=	DT 
						seed					=	&seed. 
						maxdepth				=	&maxdepth. 
						maxbranch				=	&maxbranch. 
						minleafsize				=	&minobs. 		nodes 	;
		  				target 			  			&dv.				;
		   				input  			  			&ivlist.			;
						weight 			  			wgt				;
						grow 			  			CHAID				;
		   				prune 			  			rep				;
		   				output nodestats			=	criterion			;
						code file				=	&codefile.			;
		     			partition fraction					(validate=&val_prop.)		;
		     			rules file					=	&rulefile.			;
			run;

		Data _Score;
			set DT;
				%include  &codefile. /source2;
			run;

		data __Import_Rules;
		 	infile  &rulefile. dsd delimiter ='|' truncover;
		 		input in_column :$400.;
			run;

		Data Begin_Cleaning;
			set __Import_Rules;
				format _A_  $16.;
				F_Del = 0;
				if substr(compress(in_column),1,4)	= 'NODE' 						then 	_A_ 	= in_column	; 
				if compress(in_column) IN ('*------------------------------------------------------------*') 	then 	F_Del 	= 1			;
				if length(in_column) > 15 									then 	do;
					if 				upcase(substr(compress(in_column),1,14)) 	= 'PREDICTEDVALUE' 		then 	F_Del 	= 1		;	else
					if _A_ = . and 	upcase(substr(compress(in_column),1,9)) 	= 'PREDICTED' 	   				then 	_B_ 	= in_column	;
				end;
				if F_Del = 0 then output;
			run;
			
		Data Clean_Step_2;
			set Begin_Cleaning;
				format __Node $16.;
				retain __Node ;
				retain num;
				num = num+1;
				if not missing(_A_) THEN __Node 	= 	_A_ 							; 
				if _B_ ne "" 		then new_name	=	substr(_B_,1,index(_B_, '(' )-1) 			;
				if new_name ne "" 	then new_name_1	=	substr(_B_,1,index(_B_, '=' )-1) 			;
				if new_name ne "" 	then new_name_2	=	compress(substr(new_name,index(new_name, '=' )+1)) 	;
				drop _A_ f_del _B_;
				if __Node ne in_column then output;
			run;

		proc sort data = Clean_Step_2;
			by __Node num ;
			run;

		proc transpose data=Clean_Step_2 out=T_1( drop =_name_ ) ;
			var new_name_2;
			by __Node;
			id  new_name_1;
/*			where new_name_1 ne '';*/
			run; 

		Data Clean_Step_3;
			set Clean_Step_2 (where = (new_name = ""));
				by __Node;
				retain Rules;
				format Rules $1200.;
				if first.__Node then Rules = '';
				Rules=catx(',',Rules,in_column);
				keep __Node Rules;
				if last.__Node then output;
			run;

		Data Rules;
			merge Clean_Step_3 T_1;
				by __Node;
				Node_Num = input(compress(__Node, '1234567890', 'k'),3.);
				drop Predicted_1 Predicted_0;
			run;
		/*Rule Print*/


		proc sql;
			create table 		Bands_wt_1
			as select 			_Node_ 		as DT_Node, 
								/*sum(Outstanding_Balance) as ostd_balance, */
								sum(&dv.) 	as Bad, 
								count(&dv.) as Total, 
								calculated Bad/calculated Total as Bad_Rate format = Percent10.2
			from 				_Score
			group by 			1; 
		quit;

		proc sql;
			create table 	All_Rules
			as select 		t1.*, t2.*
			from 			Rules 		t1
			left join		Bands_wt_1 	t2 on t1.Node_Num = t2.DT_Node;
		 quit;

		proc sort data=All_Rules;
			by descending Bad_Rate;
		run;

		Title1 'Final Nodes';
			proc print data = All_Rules (keep = DT_Node Bad Total Bad_Rate Rules) ; run;
		run;


		proc export data	= All_Rules(keep = DT_Node Bad Total Bad_Rate Rules)
			outfile		= "&Folder_Path.\&output..csv"
			dbms		= csv
			replace;
		run;



		/*		PROC EXPORT DATA= All_Rules(keep = DT_Node Bad Total Bad_Rate Rules) outfile= "&Folder_Path.\&output..xlsx" dbms=xlsx replace;*/
		/*			sheet="DT";*/
		/*		run;*/

		proc delete data = DT Criterion _Score __Import_Rules Begin_Cleaning Clean_Step_2 T_1 Clean_Step_3 Rules Bands_wt_1 All_Rules ; run;

%Mend;

/*********************************************************************************************/
/*********************************************************************************************/
/*********************************************************************************************/

%let Project 		= Col ;
%let Folder_Path 	= C:\Users\1567478\MyData;
%let data 		= Mid;

libname ddata "C:\col\data";
Title "Decision tree for &project.";

/*List of variable*/
%let var_list 		= 	;


/***********************************************************************/

%_Decision_Tree_ ( 	 data		=	mid 									/* Input data */
			,seed		=	6581									/* */
			,maxdepth 	=	10 									/* */
			,maxbranch 	=	2 								 	/* */
			,minobs 	=	3000 								 	/* */
			,wt 		=	 						 			/* */
			,dv		=	bad									/* */
			,ivlist 	=	&var_list.							 	/* */
			,codefile 	=	"&Folder_Path.\Decision_tree.sas" 					/* */
			,rulefile	=	"&Folder_Path.\Decision_tree.txt"					/* */
			,output		=	Final_Decision_tree							/* */
			,val_prop	= 	0.25 									/* */
	    );

/***********************************************************************/

