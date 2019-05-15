
%Macro recovery();
		data base_file;
			set master_file ;
				if '01-dec-2013'd <= co_month <= '31-dec-2019'd;
				keep account_number charge_off_flag charge_off_amount;
			run;

		proc sql; 
			create table recovery as 
				select account_number, file_month, recovery_amount
				from good 
				where account_number in (select distinct account_number from base_file);
			quit;

		data recovery;
			set recovery;
				format recovery_start_date date9.;
				recovery_start_date= '01-dec-2013'd;
				month_dif = intnx('month', recovery_start_date, file_month);
			run;

		/*Check for duplicate */
		proc sort data = recovery nodupkey ; by account_number month_dif; run; quit;

		proc transpose data=recovery out=recovery_transpose (drop=_LABEL_ _NAME_ ) prefix=recovery_amount_ ;
			by account_number;
			var recovery_amount; 
			id month_dif; run;

		proc export data	= recovery
			   		outfile	= "recovery.csv"
			   		dbms	= csv
			   		replace;
			run;
%Mend;
