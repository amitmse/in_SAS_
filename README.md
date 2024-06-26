# SAS macro for Logistic (Scorecard) Model Development

1. **Logistic Model**: https://github.com/amitmse/in_SAS_/blob/master/Logistic_Model_Iteration_final.sas
	
 		This one iterates Logistic Regression Model and provide range of model options.
   		Final model is based of following options ( user cut-off ):
	   		- VIF
	   		- Sign of beta
	   		- P-Value
	   		- No. of variables
   
		Along with model it provides following metrics:
			- KS
			- Concordant, Discordant, Pairs, Somers D
			- AUC/C-Stat/Area under curve
			- Correlation with bad and model variable

2. **Computation of Weight of Evidence (WOE) Variable**: https://github.com/amitmse/in_SAS_/blob/master/Automatic_binning_for_numeric_and_character_variables_woe_method.sas

   		Create WOE variables for numeric and character variables with following options 
			- Monotonic WOE binning	
			- Provide SAS code to generate Monotonic WOE binning variables 
   
		https://github.com/amitmse/in_SAS_/blob/master/Woe_calculation_Macro.sas
   		Weight of Evidence Calculation
     
4. **Computation of Information Value and Weight of Evidence Variable**:    

      https://github.com/amitmse/in_SAS_/blob/master/Information_value_Raw_and_Monotonic_binning.sas

   		Compute Information value and Weight of Evidence variables with following options
   			- Information value
			- Raw WOE binning 	
			- Monotonic WOE binning	
			- Provide SAS code to generate Monotonic WOE binning variables 

   		https://github.com/amitmse/in_SAS_/blob/master/Raw_Information_Value.sas

		Compute Information value
   
      		https://github.com/amitmse/in_SAS_/blob/master/Raw_information_value_LP.sas
   
      		https://github.com/amitmse/in_SAS_/blob/master/Information_value_with_Weight.sas
      
4. **Exploratory Data Analysis (EDA)** : https://github.com/amitmse/in_SAS_/blob/master/EDA.sas

   		Provides basic distribution of data i.e., count, missing, unique, sum, mean, STD, percentile
   
      		https://github.com/amitmse/in_SAS_/blob/master/Create%20Historical%20and%20Performance%20Variables.sas
   
      		https://github.com/amitmse/in_SAS_/blob/master/Merge.xlsx
   
      		https://github.com/amitmse/in_SAS_/blob/master/Hash%20Merge.sas
   
      		https://support.sas.com/resources/papers/proceedings/proceedings/sugi26/p103-26.pdf

6. **Lift Table (KS & GINI)**: https://github.com/amitmse/in_SAS_/blob/master/Lift_Table_v1.sas

		Compute KS and GII
   
      		https://github.com/amitmse/in_SAS_/blob/master/KS_Macro_with_Weight.sas
   
      		https://github.com/amitmse/in_SAS_/blob/master/Lift_Table%20-%20Transreg.sas
   
      		https://github.com/amitmse/in_SAS_/blob/master/GINI.sas
   
      		https://github.com/amitmse/in_SAS_/blob/master/GINI.xlsx

7. Marginal KS: https://github.com/amitmse/in_SAS_/blob/master/Marginal_KS_Macro.sas

		Compute Marginal KS to check contribution of a variable in a model.      

8. Characteristic Analysis Macro: https://github.com/amitmse/in_SAS_/blob/master/Characteristic_Analysis_Macro.sas

		 Characteristic Analysis of model variable
    
      		https://github.com/amitmse/in_SAS_/blob/master/Characteristic_Analysis_Macro_LP.sas 

10. Decision Tree: https://github.com/amitmse/in_SAS_/blob/master/Decision%20Tree.sas

      		Decision Tree for segmentation

11. Score scale by Point to Double the Odds (PDO): https://github.com/amitmse/in_SAS_/blob/master/Point_Score_By_PDO_Method.sas

		- Scale score using Point to Double the Odds method.
    		
      		https://github.com/amitmse/in_SAS_/blob/master/Point%20Score%20by%20PDO%20Method.xlsx
    
      		https://github.com/amitmse/in_SAS_/blob/master/PDO%20Score.xlsx
    
      		https://github.com/amitmse/in_SAS_/blob/master/PDO.sas
    
      		https://github.com/amitmse/in_SAS_/blob/master/PDO_Calculation.sas
    
      		https://github.com/amitmse/in_SAS_/blob/master/Scale_Score_by_PDO.sas
      

***************************************************************************************************************
