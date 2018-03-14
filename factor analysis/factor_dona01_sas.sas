data dona03; 
infile 'H:\01_self study_flash drive\AAAschulich\aaa_ma\time series\R learning\r_reference\R_exercise
\factor_analysis\donation\dona03_for_sas.csv' dlm=',';

input  donor_age	frequency_status_97nk	income_group	
last_gift_amt	lifetime_avg_gift_amt	lifetime_card_prom	lifetime_gift_amount	lifetime_prom	
median_home_value	median_household_income	months_since_first_gift	months_since_last_gift	
number_prom_12	per_capita_income	recent_avg_card_gift_amt	recent_response_count	wealth_rating;
run;

data dona03;
set dona03;
if _n_>=2;
run;

ods graphics on;
proc factor data=dona03 rotate=varimax outstat=fact_all plots=(scree) method=ml;
title "factor analysis";
run;
ods graphics off;
