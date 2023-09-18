*CHANGE WORKING DIRECTORY
cd "/Users/jeremyspater/Dropbox/duke/political economy core/weber/replication_files/ReplicationFiles_ProtestantEthic_JS-IT_CPS/replication_datafiles/census"

insheet using "rdd_data_STATA.csv"

*net install rdrobust, from(https://sites.google.com/site/rdpackages/rdrobust/stata) replace

*NOTE
*Bias-corrected estimates are accessed using "ereturn list" after calling rdrobust
*e(tau_bc): bias-corrected estimate
*e(se_tau_rb): robust standard error

*table 3
*column 1
rdrobust growth dist, fuzzy(pctcath) 
ereturn list 
*column 2
rdrobust growth dist, fuzzy(pctcath) covs(elev rough) 
ereturn list 
*column 3
rdrobust growth dist, fuzzy(pctcath) covs(elev rough lon lat) 
ereturn list 
*column 4
rdrobust growth dist, fuzzy(pctcath) covs(elev rough lon lat pop1860) 
ereturn list 

