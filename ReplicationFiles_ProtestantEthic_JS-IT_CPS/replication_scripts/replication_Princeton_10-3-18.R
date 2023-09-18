#CHANGE WORKING DIRECTORY
setwd('/Users/jeremyspater/Dropbox/duke/political economy core/weber/replication_files/ReplicationFiles_ProtestantEthic_JS-IT_CPS')

#Princeton data
#Agriculture employment (A3), education (A4), birthrate (A6), infant mortality (A7), overall mortality (A8)

rm(list=ls())
Num = function(x){as.numeric(as.character(x))}
s = function(x){summary(factor(x))}
library(plyr);library(dplyr, warn.conflicts = F)

BorderDistricts = c('Broye',
                    'Glane',
                    'Gruyere',
                    'Sarine',
                    'See',
                    'Veveyse',
                    'Avenches',
                    'Echallens',
                    'Grandson',
                    'Lavaux',
                    'Moudon',
                    'Oron',
                    'Payerne',
                    'Paysd\'enhaut',
                    'Vevey', 
                    'Yverdon')

############################################################
#Cleaning and pre-processing
############################################################

#1870 demographic data
A = read.csv('./replication_datafiles/princeton/sw1870_read.csv',
             na.strings='', stringsAsFactors=F)  #181     

A = A %>% filter(District1 %in% BorderDistricts) #16

#1888 demographic data
B = read.csv('./replication_datafiles/princeton/sw1888_read.csv',
             na.strings='', stringsAsFactors=F)  #182     

B = B %>% filter(District1 %in% BorderDistricts) #16

#1910 demographic data
C = read.csv('./replication_datafiles/princeton/sw1910_read.csv',
             na.strings='', stringsAsFactors=F)  #187  

C = C %>% filter(District1 %in% BorderDistricts) #16

#District populations
D = read.csv('./replication_datafiles/princeton/CensusPopulationsByDistrict_7-19-18.csv',
             na.strings='', stringsAsFactors=F) 

#Merge district population with demographic data
A_merge = merge(A, D[,c('Canton','District', 'Population.1870')], by.x = 'District1', by.y = 'District') #1870
B_merge = merge(B, D[,c('Canton','District', 'Population.1888')], by.x = 'District1', by.y = 'District') #1888
C_merge = merge(C, D[,c('Canton','District', 'Population.1910')], by.x = 'District1', by.y = 'District') #1910

####################
#1870
A_merge$ED3_pct = A_merge$ED3 / 1000
lm_ed1 = lm('ED3_pct ~ Canton.x', data = A_merge, weights = A_merge$Population.1870)

#Infant mortality 1870; goes into Table A7
infmort_1870 = (lm('MORT ~ Canton.x', data = A_merge, weights = A_merge$Population.1870)) #Vaud lower by 30 per thousand, significant at 5%

#IF 1870; result for paper
#Note, divide these by 1000 because of scaling in original data set (see codebook)
if_1870 = (lm('IF/1000 ~ Canton.x', data = A_merge, weights = A_merge$Population.1870)) #Vaud actually a little higher; not significant

####################
#1888
B_merge$USCHOOL_pct = B_merge$USCHOOL / 1000
#result for paper
lm_ed2 = lm('USCHOOL_pct ~ Canton.x', data = B_merge, weights = B_merge$Population.1888)

#Infant mortality 1888; result for paper
infmort_1888 = (lm('INFMORT ~ Canton.x', data = B_merge, weights = B_merge$Population.1888)) #Vaud lower by 19 per thousand, significant at 5%

#IF 1888; result for paper
#Note, divide these by 1000 because of scaling in original data set (see codebook)
if_1888 = (lm('IF/1000 ~ Canton.x', data = B_merge, weights = B_merge$Population.1888)) #Vaud a little lower; not significant


####################
#1910
#Infant mortality; result for paper
infmort_1910 = (lm('MORT ~ Canton.x', data = C_merge, weights = C_merge$Population.1910)) #Vaud lower by 45 per thousand, significant at 1%

#IF 1910; result for paper
#Note, divide these by 1000 because of scaling in original data set (see codebook)
if_1910 = (lm('IF/1000 ~ Canton.x', data = C_merge, weights = C_merge$Population.1910)) #Vaud lower; significant at 1%

C_merge$ED3_pct = C_merge$ED3 / 1000
#result for paper
lm_ed3 = lm('ED3_pct ~ Canton.x', data = C_merge, weights = C_merge$Population.1910)


############################################################
#Tables
############################################################

############################################################
#use TAGRIC to manually create Table A3
#Agricultural employment rate (result for paper)
summary(lm('TAGRIC/10 ~ Canton.x', data = A_merge, weights = A_merge$Population.1870)) #p ~ 0.5

#Latex for Table A3
# \begin{table}
# \centering
# \begin{tabular}{ |c|c|c|c| }
# \hlineB{2}
# \multicolumn{2}{|c|}{Agricultural Employment Rates, Fribourg vs. Vaud} \\
# \hlineB{2}
# & 1870 \\
# \hline
# Vaud Coefficient   & -5.4 \\
# (Std. Error) &  (6.4) \\
# P-value &  0.41 \\
# \hline
# Intercept & 59.6 \\
# (Std. Error) & (4.7) \\
# \hline
# Districts (N) & 16 \\
# \hlineB{2}
# \end{tabular}
# \caption{Agricultural employment rates (percent) for Fribourg and Vaud districts containing villages included in RDD samples. Linear regression. Data from Van der Walle (1980).}
# \label{tab:AgEmpl}
# \end{table}


############################################################
#Post-primary Education Rates
#use lm_ed1,2,3 to manually create Table A4
summary(lm_ed1) #1870
summary(lm_ed2) #1888
summary(lm_ed3) #1910

#Latex for Table A4
# \begin{table}
# \centering
# \begin{tabular}{ |c|c|c|c| }
# \hlineB{2}
# \multicolumn{4}{|c|}{Post-Primary Education Rates, Fribourg vs. Vaud} \\
# \hlineB{2}
# & 1870 & 1888 & 1910  \\
# \hline
# Vaud Coefficient   & -0.15 & 0.06 & 2.42 \\
# (Std. Error) &  (2.56)   & (2.57)  & (3.55)  \\
# P-value &  0.95  & 0.98  & 0.51  \\
# \hline
# Intercept & 10.54 & 9.18 & 16.15 \\
# (Std. Error) & (1.88) & (1.88) & (2.56) \\
# \hline
# Districts (N) & 16 & 16 & 16 \\
# \hlineB{2}
# \end{tabular}
# \caption{Post-primary education rates (percent) for Fribourg and Vaud districts containing villages included in RDD samples. Population-weighted linear regression. Data from Van der Walle (1980).}
# \label{tab:PostPrimaryEd}
# \end{table}

############################################################
#use if_1870,1888,1910 to manually create Table A6
#Make table for IF (normalized fertility rate, relative to Hutterites, Coale index)
summary(if_1870)
summary(if_1888)
summary(if_1910)

#Latex for Table A6
# \begin{table}
# \centering
# \begin{tabular}{ |c|c|c|c|c| }
# \hlineB{2}
# \multicolumn{4}{|c|}{Fertility Index, Fribourg vs. Vaud} \\
# \hlineB{2}
# & 1870 & 1888 & 1910  \\
# \hline
# Vaud Coefficient   & 0.003 & -0.041 & -0.108  \\
# (Std. Error) &  (0.024)   & (0.026)  & (0.027)  \\
# P-value &  0.899  & 0.142  & 0.001**  \\
# \hline
# Intercept & 0.329 & 0.373 & 0.362 \\
# (Std. Error) & (0.017) & (0.019) & (0.020) \\
# \hline
# Districts (N) & 16 & 16 & 16 \\
# \hlineB{2}
# \end{tabular}
# \caption{Fertility index (Coale's index $I_f$) for Fribourg and Vaud districts containing villages included in RDD samples. Population-weighted linear regression. Data from Van der Walle (1980).}
#                           \label{tab:Fertility_Index}
#                           \end{table}


############################################################
#use infmort_1870,1888,1910 to manually create Table %A7
summary(infmort_1870)
summary(infmort_1888)
summary(infmort_1910)

#Latex for Table A7
# \begin{table}
# \centering
# \begin{tabular}{ |c|c|c|c|c| }
# \hlineB{2}
# \multicolumn{4}{|c|}{Infant mortality rate (per 1000), Fribourg vs. Vaud} \\
# \hlineB{2}
# & 1870 & 1888 & 1910  \\
# \hline
# Vaud Coefficient   & -30.4 & -19.5 & -44.7  \\
# (Std. Error) &  (11.8)   & (7.5)  & (12.0)  \\
# P-value &  0.022*  & 0.021*  & 0.002**  \\
# \hline
# Intercept & 270 & 233 & 186 \\
# (Std. Error) & (9) & (5) & (9) \\
# \hline
# Districts (N) & 16 & 16 & 16 \\
# \hlineB{2}
# \end{tabular}
# \caption{Infant mortality rate (per 1000) for Fribourg and Vaud districts containing villages included in RDD samples. Population-weighted linear regression. Data from Van der Walle (1980).}
# \label{tab:Infant_Mortality}
# \end{table}

############################################################
#use CDR to manually create Table A8
#Crude death rate (result for paper)
summary(lm('CDR ~ Canton.x', data = C_merge, weights = C_merge$Population.1910)) #Vaud is clearly much lower, by 3 per thousand

#Latex for Table A8
# \begin{table}
# \centering
# \begin{tabular}{ |c|c|c| }
# \hlineB{2}
# \multicolumn{2}{|c|}{Total mortality rate (per 1000), Fribourg vs. Vaud} \\
# \hlineB{2}
# & 1910  \\
# \hline
# Vaud Coefficient  & -3.20  \\
# (Std. Error)  & (0.87)  \\
# P-value  & 0.002**  \\
# \hline
# Intercept & 20.4 \\
# (Std. Error)  & (0.64) \\
# \hline
# Districts (N)& 16 \\
# \hlineB{2}
# \end{tabular}
# \caption{Total mortality rate (per 1000) for Fribourg and Vaud districts containing villages included in RDD samples. Population-weighted linear regression. Data from Van der Walle (1980).}
# \label{tab:Total_Mortality}
# \end{table}


