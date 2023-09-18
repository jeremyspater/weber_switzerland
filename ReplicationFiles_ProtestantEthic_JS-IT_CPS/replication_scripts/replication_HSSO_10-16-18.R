#CHANGE WORKING DIRECTORY
setwd('/Users/jeremyspater/Dropbox/duke/political economy core/weber/replication_files/ReplicationFiles_ProtestantEthic_JS-IT_CPS')

#HSSO data
#Births (Figure 1), real salaries (Figure 2)

rm(list=ls())
Num = function(x){as.numeric(as.character(x))}
s = function(x){summary(factor(x))}
library(plyr);library(dplyr, warn.conflicts = F)
library(ggplot2)

A = read.csv('./replication_datafiles/HSSO/C.16_read.csv',
             na.strings='', stringsAsFactors=F)  

#Create Figure 1, births in Fribourg and Vaud
ggplot(data = A) + geom_line(aes(x = Jahr, y = VD, linetype = 'solid') ) +
  geom_line(aes(x = Jahr, y = FR, linetype = 'dashed') ) +
  theme_bw() +
  scale_linetype_identity(name = 'Canton', guide = 'legend',labels = c('Fribourg','Vaud')) +
  ylab('Births') +
  xlab('Year') +
  xlim(low = 1800, high = 1910) 
#ggsave('./Figure1.jpg', height = 4, width = 6)

C = read.csv('./replication_datafiles/HSSO/G.02_read.csv',
             na.strings='', stringsAsFactors=F)

#Create Figure 2, real salaries in Switzerland
ggplot(data = C) + 
  geom_line(aes(x = Year, y = Real.salaries.index) ) +
  theme_minimal() +
  ylab('Real salaries index') +
  xlab('Year') +
  theme_bw(base_size = 14)
#ggsave('./Figure2.jpg', height = 4, width = 6)

