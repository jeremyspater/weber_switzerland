#Vs 10-16v1, it breaks "percent catholic" into 2 lines in legend of figure 3
#Vs 10-15, it saves Fig 5 to a file to fix formatting issue
#Vs 10-11, it increases text size for Figure 7
#Vs 10-3, it changes maps to black-and-white
#CHANGE WORKING DIRECTORY
setwd('/Users/jeremyspater/Dropbox/duke/political economy core/weber/replication_files/ReplicationFiles_ProtestantEthic_JS-IT_CPS')

library(rdrobust)
library(plyr);library(dplyr)
library(ggplot2)

########################################################################
#Table 1
#Use census data to ccompare growth of largest cities in Fribourg and Vaud

rm(list=ls())
A = readRDS('./replication_datafiles/census/2-21-17 input.RDS')
A = filter(A, PctGerman < 0.5) #keep places with < 50% German; 616 / 665

VaudCit45k = filter(A, State.1910 == 'Vaud', Pop.1860 >= 4500) #there are 3
FribCit2k = filter(A, State.1910 == 'Freiburg', Pop.1860 >= 2000) #there are 3
vd = dplyr::select(VaudCit45k, State.1910, clean.city.1910, Pop.1860, Pop.1910, Growth)
fb = dplyr::select(FribCit2k, State.1910, clean.city.1910, Pop.1860, Pop.1910, Growth)

#Results for Table 1
mutate(vd, PctGrowth = Growth / Pop.1860)
mutate(fb, PctGrowth = Growth / Pop.1860)
sum(A$Pop.1860[which(A$State.1860 == 'Freiburg')]) #77304
sum(A$Pop.1910[which(A$State.1910 == 'Freiburg')]) #105407
#delta 28103; 36.3%
sum(A$Pop.1860[which(A$State.1860 == 'Vaud')]) #213035
sum(A$Pop.1910[which(A$State.1910 == 'Vaud')]) #323547
#delta 110512; 51.9%

#Latex for Table 1
# \begin{table}
# \centering
# \begin{tabular}{ |c|c|c|c|c|c| }
# \hlineB{2}
# \textbf{State} & \textbf{City} &  \textbf{Pop. 1860} & \textbf{Pop. 1910} & \textbf{Growth} & \textbf{Growth} ($\%$) \\
# \hlineB{2}
# \hlineB{2}
# Vaud    & Lausanne &   20,515 &   66,227 &  45,712 & $223\%$ \\
# Vaud    & Vevey &   6,494  &  14,031  & 7,537 & $116\%$  \\
# Vaud    & Yverdon &  4,986   &  8,846  & 3,860 & $77\%$ \\
# \hline
# Vaud    & \textit{All} &  213,035   &  323,547  & 110,512 & $52\%$ \\
# \hlineB{2}
# Fribourg & Fribourg & 10,454 & 20,367 &  9,913 & $95\%$ \\
# Fribourg & Chatel-St-Denis  &   2,381  &   2,696 &   315 & $13\%$ \\
# Fribourg & Bulle  &  2,086  &   4,099 &  2,013 & $97\%$ \\
# \hline
# Fribourg & \textit{All} & 77,304 & 105,407 & 28,103 & $36\%$ \\
# \hlineB{2}
# \end{tabular}
# \caption{Comparison of population growth between the largest cities in Fribourg and Vaud, and province totals.}
# \label{city_comparison}
# \end{table}
########################################################################

########################################################################
#Results for Table 2
#Sharp RDD results, for base model with no controls, with different population thresholds

#Regression with no controls; vary population thresholds
rm(list=ls())
A = readRDS('./replication_datafiles/census/2-21-17 input.RDS')
A = filter(A, PctGerman < 0.5) #keep places with < 50% German; 616 / 665
A1000 = filter(A, Pop.1860 < 1000) #keep places with < 2000 people; 572 / 616
A2000 = filter(A, Pop.1860 < 2000) #keep places with < 2000 people; 598 / 616
A3000 = filter(A, Pop.1860 < 3000) #keep places with < 2000 people; 606 / 616

main0_1000 = rdrobust(A1000$Growth, A1000$Dist)
main0_2000 = rdrobust(A2000$Growth, A2000$Dist)
main0_3000 = rdrobust(A3000$Growth, A3000$Dist)
main0_ALL = rdrobust(A$Growth, A$Dist)

summary(main0_1000)
summary(main0_2000)
summary(main0_3000)
summary(main0_ALL)

#Latex for Table 2
# \begin{table}
# \centering
# \begin{tabular}{ |c|c|c|c|c| }
# \hlineB{2}
# \multicolumn{5}{|c|}{Sharp RDD} \\
# \hlineB{2}
# & Pop. $<1$k & Pop. $<2$k & Pop. $<3$k & All Towns \\
# \hline
# Coefficient   & 54    &135&   143 & 152 \\
# (Std. Error) &   (25)  & (72)   & (80) & (109) \\
# P-value &   0.029**  & 0.059*  & 0.074* & 0.16 \\
# Vaud Obs. &99  & 101 &  103 & 96 \\
# Fribourg Obs.   &117 & 117 &  118 & 110 \\
# Bandwidth [km] &   4.6  & 4.3 & 4.3 & 3.4 \\
# \hline
# Controls &  \textit{No}  & \textit{No} & \textit{No} & \textit{No} \\
# \hline
# Sample Frame &   Pop. $<1$k  & Pop. $<2$k & Pop. $<3$k & All Towns \\
# \hlineB{2}
# \end{tabular}
# \caption{Sharp RDD results, for base model with no controls, for varying population thresholds. Local-linear polynomial with CTT bandwidth. Coefficient is estimated effect on population growth 1860-1910 of being located on the Fribourg side of the border.}
# \label{no_coef}
# \end{table}
##################################################################################################################################

##################################################################################################################################
#Table 3
#Fuzzy RDD results, with controls

#Analysis is done in STATA: see "replication_scripts/replication_Census_FuzzyRDD_10-3-18.do"

#Latex for Table 3:
# \begin{table}
# \centering
# \begin{tabular}{ |c|c|c|c|c| }
# \hlineB{2}
# \multicolumn{5}{|c|}{Fuzzy RDD (Second Stage)} \\
# \hlineB{2}
# & Model 1 & Model 2 & Model 3 & Model 4 \\
# \hline
# Coefficient   & 56 & 58 & 68 & 64\\
# (Std. Error) &  (21)   & (22)  & (23) & (22)  \\
# P-value &  0.009***   & 0.009***  & 0.003*** & 0.004*** \\
# Vaud Obs. & 117 & 107  &  130 & 128 \\
# Fribourg Obs.   & 147  & 137 &  161 & 158 \\
# Bandwidth [km] &  6.4 & 5.4 & 7.9 & 7.6 \\
# \hline
# Controls &  \textit{No}  & Elev., Rough., & \parbox[t]{3cm}{\centering Elev., Rough.,\\Lat., Long.} & \parbox[t]{3cm}{\centering Elev., Rough.,\\Lat., Long.,\\ 1860. Pop.}  \\
# \hline
# Sample Frame &   Pop. $<1$k  & Pop. $<1$k & Pop. $<1$k & Pop. $<1$k  \\
# \hlineB{2}
# \end{tabular}
# \caption{Fuzzy RDD results (second stage), for various sets of controls: elevation, roughness, and initial population. Local-linear polynomial with CTT bandwidth. Coefficient is is treatment-effect-on-the-treated, on population growth 1860-1910, of a unit increase in the proportion of the population that is Catholic.}
# \label{fuz_1}
# \end{table}
##################################################################################################################################

##################################################################################################################################
#Table 4: Growth of Catholic and total populations in Vaud's largest cities

rm(list=ls())
A = readRDS('./replication_datafiles/census/2-21-17 input.RDS')
A = filter(A, PctGerman < 0.5) #keep places with < 50% German; 616 / 665

VaudCit45k = filter(A, State.1910 == 'Vaud', Pop.1860 >= 4500) #there are 3
select(VaudCit45k, clean.city.1910, Pop.1860, Pop.1910, Katholisch.1860, Katholisch.1910) %>%
  mutate(CathGrowth = (Katholisch.1910-Katholisch.1860)/Katholisch.1860,
         TotalGrowth = (Pop.1910-Pop.1860)/Pop.1860 )

#Latex for Table 4
# \begin{table}
# \centering
# \begin{tabular}{ |c|c|c|c|c| }
# \hlineB{2}
# \textbf{City} &  \textbf{Cath. 1860} & \textbf{Cath. 1910} & \textbf{Cath. Growth} ($\%$) & \textbf{Total Growth} ($\%$)  \\
# \hlineB{2}
# \hlineB{2}
# Lausanne   & 1,601 & 15,597 &  $874\%$ & $223\%$   \\
# Vevey   &  794  &  4,514 & $469\%$ & $116\%$  \\
# Yverdon    &  412 & 1,258 & $205\%$ & $77\%$  \\
# \hlineB{2}
# \end{tabular}
# \caption{Growth of Catholic and total populations in Vaud's largest cities.}
#   \label{cath_comparison}
#   \end{table}
##################################################################################################################################

##################################################################################################################################
#Figures 3 and 4: MAPS of religious distributions and large cities in Fribourg and Vaud

library(raster)
library(ggplot2)
library(ggrepel)

rm(list=ls())
A=readRDS('./replication_datafiles/census/merged data 9-13-16.RDS')
A$clean.city.1910[which(A$clean.city.1910 == 'Lo Chatelard-Montreux')] = "Le Chatelard-Montreux"

source('./replication_scripts/getData_2_function.R') #change getData function to download from https:// rather than http:// . . .

#get borders
adm1<- getData_JS('GADM', path = '.', country='CHE',level=1)
CantonMap=adm1[adm1$NAME_1=="Fribourg"|adm1$NAME_1=="Vaud",]
rm(adm1)

CM=fortify (CantonMap) #id: 7 fribourg; 24 vaud

library(plyr,quietly=T)
CM$State=CM$id
CM$State=mapvalues(CM$State, from=c('7','24'), to=c('Fribourg','Vaud'))

#Figure 3: Religion map
A$PctCath=100*A$Katholisch.1910/A$Pop.1910
pl = ggplot(A[!is.na(A$PctCath),],aes(x=lon, y=lat))
pl = pl + geom_polygon(data=CM[which(CM$id==24),],aes(x=long, y=lat,group=group,fill=State))#,alpha=0.1))
pl = pl + geom_polygon(data=CM[which(CM$id==7),],aes(x=long, y=lat,group=group,fill=State))#,alpha=0.1))
pl = pl + geom_point(aes(color=PctCath))
pl + labs(fill='State', x='Longitude',y='Latitude') +
  scale_color_gradient(name = "Percent\nCatholic", low = 'white', high = 'black') +
  theme_bw(base_size = 16) +
  #theme_set(theme_bw(base_size = 18)) +
  scale_fill_grey(start = 0.65, end = 0.35) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
#ggsave('./Figure3_10-16-18.jpg', height = 4, width = 6)

#Figure 4: Cities map

#devtools::install_github("dkahle/ggmap", ref = "tidyup")
#https://github.com/dkahle/ggmap/issues/51
#https://stackoverflow.com/questions/19827598/error-in-get-map-using-ggmap-in-r
library(ggmap,quietly=T)

#To make this work, you need a free API key from Google
#Use this link: https://console.developers.google.com/apis/api/static_maps_backend?project=_
#Register for a free API key for the "Maps Static API"; enter it on the two lines below
#[redacted]
#gmap2=get_map(location=c(lon=6.6, lat=46.7), zoom=9, maptype='satellite', api_key = redacted)
#saveRDS(gmap2, file = 'swissmap_google')
#gmap2_bw = get_map(location=c(lon=6.6, lat=46.7), zoom=9, maptype='satellite', color = 'bw',
                  # api_key = redacted)
#saveRDS(gmap2_bw, '/Users/jeremyspater/Dropbox/duke/political economy core/weber/replication_files/my version/swissmapBW.RDS')
#gmap2 = readRDS('/Users/jeremyspater/Dropbox/duke/political economy core/weber/replication_files/my version/swissmapBW.RDS')
#plot(gmap2)
#gmap3_bw = get_map(location=c(lon= 6.73 , lat=46.6), zoom=9, maptype='satellite', color = 'bw',
                    #api_key = redacted)
#saveRDS(gmap3_bw, '/Users/jeremyspater/Dropbox/duke/political economy core/weber/replication_files/my version/swissmapBW_centered.RDS')
gmap2 = readRDS('./swissmapBW_centered.RDS')

#BLACK-AND-WHITE VERSION 10-12-18
n=ggmap(gmap2, darken = c(0.1, 'white'))
n=n+geom_polygon(data=CM[which(CM$group %in% c('24.1','24.2','24.5')),],
                 aes(x=long, y=lat,group=group,fill=State),alpha=0.5, color = 'black')
n=n+geom_polygon(data=CM[which(CM$id==7),],
                 aes(x=long, y=lat,group=group,fill=State),alpha=0.5, color = 'black')
n=n+scale_size_continuous(breaks=c(4000,10000,60000),range = c(1,10))
n=n+geom_point(data=A[which(A$Pop.1910>3000),],aes(x=lon,y=lat,size=Pop.1910),color='black',alpha=1)
n+geom_text_repel(aes(label = clean.city.1910),
                  color = 'white',#"gray70",
                  data = A[which(A$Pop.1910>4000),],#3000),],
                  force = 10,
                  size=4 ) + #   ,
                  #nudge_y = c(ifelse(A[which(A$Pop.1910>4000),'clean.city.1910'] == 'Lausanne', 0.1, 0),
                  #            ifelse(A[which(A$Pop.1910>4000),'clean.city.1910'] == 'Vevey', -0.2, 0) ) ) +
  labs(fill='State', x='Longitude',y='Latitude')  +
 # guides(fill = guide_legend(title = 'State'),
 #   size=guide_legend(title="1910 Pop.")) +
  #scale_size_continuous(name = "1910 Pop.") +
  theme_bw(base_size = 16) +
  scale_fill_grey(start = 1, end = 0.65) 

#ggsave('./Figure4_10-15-18.jpg', height = 4, width = 6)
  
##################################################################################################################################

##################################################################################################################################
#Figure 5
#Proportions of Catholics, binned by distance from the border.

rm(list=ls())
library(ggplot2)

A=readRDS('./replication_datafiles/census//10-5 input.RDS')
A$PctCath=A$Katholisch.1910/(A$Katholisch.1910+A$Protestantisch.1910) #24 na's; from A$Kath. 
#use 1860 data for where 1910 doesn't exist
A$PctCath[which(is.na(A$PctCath))]=(A$Katholisch.1860/(A$Katholisch.1860+A$Protestantische.1860))[which(is.na(A$PctCath))] #use 1860 data

#sequence of distance breakpoints
br=seq(from=min(A$Dist),to=max(A$Dist),by=0.5)
midpoints=(br[-1]+br[-length(br)])/2
A$bin=cut(A$Dist,breaks=br)

test=data.frame(cbind(midpoints,by(A$Pop.1910,A$bin,mean),by(A$Growth,A$bin,mean),
                      by(A$PctCath,A$bin,mean),by(A$PctCath,A$bin,sd)))
test=cbind(test,table(A$bin))[,c(1,2,3,4,5,7)]
names(test)=c('midpoints','pop 1910','growth','mean','sd','Freq')
test$min=test$mean - 2*test$sd
test$max=test$mean + 2*test$sd

#Create Figure 5
ggplot(test[which(test$midpoints < 10 & test$midpoints > -10),],aes(x=midpoints,y=mean))+geom_point(size=2)+
  #geom_errorbar(aes(ymin=(min),ymax=(max)))+
  scale_x_continuous(limits = c(-10, 10))+
  scale_y_continuous(limits = c(-0.25, 1.2))+
  ggtitle("Average Proportion Catholic by Bin") +
  labs(x="Distance (bin midpoint) [km]",y="Avg. Proportion Catholic") +
  theme_bw()
#ggsave('./Figure5.jpg', height = 4, width = 6)

##################################################################################################################################

##################################################################################################################################
#Analysis for Figure 6
#"Population growth averages 1860-1910, villages with initial population <1,000,
#binned by distance from the border, 0.25km bin width. Shown with local-linear RDD regression
#and 95% confidence intervals.

rm(list=ls())
library(plyr);library(dplyr)
library(ggplot2)
library(rdrobust)

A = readRDS('./replication_datafiles/census/2-21-17 input.RDS')
A = filter(A, PctGerman < 0.5) #keep places with < 50% German; 616 / 665
A = filter(A, Pop.1860 < 1000) #keep places with < 2000 people; 598 / 616

A$Treat = A$Dist > 0

main0 = rdrobust(A$Growth, A$Dist) #Run RDD
bw = main0$bws[1,1] #CTT bandwidth

B = A[which(abs(A$Dist) < bw),c('Growth','Treat','Dist')] #Create data frame for plotting, using data inside CTT bandwidth
B$w = 1 - (abs(B$Dist) / bw) #triangular weights

bs = 0.25 #Bin size
br=c(seq(from=0,to=min(B$Dist)-bs,by=-bs),seq(from=0,to=max(B$Dist)+bs,by=bs)) #Create breaks
br=br[order(br)] 
br=br[!duplicated(br)]
midpoints=(br[-1]+br[-length(br)])/2
B$bin=cut(B$Dist,breaks=br) #Add breaks to main dataframe

Br=data.frame(midpoints,Growth = ddply(B, 'bin', function(x) mean(x$Growth))$V1) #Average growth for each bin
rm(br,bs,midpoints)

Br$Treat = Br$midpoints > 0 #Treatment variable
Br=rbind(Br, c(0,NA,FALSE), c(0,NA,TRUE))  #Add NA's at distance = 0 to facilitate plotting
Br$Treat = as.logical(Br$Treat)
Br = Br[order(Br$midpoints),] %>% plyr::rename(c('midpoints' = 'Dist')) #Re-order

HB = lm('Growth ~ Treat + Dist + Dist*Treat', data=B, weights = w) #Re-do RDD
pr1 = predict(HB, Br, interval="confidence",se.fit=T) #Create onfidence intervals
Br1 = data.frame(Br, pr1)  #Create data frame for plotting

#Figure 6
ggplot(Br1,aes(x=Dist,y=Growth)) + 
  geom_ribbon(aes(ymin=fit.lwr,ymax=fit.upr),alpha=0.3,data=filter(Br1, Treat==0)) +
  geom_ribbon(aes(ymin=fit.lwr,ymax=fit.upr),alpha=0.3,data=filter(Br1, Treat==1)) +
  geom_line(aes(y=fit.fit),data=filter(Br1, Treat==0))+
  geom_line(aes(y=fit.fit),data=filter(Br1, Treat==1))+
  geom_point() + theme_bw() + theme(axis.text=element_text(size=12),
                                    axis.title=element_text(size=14),
                                    plot.title = element_text(hjust = 0.5))+
  labs(x = "Distance from border [km]", title = "Sharp RDD plot")
##################################################################################################################################

##################################################################################################################################
#Results for Figure 7: Run RDD for various bandwidths

rm(list=ls())
library(rdrobust)
library(plyr);library(dplyr)
library(ggplot2)

A = readRDS('./replication_datafiles/census/2-21-17 input.RDS')
A = filter(A, PctGerman < 0.5) #keep places with < 50% German; 616 / 665
A = filter(A, Pop.1860 < 1000) #keep places with < 1000 people; 572 / 616
bw = data.frame(h=seq(3,9,1),b=seq(3,9,1))

coefs = ddply(bw, 'h', function(x) rdrobust(A$Growth, A$Dist, h=x$h, b=x$b)[['coef']][1])
cis = ddply(bw, 'h', function(x) rdrobust(A$Growth, A$Dist, h=x$h, b=x$b)[['ci']][1,])
res = right_join(coefs, cis, by='h')

#Create Figure 7
ggplot(data=res) + 
  geom_point(aes(x=h,y=V1)) +
  geom_errorbar(aes(x=h, ymin=`CI Lower`, ymax = `CI Upper`))+
  xlab("Bandwidth [km]") +
  ylab("Treatment effect [people per village]") +
  theme_bw(base_size = 14)
#ggsave('./Figure7.jpg', height = 4, width = 6)

########################################################################################################################################
#Results for Table A1

rm(list=ls())
library(rdrobust)
library(plyr);library(dplyr)
library(ggplot2)

A = readRDS('./replication_datafiles/census/2-21-17 input.RDS')
A = filter(A, PctGerman < 0.5) #keep places with < 50% German; 616 / 665
A = filter(A, Pop.1860 < 1000) #keep places with < 1000 people; 572 / 616

#Sharp RDD results
#main regression; no controls
main0 = rdrobust(A$Growth, A$Dist)
#main regression; control for elevation and roughness
main1 = rdrobust(A$Growth, A$Dist, covs=cbind(A$elev,A$rough))
main2 = rdrobust(A$Growth, A$Dist, covs=cbind(A$elev,A$rough,A$lon,A$lat))
main3 = rdrobust(A$Growth, A$Dist, covs=cbind(A$elev,A$rough,A$lon,A$lat,A$Pop.1860))

summary(main0)
summary(main1)
summary(main2)
summary(main3)

#Latex for Table A1
# \begin{table}
# \centering
# \begin{tabular}{ |c|c|c|c|c| }
# \hlineB{2}
# \multicolumn{5}{|c|}{Sharp RDD} \\
# \hlineB{2}
# & Model 1 & Model 2 & Model 3 & Model 4 \\
# \hline
# Coefficient   & 54 & 53 & 50 & 47\\
# (Std. Error) &  (25)   & (24)  & (24) & (19) \\
# P-value &  0.029**   & 0.030**  & 0.038** & 0.014** \\
# Vaud Obs. & 99 & 100  &  101 & 110 \\
# Fribourg Obs.   & 117  & 119 &  122 & 140 \\
# Bandwidth [km] &  4.6    & 4.7 & 4.8  & 5.7\\
# \hline
# Controls &  \textit{No}  & Elev., Rough., & \parbox[t]{3cm}{\centering Elev., Rough.,\\Lat., Long.} & \parbox[t]{3cm}{\centering Elev., Rough.,\\Lat., Long.,\\ 1860. Pop.}  \\
# \hline
# Sample Frame &   Pop. $<1$k  & Pop. $<1$k & Pop. $<1$k & Pop. $<1$k  \\
# \hlineB{2}
# \end{tabular}
# \caption{Sharp RDD results, for various sets of controls: elevation, roughness, and initial population. Local-linear polynomial with CTT bandwidth. Coefficient is estimated effect on population growth 1860-1910 of being located on the Fribourg side of the border.}
# \label{coef_1}
# \end{table}

####################
#Results for Table A2
#Sharp RDD results, Growthrate as DV
A$GrowthRate = 100*((A$Pop.1910 / A$Pop.1860)^(1/(1910-1860)) -1)
main0gr = rdrobust(A$GrowthRate, A$Dist)
main1gr = rdrobust(A$GrowthRate, A$Dist, covs=cbind(A$elev,A$rough))
main2gr = rdrobust(A$GrowthRate, A$Dist, covs=cbind(A$elev,A$rough,A$lon,A$lat))
main3gr = rdrobust(A$GrowthRate, A$Dist, covs=cbind(A$elev,A$rough,A$lon,A$lat,A$Pop.1860))

summary(main0gr)
summary(main1gr)
summary(main2gr)
summary(main3gr)

#Latex for Table A2
# \begin{table}
# \centering
# \begin{tabular}{ |c|c|c|c|c| }
# \hlineB{2}
# \multicolumn{5}{|c|}{Sharp RDD: Percent Growth Rate as DV} \\
# \hlineB{2}
# & Model 1 & Model 2 & Model 3 & Model 4 \\
# \hline
# Coefficient   & 0.37 & 0.37 & 0.36 & 0.35\\
# (Std. Error) &  (0.12)   & (0.12)  & (0.12) & (0.12) \\
# P-value &  0.0018***   & 0.0015***  & 0.0025*** & 0.0026*** \\
# Vaud Obs. & 106 & 107  &  109 & 110 \\
# Fribourg Obs.   & 135  & 137 &  138 & 140 \\
# Bandwidth [km] &  5.3    & 5.4 & 5.5  & 5.8\\
# \hline
# Controls &  \textit{No}  & Elev., Rough., & \parbox[t]{3cm}{\centering Elev., Rough.,\\Lat., Long.} & \parbox[t]{3cm}{\centering Elev., Rough.,\\Lat., Long.,\\ 1860. Pop.}  \\
# \hline
# Sample Frame &   Pop. $<1$k  & Pop. $<1$k & Pop. $<1$k & Pop. $<1$k  \\
# \hlineB{2}
# \end{tabular}
# \caption{Sharp RDD results, for various sets of controls: elevation, roughness, and initial population. Local-linear polynomial with CTT bandwidth. Coefficient is estimated effect on the percentage population growth rate from 1860-1910 of being located on the Fribourg side of the border.}
# \label{coef_1_rate}
# \end{table}

################################################################################

################################################################
#Analysis for Table A5:
#Compare Murten (German/Protestant parts of Fribourg) to Catholic parts
library(MASS)
rm(list=ls())
dat = readRDS('./replication_datafiles/census/2-21-17 input.RDS')

A = filter(dat, Pop.1860 < 2000, State.1910 == 'Freiburg') 
#models 3 and 4
summary(rlm(A$Growth~A$PctCath))
summary(rlm(A$Growth~A$PctCath+A$elev+A$rough+A$lon+A$lat+A$Pop.1860))

#models 1 and 2
A = filter(dat, Pop.1860 < 1000, State.1910 == 'Freiburg') 
summary(rlm(A$Growth~A$PctCath))
summary(rlm(A$Growth~A$PctCath+A$elev+A$rough+A$lon+A$lat+A$Pop.1860))

#Latex for Table A5:
# \begin{table}
# \centering
# \begin{tabular}{ |l|c|c|c|c| }
# \hlineB{2}
# \multicolumn{5}{|c|}{Robust linear regression} \\
# \hlineB{2}
# & \multicolumn{4}{c|}{\textit{Dependent variable}: Growth} \\
# \cline{2-5}
# & Model 1 & Model 2 & Model 3 & Model 4 \\
# \hline
# Pct. Catholic & 28 & 38 & 21 & 35\\
# & (16)* & (15)** & (16) & (16)** \\[0.25cm]
# Elevation & & -0.04 & &-0.04 \\
# & & (0.05) & & (0.05)\\[0.25cm]
# Roughness & & -0.04 & &-0.04 \\
# & & (0.04) & & (0.04)\\[0.25cm]
# Longitude & & 143  & &123 \\
# & & (35)*** & & 37 \\[0.25cm]
# Latitude & & -123 & & -114 \\
# & & (65)* & & (68)* \\[0.25cm]
# Pop. 1860 & &0.22 & & 0.25 \\
# & & (0.02)*** & & (0.01)***\\[0.25cm]
# Intercept   & 20 & 4726 & 30 & 4445 \\
# &  (15)   & (2921)  & (15)** & (3068)  \\
# \hline
# Sample Frame &  \parbox[t]{3cm}{\centering Pop. $<1$k,\\Fribourg}& \parbox[t]{3cm}{\centering Pop. $<1$k,\\Fribourg} 
# & \parbox[t]{3cm}{\centering Pop. $<2$k,\\Fribourg}& \parbox[t]{3cm}{\centering Pop. $<2$k,\\Fribourg}   \\
# \hline
# Observations & 263 & 263 & 272 & 272 \\
# \hlineB{2}
# \textit{Note:}  & \multicolumn{4}{r|}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\ 
# \hline
# \end{tabular}
# \caption{Regression results for Fribourg, showing correlation between proportion Catholic and growth.}
# \label{murten_regression}
# \end{table}

##################################################################################################################################



