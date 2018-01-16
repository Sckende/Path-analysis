################ Compilation pour Analyses de piste_data 4 ####################
setwd("/Users/nicolas/Documents/Claire/Doc doc doc !/R analysis/Data")
acqui<-read.table("acq_rate_1996-2016.txt", h=T, sep="\t", dec=",")
attaq<-read.table("attaq_rate_1996-2016.txt",h=T,sep="\t",dec = ",")

head(attaq)

####rajout du time lag (=lag) dans les données attaque
attaq$lag[attaq$year==1996 | attaq$year==2004]<-0
attaq$lag[attaq$year==1997 | attaq$year==2005 | attaq$year==2015]<-1
attaq$lag[attaq$year==1998 | attaq$year==2016]<-2
attaq$lag[attaq$year==1999]<-3

#####Calcul acquisition & attack rates (nb attaq/oeufs par jour)
attaq$atq_rate<-(attaq$attack/attaq$obs_length)*(3600*24)
summary(attaq)

acqui$acq_rate<-(acqui$egg/acqui$obs_lenght)*(3600*24)
summary(acqui)

#####Attack rate per year######
atqLEMy<-tapply(attaq$atq_rate,attaq$year,mean);atqLEMy
std.errY<-tapply(attaq$atq_rate,attaq$year,std.error);std.errY

TABy<-cbind(atqLEMy,std.errY)
TABy <- as.data.frame(TABy)

d<-NULL
j<-unique(attaq$year)
for (i in j){
  lagY <- attaq$lag[attaq$year == i]
  
  c<-data.frame(i,lagY)
  d<-rbind(d, c)
}

G<-aggregate(d,list(d$i,d$lagY), mean); G<-G[-9,]; G
G<-G[order(G$i),];G

TABy$lag<-G$lagY
TABy$year<-G$i

########################################################
##### Ajout des variables biotiques et abiotiques #####
######################################################
temp<-read.table("Tair moy 1989-2016 BYLCAMP.txt", h=T, sep="\t", dec=",")
prec<-read.table("precipitation_Bylot_1996-2016.txt",h=T, dec = ",", sep = "\t")
fox<-read.csv("Fox_abundance_Chevallier.txt", sep = "\t", dec = ",")
lmg<-read.csv("LEM96-2016.txt", sep = "\t", dec = ",")
prod<-read.csv("Prod_prim_camp_2.txt", sep = "\t", dec = ",")
AO<-read.csv("AO_saisonnier.txt", sep = ",", dec = ".")
breed<-read.csv("GOOSE_breeding_informations.txt", sep = "\t", dec = ".")


##### Températures #####
summary(temp)
require(date)

temp$YMD<-paste(temp$YEAR,temp$MONTH,temp$DAY,sep = "-")
temp$YMD<-strptime(temp$YMD, format = "%Y-%m-%d")
temp$JJ<-format(temp$YMD,format = "%j")
temp$JJ<-as.numeric(temp$JJ)

j<-TABy$year
TAB_temp<-NULL

for(i in j){
a<-min(attaq$date[attaq$year == i])
b<-max(attaq$date[attaq$year == i])

moy_Tc<-mean(temp$TEMP[temp$JJ>=a & temp$JJ<=b & temp$YEAR==i])
sd_Tc<-sd(temp$TEMP[temp$JJ>=a & temp$JJ<=b & temp$YEAR==i])
min_Tc<-min(temp$TEMP[temp$JJ>=a & temp$JJ<=b & temp$YEAR==i])
max_Tc<-max(temp$TEMP[temp$JJ>=a & temp$JJ<=b & temp$YEAR==i])

#min temp coolest day during spring    
SPRmin<-min(temp$TEMP[temp$JJ>=140 & temp$JJ<=171 & temp$YEAR==i])
#max temp coolest day during spring    
SPRmax<-max(temp$TEMP[temp$JJ>=140 & temp$JJ<=171 & temp$YEAR==i])
#min temp coolest day during early summer  
eSUMmin<-min(temp$TEMP[temp$JJ>=172 & temp$JJ<=196 & temp$YEAR==i])
#max temp warmest day during early summer
eSUMmax<-max(temp$TEMP[temp$JJ>=172 & temp$JJ<=196 & temp$YEAR==i])


    r<-data.frame(a,b,moy_Tc,sd_Tc,min_Tc,max_Tc,SPRmin,SPRmax,eSUMmin,eSUMmax)
    
    TAB_temp<-rbind(TAB_temp,r)
  }

TABy<-cbind(TABy,TAB_temp)


##### Précipitations #####
summary(prec)

j<-TABy$year
TAB_prec<-NULL

for(i in j){
  a<-min(attaq$date[attaq$year == i])
  b<-max(attaq$date[attaq$year == i])
  
  moy_pr<-mean(prec$rain[prec$day>=a & prec$day<=b & prec$year==i], na.rm = T)
  sd_pr<-sd(prec$rain[prec$day>=a & prec$day<=b & prec$year==i], na.rm = T)
  min_pr<-min(prec$rain[prec$day>=a & prec$day<=b & prec$year==i], na.rm = T)
  max_pr<-max(prec$rain[prec$day>=a & prec$day<=b & prec$year==i], na.rm = T)
  
  #min prec during spring    
  SPRmin_prec<-min(prec$rain[prec$day>=140 & prec$day<=171 & prec$year==i], na.rm = T)
  #max prec during spring    
  SPRmax_prec<-max(prec$rain[prec$day>=140 & prec$day<=171 & prec$year==i], na.rm = T)
  #min prec during early summer  
  eSUMmin_prec<-min(prec$rain[prec$day>=172 & prec$day<=196 & prec$year==i], na.rm = T)
  #max prec during early summer
  eSUMmax_prec<-max(prec$rain[prec$day>=172 & prec$day<=196 & prec$year==i], na.rm = T)
  
  
  s<-data.frame(moy_pr,sd_pr,min_pr,max_pr,SPRmin_prec,SPRmax_prec,eSUMmin_prec,eSUMmax_prec)
  
  TAB_prec<-rbind(TAB_prec,s)
}

TABy<-cbind(TABy,TAB_prec)

##### Autres variables #####
c<-TABy$year
suite<-NULL

for(d in c){

  fox_dens<-fox$natal_growth_dens[fox$year == d]
  #fox_dens_timelag<-fox$natal_growth_dens[fox$year == d+ ou - 1]
  lmg_abun<-lmg$LMG_C2[lmg$YEAR == d]
  prim_prod<-prod$PROD_INDICE_C2[prod$YEAR == d]
  
  winAO<-AO$winAO[AO$YEAR==d]
  sprAO<-AO$sprAO[AO$YEAR==d]
  esumAO<-AO$esumAO[AO$YEAR==d]
  lsumAO<-AO$lsumAO[AO$YEAR==d]
  
  nest_density<-breed$NEST_DENSITY[breed$YEAR==d]
  clutch_size<-breed$CLUTCH_SIZE[breed$YEAR==d]
  egg_abun<-breed$EGG_ABUN[breed$YEAR==d]
  ratio_JUVad<-breed$RATIO_YOU_AD[breed$YEAR==d]
  brood_size<-breed$BROOD_SIZE_BAND[breed$YEAR==d]
  nest_succ<-breed$NEST_SUCC[breed$YEAR==d]
  
  s<-data.frame(fox_dens, lmg_abun, prim_prod, winAO, sprAO, esumAO, lsumAO,nest_density,clutch_size,egg_abun,ratio_JUVad,brood_size,nest_succ)
  suite<-rbind(suite,s)
}

TABy<-cbind(TABy,suite)
summary(TABy)

write.csv(TABy,"Path analysis_data 4.txt")

