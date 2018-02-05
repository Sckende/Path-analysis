#setwd(dir="/Users/nicolas/Desktop")

#install.packages("readxl")
#require(readxl)
#nest2 <- read_excel("Nest monitoring 1996_2016.xlsx")
#summary(nest2)
#write.csv(nest2, file = "nest monitoring.csv")



#####-------------------With SEM data-------------------#####
setwd(dir = "/Users/nicolas/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
rm( list = ls ())
#####Succès de nidification#####
nest<-read.csv("GOOSE_nest monitoring.csv")
summary(nest)

###retrait des nids non inclus dans la colonie

#nest1<-nest[nest$NEST.TYPE=="Colony"|nest$NEST.TYPE=="Colony/collared female "|nest$NEST.TYPE=="Random plot/colony"|nest$NEST.TYPE=="Colony/Collared female"|nest$NEST.TYPE=="Collared female/colony"|nest$NEST.TYPE== "Collared female/Colony" | nest$NEST.TYPE== "Collared female and colony" | nest$NEST.TYPE== "Random Plot/Colony" | nest$NEST.TYPE== "Random plot/colony/collared female" |nest$NEST.TYPE=="Random plot/collared female/colony",]

nini<-nest[nest$NEST.TYPE=="Collared female and colony"|
    nest$NEST.TYPE=="Collared female/colony"|
    nest$NEST.TYPE=="Collared female/Colony"|
    nest$NEST.TYPE=="Colony"|
    nest$NEST.TYPE=="Colony/collared female"|
    nest$NEST.TYPE=="Colony/Collared female"|
    nest$NEST.TYPE=="Random plot"|
    nest$NEST.TYPE=="Random Plot"|
    nest$NEST.TYPE=="Random plot/collared female"|
    nest$NEST.TYPE=="Random Plot/collared female"|
    nest$NEST.TYPE=="Random plot/collared female/colony"|
    nest$NEST.TYPE=="Random plot/colony"|
    nest$NEST.TYPE=="Random Plot/Colony"|
    nest$NEST.TYPE=="Random plot/colony/collared female"|
    nest$NEST.TYPE== "Random plot/SNOW030",]

#summary(nest1)
#dim(nest1)
#unique(nest1$NEST.TYPE)

###retrait des nids dont l'issue est inconnue 
#ISSUE == 0 (exclus) 
#ISSUE == 1 (succès) 
#ISSUE == 2 (abandon) 
#ISSUE == 3 (détruit) 
#ISSUE == 5(inconnu)

#unique(nest1$ISSUE)
#nest1<-nest1[nest1$ISSUE==1 | nest1$ISSUE==2 | nest1$ISSUE==3,]
#dim(nest1)
#summary(nest1)

unique(nini$ISSUE)
nini<-nini[nini$ISSUE==1 | nini$ISSUE==2 | nini$ISSUE==3,]
dim(nini)
summary(nini)

### INICOD
#INICOD == 0 (date inconnue)
#INICOD == 1 (date observée - ponte)
#INICOD == 2 (date estimée - densité)
#INICOD == 3 (date estimée - eclosion)
###retrait des nids dont les dates initiation sont estimées avec la densité (INICOD == 2) ou inconnue (INICOD == 0)
###reste : INICOD==1 (date observée) & INICOD==3 (date estimée à l'éclos)

#nest1<-nest1[nest1$INICOD==1 | nest1$INICOD==3,]
nini<-nini[nini$INICOD==1 | nini$INICOD==3,]

###retrait des nids dont les dates initiation sont estimées avec la densité (INICOD == 2),inconnue (INICOD == 0)
# ou estimées à l'éclos (INICOD == 3)
#reste : INICOD==1 (date observée)
#nest1<-nest1[nest1$INICOD==1,]

###répartition nombre jours exposition :-s
require(ggplot2)

#nest1$AN<-as.character(nest1$AN)
#expobox <- ggplot(nest1, aes(AN,EXPO)) + geom_boxplot()
#expobox
#Same result with this command
#qplot(nest1$AN,nest1$EXPO, geom = "boxplot")

nini$AN<-as.character(nini$AN)
expobox <- ggplot(nini, aes(AN,EXPO)) + geom_boxplot()
expobox

###arrondir les jours d'exposition à l'entier supérieur
#nest1$EXPO<-ceiling(nest1$EXPO)
nini$EXPO<-ceiling(nini$EXPO)

###Réduction du data.frame avec seules les variables d'intérêt
#nidif<-data.frame(nest1$AN,nest1$NO,nest1$HABITAT,nest1$INI,nest1$INICOD,nest1$ECLO,nest1$EXPO,nest1$ISSUE,nest1$PRED,nest1$UTM.E,nest1$UTM.N,nest1$NB_VIS)
#summary(nidif);dim(nidif)
#names(nidif)<-c("AN","NO","HABITAT","INI","INICOD","ECLO","EXPO","ISSUE","PRED","UTM.E","UTM.N","NB_VIS" )

nini<-data.frame(nini$AN,nini$NO,nini$HABITAT,nini$INI,nini$INICOD,nini$ECLO,nini$EXPO,nini$ISSUE,nini$PRED,nini$UTM.E,nini$UTM.N,nini$NB_VIS,nini$NEST.TYPE)
summary(nini);dim(nini)
names(nini)<-c("AN","NO","HABITAT","INI","INICOD","ECLO","EXPO","ISSUE","PRED","UTM.E","UTM.N","NB_VIS","NEST.TYPE" )
nini$ID<-c(1:dim(nini)[1])


#nidif$EXPO[nidif$EXPO=="N/A"]=NA
#nidif$INI[nidif$INI=="N/A"]=NA
#nidif$ECLO[nidif$ECLO=="N/A"]=NA

nini$INI[nini$INI=="N/A"]=NA
nini$ECLO[nini$ECLO=="N/A"]=NA
nini$HABITAT[nini$HABITAT=="N/A"]=NA

#nidif$INI<-as.numeric(as.character(nidif$INI))
#nidif$ECLO<-as.numeric(as.character(nidif$ECLO))

nini$INI<-as.numeric(as.character(nini$INI))
nini$ECLO<-as.numeric(as.character(nini$ECLO))
nini$AN<-as.numeric(as.character(nini$AN))

###Traitement des NA dans la variable habitat
habi<-nini[is.na (nini$HABITAT) == T,]

#exploration
habi$AN<-as.character(habi$AN)
expobox <- ggplot(habi, aes(AN,UTM.E)) + geom_boxplot()
expobox

habi<-habi[habi$UTM.E!=0,]

habi$AN<-as.character(habi$AN)
expobox <- ggplot(habi, aes(AN,UTM.N)) + geom_boxplot()
expobox

#write.csv(habi, file = "habi.csv")


#retrait des lignes avec des habitat en NA et remplacement par "habi_V2" qui est le fichier issu du traitement via QGIS pour remplir les données manquantes pour la variable HABITAT
  habi_v2<-read.table("GOOSE_habi_V2.txt", sep = "\t", dec = ".", h = T)
  
  j<-unique(habi$ID)
  for (i in j){
nini$HABITAT[nini$ID==i]=habi_v2$HABITAT[habi_v2$ID==i] }
  nini<-nini[nini$UTM.E!=0,]#pour retirer les nids dont les coordonnées GPS sont 0/0
  
#gestion des deux dates INI avec NA
  #1 - valeur obtenue à partir de la date d'eclosion et de la clutch size : 186 - (23 (3-1))
nini$INI[nini$ID==2064]=161
#2 - date non calculable car clutch size inconnue, donc retrait de la ligne
nini<-nini[!is.na(nini$INI),]


#####Températures#####
#à partir d'ici nidif a été simplement remplacé par nini
#chargement des données de températures
#temp<-read.table("temperature_pond_inlet_1996-2015.txt", h=T, sep="\t", dec=",")
#temp<-read.table("TEMP_Tair moy 1989-2016 BYLCAMP.txt", h=T, sep="\t", dec=",")
temp<-read.table("TEMP_Tair moy 1989-2017 BYLCAMP.txt", h=T, sep="\t", dec=",")
temp <- temp[!temp$YEAR == 2017,]
summary(temp)
#temp<-na.omit(temp)
#transformation date en jour julien#
#install.packages("date")
require(date)

temp$YMD<-paste(temp$YEAR,temp$MONTH,temp$DAY,sep = "-")
temp$YMD<-strptime(temp$YMD, format = "%Y-%m-%d")
temp$JJ<-format(temp$YMD,format = "%j")
temp$JJ<-as.numeric(temp$JJ)

#Boucle calcul range temperature par individus nichant selon le nombre de jours d'exposition 
#à partir de la date d'initiation jusqu'à la date d'éclosion/prédation
j<-unique(nini$AN)
TAB_temp<-NULL

for(i in j){
  h<-subset(nini,nini$AN==i)
  l<-unique(h$NO)
  for (k in l){
    init<-h$INI[h$NO==k]
    eclos<-h$ECLO[h$NO==k]
    fin<-h$INI[h$NO==k] + h$EXPO[h$NO==k]
    moy_temp<-mean(temp$TEMP[temp$JJ>=init & temp$JJ<=fin & temp$YEAR==i])
    sd_temp<-sd(temp$TEMP[temp$JJ>=init & temp$JJ<=fin & temp$YEAR==i])
    min_temp<-min(temp$TEMP[temp$JJ>=init & temp$JJ<=fin & temp$YEAR==i])
    max_temp<-max(temp$TEMP[temp$JJ>=init & temp$JJ<=fin & temp$YEAR==i])

    #min temp coolest day during spring    
    SPRmin<-min(temp$TEMP[temp$JJ>=140 & temp$JJ<=171 & temp$YEAR==i])
    #min temp coolest day during early summer  
    eSUMmin<-min(temp$TEMP[temp$JJ>=172 & temp$JJ<=196 & temp$YEAR==i])
    #max temp warmest day during early summer
    eSUMmax<-max(temp$TEMP[temp$JJ>=172 & temp$JJ<=196 & temp$YEAR==i])
  #YEAR<-i
  #NUMBER<-k
  #INI<-init
  #ECLO<-eclos
  FIN<-fin
  #EXPO<-h$EXPO[h$NO==k]
  MEAN_temp<-moy_temp
  SD_temp<-sd_temp
  MIN_temp<-min_temp
  MAX_temp<-max_temp
  
  SPR_min<-SPRmin
  eSUM_min<-eSUMmin
  eSUM_max<-eSUMmax
  #ISSUE<-h$ISSUE[h$NO==k]
  #PRED<-h$PRED[h$NO==k]

  r<-data.frame(FIN,MEAN_temp,SD_temp,MIN_temp,MAX_temp,SPR_min,eSUM_min,eSUM_max)
  
  TAB_temp<-rbind(TAB_temp,r)
  }}

nini<-cbind(nini,TAB_temp)

#####Précipitations#####
#Boucle calcul range précipitation par individus nichant selon le nombre de jours d'exposition 
#à partir de la date d'initiation

prec<-read.table("PREC_precipitation_Bylot_1996-2016.txt",h=T, dec = ",", sep = "\t")
summary(prec)

j<-unique(nini$AN)
TAB_prec<-NULL
for(i in j){
  h<-subset(nini,nini$AN==i)
  l<-unique(h$NO)
  for (k in l){
    init<-h$INI[h$NO==k]
    eclos<-h$ECLO[h$NO==k]
    fin<-h$INI[h$NO==k] + h$EXPO[h$NO==k]
    moy_prec<-mean(prec$RAIN[prec$JJ>=init & prec$JJ<=fin & prec$YEAR==i], na.rm=T)
    sd_prec<-sd(prec$RAIN[prec$JJ>=init & prec$JJ<=fin & prec$YEAR==i], na.rm=T)
    max_prec<-max(prec$RAIN[prec$JJ>=init & prec$JJ<=fin & prec$YEAR==i], na.rm=T)
    cumul_prec<-sum(prec$RAIN[prec$JJ>=init & prec$JJ<=fin & prec$YEAR==i], na.rm=T)
    
    eSUMmean<-mean(prec$RAIN[prec$JJ>=172 & prec$JJ<=196 & prec$YEAR==i], na.rm=T)
    SPRmean<-mean(prec$RAIN[prec$JJ>=140 & prec$JJ<=171 & prec$YEAR==i], na.rm=T)
    #YEAR<-i
    #NUMBER<-k
    #INI<-init
    #ECLO<-eclos
    #FIN<-fin
    #EXPO<-h$EXPO[h$NO==k]
    MEAN_prec<-moy_prec
    SD_prec<-sd_prec
    MAX_prec<-max_prec
    eSUM_moy_prec<-eSUMmean
    SPR_moy_prec<-SPRmean
    #ISSUE<-h$ISSUE[h$NO==k]
    #PRED<-h$PRED[h$NO==k]
    
    s<-data.frame(MEAN_prec,SD_prec,MAX_prec,cumul_prec,eSUM_moy_prec,SPR_moy_prec)
    
    TAB_prec<-rbind(TAB_prec,s)
  }}

nini<-cbind(nini,TAB_prec)
summary(nini)

####Ajout des autres variables biologiques et météorologiques####
fox<-read.csv("FOX_abundance_Chevallier.txt", sep = "\t", dec = ",")
lmg<-read.csv("LEM_1993-2017.txt", sep = "\t", dec = ",")
prod<-read.csv("VEG_Prod_prim_camp_2.txt", sep = "\t", dec = ",")
AO<-read.csv("AO_saisonnier.txt", sep = ",", dec = ".")
breed<-read.csv("GOOSE_breeding_informations.txt", sep = "\t", dec = ",")

#Graphic with lmg and fox
plotlmg <- lmg[lmg$YEAR >= 1996 & lmg$YEAR < 2017, ]
plotfox <- fox[fox$year >= 1996 & fox$year < 2017, ]

plot(plotlmg$YEAR, plotlmg$LMG_C1, col = "green", bty = "n", ylim = c(0, 5), type = "l",  xaxp = c(1996, 2016, 10), xlab = "Année", lwd = 3)
lines(plotfox$year, plotfox$prop_natal_dens/10, type = "h", col = "blue", lwd = 3)
#Graphc with nesting success
plotgee <- breed[breed$YEAR <= 2016,]

plot(plotgee$YEAR, plotgee$NEST_SUCC, col = "green", bty = "n", ylim = c(0, 1), type = "l",  xaxp = c(1996, 2016, 10), xlab = "Année", lwd = 3)

m<-unique(nini$ID)
suite<-NULL

for(n in m){
  d<-nini$AN[nini$ID == n]
  fox_dens<-fox$natal_growth_dens[fox$year == d] #nb total de taniere en repro
  prop_fox_dens<-fox$prop_natal_dens[fox$year == d] #proportion de taniere en repro
  #fox_dens_timelag<-fox$natal_growth_dens[fox$year == d+ ou - 1]
  lmg_C2<-lmg$LMG_C2[lmg$YEAR == d]
  lmg_C1<-lmg$LMG_C1[lmg$YEAR == d]
  lmg_C1_C2<-lmg$LMG_C1_C2[lmg$YEAR == d]
  prim_prod<-prod$PROD_INDICE_C2[prod$YEAR == d]
  
  winAO<-AO$winAO[AO$YEAR==d]
  sprAO<-AO$sprAO[AO$YEAR==d]
  esumAO<-AO$esumAO[AO$YEAR==d]
  lsumAO<-AO$lsumAO[AO$YEAR==d]
  sumAO<-AO$sumAO[AO$YEAR==d]
  AOnidif<-AO$AOnidif[AO$YEAR==d]
  
  nest_density<-breed$NEST_DENSITY[breed$YEAR==d]
  clutch_size<-breed$CLUTCH_SIZE[breed$YEAR==d]
  egg_abun<-breed$EGG_ABUN[breed$YEAR==d]
  ratio_JUVad<-breed$RATIO_YOU_AD[breed$YEAR==d]
  brood_size<-breed$BROOD_SIZE_BAND[breed$YEAR==d]
  nest_succ<-breed$NEST_SUCC[breed$YEAR==d]

    s<-data.frame(fox_dens, prop_fox_dens, lmg_C2, lmg_C1, lmg_C1_C2, prim_prod, winAO, sprAO, esumAO, lsumAO, sumAO, AOnidif, nest_density, clutch_size, egg_abun, ratio_JUVad, brood_size, nest_succ)
    suite<-rbind(suite,s)
  }

nini<-cbind(nini,suite)
summary(nini)

#ajout des types d'annees de lemmings (peak, crash or intermediaire)
nini$lmg_year <- lmg$LMG_YEAR[match(nini$AN, lmg$YEAR)]
#questionner quelles annees sont pic ou crash ou inter
unique(nini$AN[which(nini$lmg_year == "inter")]) # "peak" / "inter"


####Conversion des issues des nids en variable binomiale SN####
nini$SN[nini$ISSUE == 1]<-1
nini$SN[nini$ISSUE == 2 |nini$ISSUE == 3 ]<-0


#write.csv(nini,"Path analysis_data 3.txt")
write.csv(nini,"Path analysis_data 3bis.txt") #idem que fichier Path analysis_data 3 mais avec variable proportion de taniere en reproduction en plus et les variable sumAO, AOnidif, et type d'annee de lemming
#write.csv(nini,"Path analysis_AOnidif.txt")

##### EXTRA #####
#regroupement des issues ensemble par année
TAB_temp<-TAB_temp[order(TAB_temp$YEAR,TAB_temp$ISSUE, decreasing = F),]
head(TAB_temp)

#visualisation
qplot(TAB_temp$YEAR,TAB_temp$MEAN, geom = "boxplot")

plot(nidif$AN, nidif$eSUM_moy_prec, type = "l")
plot(nidif$AN, nidif$SPR_moy_prec, type = "l")
plot(nidif$AN, nidif$eSUM_min, type = "l")
plot(nidif$AN, nidif$eSUM_max, type = "l")
plot(nidif$AN, nidif$SPR_min, type = "l")

#write.csv(TAB,"TAB.csv")


#write.csv(TAB_prec, "TAB_prec.txt")
#write.csv(TAB_temp, "TAB_temp.txt")
#visualisation
qplot(TAB_prec$YEAR,TAB_prec$MEAN_prec, geom = "boxplot")

#####représentation graphique#####
#différencier symbole/couleur si nid en échec ou succès
AN96<-TAB[TAB$YEAR==1996,]
plot(AN96$MEAN)
AN96bis<-AN96[order(AN96$ISSUE,decreasing=T),]


plot(AN96bis$MEAN, cex=1, xlab = "ANNÉE 96", 
  col = c("green","red", "red")[as.numeric(AN96bis$ISSUE)])
tapply(TAB$MEAN,TAB$YEAR,boxplot)
boxplot(TAB$MEAN[TAB$YEAR==2016], xlab = "2016")
unique(TAB$YEAR)

par(mfrow = c(3,7))

for(i in j){
  boxplot(TAB_temp$MEAN[TAB_prec$YEAR==i], xlab = paste("année",i, sep = " "))
}
require(ggplot2)  
TAB$ISSUE<-as.factor(TAB$ISSUE)
TAB$PRED<-as.factor(TAB$PRED)
qplot(NUMBER,MEAN, data = TAB_temp, colour = ISSUE, shape = PRED)
for(i in j){
  qplot(TAB$NUMBER[TAB$YEAR==i],TAB$MEAN[TAB$YEAR==i], colour = TAB$ISSUE[TAB$YEAR==i], 
    shape = TAB$PRED[TAB$YEAR==i],ylab = "temperature range per capita", 
    xlab = paste("année",i, sep = " "))
}
par(mfrow = c(1,1))
qplot(TAB_temp$NUMBER[TAB_temp$YEAR==2016],TAB_temp$MEAN[TAB_temp$YEAR==2016], colour = TAB_temp$ISSUE[TAB_temp$YEAR==2016], 
  shape = TAB_temp$PRED[TAB_temp$YEAR==2016],ylab = "temperature range per capita", 
  xlab = paste("année",2016, sep = " "))


