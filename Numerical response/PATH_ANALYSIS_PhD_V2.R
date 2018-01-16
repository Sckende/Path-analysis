#####Analyse de piste - données Ph.D.#####
#Les données ont été traitées à l'aide de deux scripts:
#"compilation pour Analyse de piste_data 3.R"
#"PATH_ANALYSIS_PhD.R"

require(nlme)
require(lme4)
require(piecewiseSEM)

#####-------------------------SEM data-------------------------#####
setwd(dir = "/Users/nicolas/Documents/Claire/Doc doc doc !/R analysis/Data")
load("SEM.Rdata")#attention, jeux de données ne contenant que des IniDate = 1 
summary(SEM)

#to have the row number per group
table(SEM$AN)
#####ALMOST #1#####
alm1<-list(
  lme(fox_dens~lmg_abun+prim_prod,data = SEM, random = ~1|AN, na.action = na.omit),
  glmer(SN~prim_prod+fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), data=SEM, family=binomial(link = "logit"), na.action = na.omit)
)
# Get goodness-of-fit and AIC
sem.fit(alm1, SEM, conditional = T)
# Extract path coefficients
sem.coefs(alm1, SEM)
#modèle avec relations simples. Pas de relation entre PP et SN et habitat et SN.

#####ALMOST #3#####
alm3<-list(
  lme(fox_dens~lmg_abun+prim_prod+esumAO,data = SEM, random = ~1|AN, na.action = na.exclude),
  lme(lmg_abun~prim_prod+esumAO+MEAN_temp+MEAN_prec,data = SEM, random = ~1|AN, na.action = na.exclude),
  glmer(SN~prim_prod+fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), data=SEM, family=binomial(link = "logit"), na.action = na.exclude),
  lme(prim_prod~esumAO,data = SEM, random = ~1|AN, na.action = na.exclude)
)

# Get goodness-of-fit and AIC
sem.fit(alm3, SEM, conditional = T)
# Extract path coefficients
sem.coefs(alm3, SEM)
#Absence de relations entre les lemmings et les temp/prec (ces relations peuvent être enlevées sans que cela ne rende faux le schéma de piste) et SN et habitat/PP (ATTENTION ! Ici la piste entre SN et prim_prod doit être conservée même si le coefficient de piste n'est pas significativement différent de 0, sinon le schéma de piste n'est plus supporté par les données)
#pas de relation entre la temp et prec et la productivité des renards mais relation avec AO et productivité des renards

#####ALMOST #3 bis#####
#Vérification de la relation entre prod primaire et SN dans l'autre sens ==> absence de relation entre ces deux variables !
alm3b<-list(
  lme(fox_dens~lmg_abun+prim_prod+esumAO,data = SEM, random = ~1|AN, na.action = na.omit),
  lme(lmg_abun~prim_prod+esumAO,data = SEM, random = ~1|AN, na.action = na.omit),
  glmer(SN~fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), data=SEM, family=binomial(link = "logit"), na.action = na.omit),
  lme(prim_prod~esumAO+SN,data = SEM, random = ~1|AN, na.action = na.omit)
)
# Get goodness-of-fit and AIC
sem.fit(alm3b, SEM, conditional = T)
# Extract path coefficients
sem.coefs(alm3b, SEM)

#####Test de la relation renard lemming avec time lag - Utilisation de SEM2#####
#setwd(dir = "/Users/nicolas/Documents/Claire/Doc doc doc !/R analysis/Data")
#SEM2<-read.table("SEM2.txt", sep = ",",dec = ".",h=T)
#summary(SEM2)
#SEM2<-na.exclude(SEM2)

#bo<-list(
 # lme(fox_dens~lmg_abun+prim_prod+esumAO,data = SEM2, random = ~1|AN, na.action = na.exclude),
  #lme(lmg_abun~prim_prod+esumAO+MEAN_temp+MEAN_prec,data = SEM2, random = ~1|AN, na.action = na.exclude),
  #glmer(SN~prim_prod+fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), data=SEM2, family=binomial(link = "logit"), na.action = na.exclude),
  #lme(prim_prod~esumAO,data = SEM2, random = ~1|AN, na.action = na.exclude)
#)

# Get goodness-of-fit and AIC
#sem.fit(alm3, SEM, conditional = T)

#####Série de modèles "PPfox" pour expliquer la relation PP sur fox#####
#Le but ici est de trouver un parent causal commun entre fox_dens et prim_prod pour remplacer le lien direct de prim_prod sur fox_dens
#La base de la série de modèles qui va suivre est "alm3"

#PPfox<-list(
 # lme(fox_dens~lmg_abun+prim_prod+esumAO,data = SEM, random = ~1|AN, na.action = na.exclude),
  #lme(lmg_abun~prim_prod+esumAO,data = SEM, random = ~1|AN, na.action = na.exclude),
  #glmer(SN~fox_dens+MEAN_prec+MEAN_temp+prim_prod+ (1|AN), data=SEM, family=binomial(link = "logit"), na.action = na.exclude),
  #lme(prim_prod~esumAO,data = SEM, random = ~1|AN, na.action = na.exclude))

#PPfox 1 - test la "nest_dens" comme parent causal à fox_dens et prim_prod
PPfox1<-list(
lme(fox_dens~lmg_abun+prim_prod+esumAO+nest_density,data = SEM, random = ~1|AN, na.action = na.exclude),
lme(lmg_abun~prim_prod+esumAO,data = SEM, random = ~1|AN, na.action = na.exclude),
glmer(SN~fox_dens+MEAN_prec+MEAN_temp+prim_prod+ (1|AN), data=SEM, family=binomial(link = "logit"), na.action = na.exclude),
lme(prim_prod~esumAO+nest_density,data = SEM, random = ~1|AN, na.action = na.exclude),
lme(nest_density~lmg_abun,data = SEM, random = ~1|AN, na.action = na.exclude))
# Get goodness-of-fit and AIC
sem.fit(PPfox1, SEM, conditional = T)
# Extract path coefficients
sem.coefs(PPfox1, SEM)

#PPfox 2 - test la "clutch_size" comme parent causal à fox_dens et prim_prod
PPfox2<-list(
  lme(fox_dens~lmg_abun+prim_prod+esumAO+clutch_size,data = SEM, random = ~1|AN, na.action = na.exclude),
  lme(lmg_abun~prim_prod+esumAO,data = SEM, random = ~1|AN, na.action = na.exclude),
  glmer(SN~fox_dens+MEAN_prec+MEAN_temp+prim_prod+clutch_size+ (1|AN), data=SEM, family=binomial(link = "logit"), na.action = na.exclude),
  lme(prim_prod~esumAO+clutch_size,data = SEM, random = ~1|AN, na.action = na.exclude),
  lme(clutch_size~lmg_abun+MEAN_temp,data = SEM, random = ~1|AN, na.action = na.exclude))
# Get goodness-of-fit and AIC
sem.fit(PPfox2, SEM, conditional = T)
# Extract path coefficients
sem.coefs(PPfox2, SEM)

#PPfox 3 - test la "egg_abun" comme parent causal à fox_dens et prim_prod
PPfox3<-list(
  lme(fox_dens~lmg_abun+prim_prod+esumAO,data = SEM, random = ~1|AN, na.action = na.exclude),
  lme(lmg_abun~prim_prod+esumAO,data = SEM, random = ~1|AN, na.action = na.exclude),
  glmer(SN~fox_dens+MEAN_prec+MEAN_temp+esumAO+ (1|AN), data=SEM, family=binomial(link = "logit"), na.action = na.exclude),
  lme(prim_prod~esumAO+egg_abun,data = SEM, random = ~1|AN, na.action = na.exclude),
  lme(egg_abun~prim_prod,data = SEM, random = ~1|AN, na.action = na.exclude)
  )
# Get goodness-of-fit and AIC
sem.fit(PPfox3, SEM, conditional = T)
# Extract path coefficients
sem.coefs(PPfox3, SEM)
#plusieurs étapes dans ce modèle
#egg_abun parent causal de fox_dens et de prim_prod ET prim_prod parent causal de egg_abun
#egg_abun-->fox_dens et fox_dens-->SN pas différent de 0
#retrait de egg_abun comme parent causal de fox_dens
#Toujours oas de coefficient different de ) entre fox_dens et SN ...

#PPfox 4 - test la "ratio_JUVad" comme parent causal à fox_dens et prim_prod
PPfox4<-list(
  lme(fox_dens~lmg_abun+prim_prod+esumAO+ratio_JUVad,data = SEM, random = ~1|AN, na.action = na.exclude),
  lme(lmg_abun~prim_prod+esumAO,data = SEM, random = ~1|AN, na.action = na.exclude),
  glmer(SN~fox_dens+MEAN_prec+MEAN_temp+prim_prod+ratio_JUVad+ (1|AN), data=SEM, family=binomial(link = "logit"), na.action = na.exclude),
  lme(prim_prod~esumAO+ratio_JUVad,data = SEM, random = ~1|AN, na.action = na.exclude),
  lme(ratio_JUVad~lmg_abun+esumAO,data = SEM, random = ~1|AN, na.action = na.exclude)
  )
# Get goodness-of-fit and AIC
sem.fit(PPfox4, SEM, conditional = T)
# Extract path coefficients
sem.coefs(PPfox4, SEM)
#le modèle fit mais perte des relations entre SN et ratio_JUVad, prim_prod et fox_dens

#PPfox 5 - test la "brood_size" comme parent causal à fox_dens et prim_prod
PPfox5<-list(
  lme(fox_dens~lmg_abun+prim_prod+esumAO+brood_size,data = SEM, random = ~1|AN, na.action = na.exclude),
  lme(lmg_abun~prim_prod+esumAO,data = SEM, random = ~1|AN, na.action = na.exclude),
  glmer(SN~fox_dens+MEAN_prec+MEAN_temp+prim_prod+brood_size+ (1|AN), data=SEM, family=binomial(link = "logit"), na.action = na.exclude),
  lme(prim_prod~esumAO+brood_size,data = SEM, random = ~1|AN, na.action = na.exclude),
  lme(brood_size~lmg_abun+esumAO,data = SEM, random = ~1|AN, na.action = na.exclude)
  )
# Get goodness-of-fit and AIC
sem.fit(PPfox5, SEM, conditional = T)
# Extract path coefficients
sem.coefs(PPfox5, SEM)
#here I'm loosing the links between SN and fox_dens / prim_prod

#PPfox 6 - test la "nest_succ" comme parent causal à fox_dens et prim_prod
PPfox6<-list(
  lme(fox_dens~lmg_abun+prim_prod+esumAO+nest_succ,data = SEM, random = ~1|AN, na.action = na.exclude),
  lme(lmg_abun~prim_prod+esumAO,data = SEM, random = ~1|AN, na.action = na.exclude),
  glmer(SN~fox_dens+MEAN_prec+MEAN_temp+prim_prod+ (1|AN), data=SEM, family=binomial(link = "logit"), na.action = na.exclude),
  lme(prim_prod~esumAO+nest_succ,data = SEM, random = ~1|AN, na.action = na.exclude),
  lme(nest_succ~SN+lmg_abun+MEAN_temp+MEAN_prec+esumAO,data = SEM, random = ~1|AN, na.action = na.exclude)
  )
# Get goodness-of-fit and AIC
sem.fit(PPfox6, SEM, conditional = T)
# Extract path coefficients
sem.coefs(PPfox6, SEM)
#here the link between fox_dens and prim_prod DISAPPEAR !!!!

#####-------------------------SEM-V2 data-------------------------#####

setwd(dir = "/Users/nicolas/Documents/Claire/Doc doc doc !/R analysis/Data")
load("SEMv2.Rdata")#jeux de données rectifié avec les estimations des dates d'initiation avec code de 1 à 3
load("SEMv2_1.Rdata")#jeux de données rectifié avec les estimations des dates d'initiation code 1 et 3
load("SEMv2_2.Rdata")#jeux de données rectifié avec les estimations des dates d'initiation code 1 - normalement même jeu de données que SEM
summary(nest)#réfère à SEMv2.RData
summary(nest1)#réfère à SEMv2_1.RData
summary(nest2)#réfère à SEMv2_2.RData

#traitement des NAs
nest<-na.omit(nest)
nest1<-na.omit(nest1)
nest2<-na.omit(nest2)

#Changement du nom de la variable ISSUE en SN
nest$SN<-nest$ISSUE
nest1$SN<-nest1$ISSUE
nest2$SN<-nest2$ISSUE
#Package nécessaires
require(nlme)
require(lme4)
require(piecewiseSEM)
#####ALMOST #1#####
alm1<-list(
  lme(fox_dens~lmg_abun+prim_prod+MEAN_temp,data = nest1, random = ~1|AN),
  glmer(SN~prim_prod+fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), data=nest1, family=binomial(link = "logit"))
)
# Get goodness-of-fit and AIC
sem.fit(alm1, nest1, conditional = T)
# Extract path coefficients
sem.coefs(alm1, nest1)
#ajout du lien fox_dens - MEAN_temp

#####ALMOST #3#####
alm3<-list(
  lme(fox_dens~lmg_abun+prim_prod+esumAO+MEAN_temp,data = nest1, random = ~1|AN),
  lme(lmg_abun~prim_prod+esumAO+MEAN_temp+MEAN_prec,data = nest1, random = ~1|AN),
  glmer(SN~prim_prod+fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), data=nest1, family=binomial(link = "logit")),
  lme(prim_prod~esumAO+MEAN_temp,data = nest1, random = ~1|AN)
)

# Get goodness-of-fit and AIC
sem.fit(alm3, nest1, conditional = T)
# Extract path coefficients
sem.coefs(alm3, nest1)
#ajout du lien fox_dens - MEAN_temp
#ajout du lien prim_prod - MEAN_temp

####PPfox 1 - test la "nest_dens" comme parent causal à fox_dens et prim_prod
PPfox1<-list(
  lme(fox_dens~lmg_abun+prim_prod+esumAO+MEAN_temp+nest_density,data = nest1, random = ~1|AN, na.action = na.exclude),
  lme(lmg_abun~prim_prod+esumAO,data = nest1, random = ~1|AN, na.action = na.exclude),
  glmer(SN~fox_dens+MEAN_prec+MEAN_temp+prim_prod+ (1|AN), data=nest1, family=binomial(link = "logit"), na.action = na.exclude),
  lme(prim_prod~esumAO+MEAN_temp+nest_density,data = nest1, random = ~1|AN, na.action = na.exclude),
  lme(nest_density~lmg_abun+esumAO,data = nest1, random = ~1|AN, na.action = na.exclude))
# Get goodness-of-fit and AIC
sem.fit(PPfox1, nest1, conditional = T)
# Extract path coefficients
sem.coefs(PPfox1, nest1)

####PPfox 2 - test la "clutch_size" comme parent causal à fox_dens et prim_prod
PPfox2<-list(
  lme(fox_dens~lmg_abun+prim_prod+esumAO+MEAN_temp+clutch_size,data = nest1, random = ~1|AN),
  lme(lmg_abun~prim_prod+esumAO,data = nest1, random = ~1|AN),
  glmer(SN~fox_dens+MEAN_prec+MEAN_temp+prim_prod+clutch_size+ (1|AN), data=nest1, family=binomial(link = "logit")),
  lme(prim_prod~esumAO+clutch_size+MEAN_temp,data = nest1, random = ~1|AN),
  lme(clutch_size~lmg_abun+MEAN_temp+esumAO,data = nest1, random = ~1|AN))
# Get goodness-of-fit and AIC
sem.fit(PPfox2, nest1, conditional = T)
# Extract path coefficients
sem.coefs(PPfox2, nest1)
#ajout du lien fox_dens - MEAN_temp
#ajout du lien prim_prod - MEAN_temp
#ajout du lien clutch_size - esumAO
#---> pas intéressant, perte de toutes les relations avec le SN

####PPfox 6 - test la "nest_succ" comme parent causal à fox_dens et prim_prod
PPfox6<-list(
  lme(fox_dens~lmg_abun+prim_prod+esumAO+MEAN_temp+nest_succ,data = nest1, random = ~1|AN),
  lme(lmg_abun~prim_prod+esumAO,data = nest1, random = ~1|AN),
  glmer(SN~fox_dens+MEAN_prec+MEAN_temp+prim_prod+ (1|AN), data=nest1, family=binomial(link = "logit")),
  lme(prim_prod~esumAO+MEAN_temp+nest_succ,data = nest1, random = ~1|AN),
  lme(nest_succ~SN+lmg_abun+MEAN_temp+MEAN_prec+fox_dens+esumAO,data = nest1, random = ~1|AN)
)
# Get goodness-of-fit and AIC
sem.fit(PPfox6, nest1, conditional = T)
# Extract path coefficients
sem.coefs(PPfox6, nest1)
#ici ajout du lien nest_succ - fox_dens sans que la première analyse me donne cette piste manquante. Cependant, après l'avoir ajouter malgré tout, le coefficient de piste est significatif
