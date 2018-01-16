#####Analyse de piste - données Ph.D.#####
setwd(dir = "/Users/nicolas/Documents/Claire/Doc doc doc !/R analysis/Data")
SEM<-read.table("Path analysis_data 3.txt", sep = ",", dec = ".", h=T)
summary(SEM)


#Traitement des NAs
#Nécessaire d'oter toutes les lignes comportant des NAs adfin que les modèles soient
#tous effectués avec le même jeu de données
#which(is.na(SEM$SN)) #localisation of NAs in the vector
SEM<-na.omit(SEM) 

#####piecewiseSEM package method - Ph.D. data##### 
library(piecewiseSEM)
# Load model packages
library(lme4)
library(nlme)
#save(SEM, file = "SEM.RData")


###### Causal path #1 #####
#Causal model #1
test1<-list(
  lme(fox_dens~lmg_abun, random = ~1|AN, na.action = na.omit, data = SEM),
  lme(prim_prod~MEAN_temp+MEAN_prec, random = ~1|AN, na.action = na.omit, data = SEM),
  glmer(SN~fox_dens+prim_prod+HABITAT+MEAN_temp+MEAN_prec + (1|AN), family=binomial(link = "logit"), na.action = na.omit, data = SEM)
)
# Get goodness-of-fit and AIC
sem.fit(test1, SEM, conditional = T)
#Lien prod prim - lmg et prod prim - fox. Un lien de prod prim vers lemming pourrait expliquer ces deux resultats (cf Causal model #1 bis)

#Causal model #1 bis avec relation entre productivité primaire et abondance de lemmings
test1<-list(
  lme(fox_dens~lmg_abun, random = ~1|AN, na.action = na.omit, data = SEM),
  lme(prim_prod~MEAN_temp+MEAN_prec, random = ~1|AN, na.action = na.omit, data = SEM),
  lme(lmg_abun~prim_prod, random = ~1|AN, na.action = na.omit, data = SEM),
  glmer(SN~fox_dens+prim_prod+HABITAT+MEAN_temp+MEAN_prec + (1|AN), family=binomial(link = "logit"), na.action = na.omit, data = SEM)
)
# Get goodness-of-fit and AIC
sem.fit(test1, SEM, conditional = T)
#Relation renard - productivité primaire non expliquée par le lien entre la productivité primaire - primaire
#Une autre relation doit être trouver : AO ? Effet de la temp sur les renards plus tôt dans la saison

#Causal model #2
test2<-list(
  lme(fox_dens~lmg_abun, random = ~1|AN, na.action = na.omit),
  lme(prim_prod~HABITAT+MEAN_temp+MAX_prec, random = ~1|AN, na.action = na.omit),
  glmer(SN~fox_dens+prim_prod+HABITAT+MEAN_temp+MAX_prec + (1|AN), family=binomial(link = "logit"))
)
# Get goodness-of-fit and AIC
sem.fit(test2, SEM, conditional = T)
# Extract path coefficients
sem.coefs(test2, SEM)

#Causal model #3
test3<-list(
  lme(fox_dens~lmg_abun, random = ~1|AN, na.action = na.omit),
  lme(prim_prod~HABITAT+MAX_temp+MEAN_prec, random = ~1|AN, na.action = na.omit),
  glmer(SN~fox_dens+prim_prod+HABITAT+MAX_temp+MEAN_prec + (1|AN), family=binomial(link = "logit"))
)
# Get goodness-of-fit and AIC
sem.fit(test3, SEM, conditional = T)
# Extract path coefficients
sem.coefs(test3, SEM)

#Causal model #4
test4<-list(
  lme(fox_dens~lmg_abun, random = ~1|AN, na.action = na.omit),
  lme(prim_prod~HABITAT+MAX_temp+MAX_prec, random = ~1|AN, na.action = na.omit),
  glmer(SN~fox_dens+prim_prod+HABITAT+MAX_temp+MAX_prec + (1|AN), family=binomial(link = "logit"))
)
# Get goodness-of-fit and AIC
sem.fit(test4, SEM, conditional = T)
# Extract path coefficients
sem.coefs(test4, SEM)

#Causal model #5
test5<-list(
  lme(fox_dens~lmg_abun, random = ~1|AN, na.action = na.omit),
  lme(prim_prod~HABITAT+MIN_temp+MEAN_prec, random = ~1|AN, na.action = na.omit),
  glmer(SN~fox_dens+prim_prod+HABITAT+MIN_temp+MEAN_prec + (1|AN), family=binomial(link = "logit"))
)
# Get goodness-of-fit and AIC
sem.fit(test5, SEM, conditional = T)
# Extract path coefficients
sem.coefs(test5, SEM)

#Causal model #6
test6<-list(
  lme(fox_dens~lmg_abun, random = ~1|AN, na.action = na.omit),
  lme(prim_prod~HABITAT+MIN_temp+MAX_prec, random = ~1|AN, na.action = na.omit),
  glmer(SN~fox_dens+prim_prod+HABITAT+MIN_temp+MAX_prec + (1|AN), family=binomial(link = "logit"))
)
# Get goodness-of-fit and AIC
sem.fit(test6, SEM, conditional = T)
# Extract path coefficients
sem.coefs(test6, SEM)

#####Causal path #11 #####
#Causal model #1
test<-list(
  lme(fox_dens~lmg_abun, random = ~1|AN, na.action = na.omit),
  #glmer(SN~fox_dens+HABITAT+lmg_abun+prim_prod+MEAN_prec + (1|AN), family=binomial(link = "logit")),
  glmer(SN~prim_prod+fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), family=binomial(link = "logit"))
)
# Get goodness-of-fit and AIC
sem.fit(test, SEM, conditional = T)
# Extract path coefficients
sem.coefs(test, SEM)

#####Causal path #16 #####
#avec températures mean
test_MEAN<-list(
  lme(fox_dens~lmg_abun, random = ~1|AN, na.action = na.omit),
  lme(prim_prod~HABITAT+MEAN_temp+MEAN_prec, random = ~1|AN, na.action = na.omit),
  glmer(SN~fox_dens+prim_prod+HABITAT+ (1|AN), family=binomial(link = "logit"))
)
# Get goodness-of-fit and AIC
sem.fit(test_MEAN, SEM, conditional = T)
# Extract path coefficients
sem.coefs(test_MEAN, SEM)

#Modèle #16 avec températures max
test_MAX<-list(
  lme(fox_dens~lmg_abun, random = ~1|AN, na.action = na.omit),
  lme(prim_prod~HABITAT+MAX_temp+MAX_prec, random = ~1|AN, na.action = na.omit),
  glmer(SN~fox_dens+prim_prod+HABITAT+ (1|AN), family=binomial(link = "logit"))
)
# Get goodness-of-fit and AIC
sem.fit(test_MAX, SEM, conditional = T)
# Extract path coefficients
sem.coefs(test_MAX, SEM)


#save(SEM, file = "SEM.RData")
#load()

summary(SEM)
summary(SEM2)
#####ALMOST #1#####
alm1<-list(
  lme(fox_dens~lmg_abun+prim_prod,data = SEM, random = ~1|AN, na.action = na.omit),
  glmer(SN~prim_prod+fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), data=SEM, family=binomial(link = "logit"), na.action = na.omit)
)
# Get goodness-of-fit and AIC
sem.fit(alm1, SEM, conditional = T)
# Extract path coefficients
sem.coefs(alm1, SEM)
#modèle plausible mais pas bon car pas de relation entre le SN et les renards ! Dépend du jeux de données (SEM ou SEM2)

#####ALMOST #2#####
#integration de l'indice d'AO pour expliquer relation fox - PP (sans le lien direct fox - PP)
alm2<-list(
  lme(fox_dens~lmg_abun+esumAO,data = SEM2, random = ~1|AN, na.action = na.omit),
  glmer(SN~prim_prod+fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), data=SEM2, family=binomial(link = "logit"), na.action = na.omit),
  lme(prim_prod~esumAO,data = SEM2, random = ~1|AN, na.action = na.omit)
)
# Get goodness-of-fit and AIC
sem.fit(alm2, SEM2, conditional = T)
# Extract path coefficients
sem.coefs(alm2, SEM2)

#####ALMOST #2 bis#####
#idem mais ici j'ai gardé le lien direct fox - PP ==> coeff de piste plus intéressants !
alm2b<-list(
  lme(fox_dens~lmg_abun+prim_prod+esumAO,data = SEM2, random = ~1|AN, na.action = na.omit),
  glmer(SN~prim_prod+fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), data=SEM2, family=binomial(link = "logit"), na.action = na.omit),
  lme(prim_prod~esumAO,data = SEM2, random = ~1|AN, na.action = na.omit)
)
# Get goodness-of-fit and AIC
sem.fit(alm2b, SEM2, conditional = T)
# Extract path coefficients
sem.coefs(alm2b, SEM2)

#####ALMOST #3#####
#idem mais ici j'ai gardé le lien direct fox - PP ==> coeff de piste plus intéressants !
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
#si j'utilise SEM, le modèle devient bon si j'ajoute le température moyenne aux lemmings 
#mais toujours pas de relation entre le SN et la productivité des renards

alm3<-list(
  lme(fox_dens~lmg_abun+prim_prod+esumAO,data = SEM2, random = ~1|AN, na.action = na.omit),
  lme(lmg_abun~prim_prod+esumAO,data = SEM2, random = ~1|AN, na.action = na.omit),
  glmer(SN~prim_prod+fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), data=SEM2, family=binomial(link = "logit"), na.action = na.omit),
  lme(prim_prod~esumAO,data = SEM2, random = ~1|AN, na.action = na.omit)
)

# Get goodness-of-fit and AIC
sem.fit(alm3, SEM2, conditional = T)
# Extract path coefficients
sem.coefs(alm3, SEM2)
#Avec SEM2 pas de lien entre les lemmings et la température, ni entre SN et habitat et PP

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

#####ALMOST #4#####
#idem mais AO remplace temp et precipitation pour l'influence sur le SN
alm4<-list(
  lme(fox_dens~lmg_abun+prim_prod+esumAO,data = SEM, random = ~1|AN, na.action = na.omit),
  lme(lmg_abun~prim_prod+esumAO,data = SEM, random = ~1|AN, na.action = na.omit),
  glmer(SN~prim_prod+fox_dens+HABITAT+esumAO+ (1|AN), data=SEM, family=binomial(link = "logit"), na.action = na.omit),
  lme(prim_prod~esumAO,data = SEM, random = ~1|AN, na.action = na.omit)
)
# Get goodness-of-fit and AIC
sem.fit(alm4, SEM, conditional = T)
# Extract path coefficients
sem.coefs(alm4, SEM) #### pas bon modèle !
#####BROUILLON#####

#Modèle #1 avec températures maximum
test_MAX<-list(
  lme(fox_dens~lmg_abun, random = ~1|AN, na.action = na.omit),
  lme(lmg_abun~prim_prod+MEAN_prec+MEAN_temp+HABITAT, random = ~1|AN, na.action = na.omit),
  lme(fox_dens~prim_prod+MEAN_temp+MEAN_prec+HABITAT+lmg_abun, random = ~1|AN, na.action = na.omit),
  glmer(SN~fox_dens+prim_prod+HABITAT+MEAN_temp+MEAN_prec+lmg_abun + (1|AN), family=binomial(link = "logit"))
)
# Get goodness-of-fit and AIC
sem.fit(test_MAX, SEM, conditional = T)
# Extract path coefficients
sem.coefs(test_MAX, SEM)

test0<-list(
  lme(fox_dens~lmg_abun+prim_prod, random = ~1|AN, na.action = na.omit),
  lme(lmg_abun~prim_prod+MEAN_prec+MEAN_temp+HABITAT, random = ~1|AN, na.action = na.omit),
  lme(fox_dens~prim_prod+MEAN_temp+MEAN_prec+HABITAT+lmg_abun, random = ~1|AN, na.action = na.omit),
  glmer(SN~fox_dens+prim_prod+HABITAT+MEAN_temp+MEAN_prec+lmg_abun + (1|AN), family=binomial(link = "logit"))
)
# Get goodness-of-fit and AIC
sem.fit(test0, SEM, conditional = T)


test<-list(
  lme(fox_dens~lmg_abun+prim_prod, random = ~1|AN, na.action = na.omit),
  glmer(SN~fox_dens+HABITAT+lmg_abun+prim_prod+MEAN_prec + (1|AN), family=binomial(link = "logit")),
  glmer(SN~prim_prod+fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), family=binomial(link = "logit")),
  glmer(SN~HABITAT+ (1|AN), family=binomial(link = "logit")),
  glmer(SN~MEAN_prec+ (1|AN), family=binomial(link = "logit")),
  lme(prim_prod~HABITAT, random = ~1|AN, na.action = na.omit),
  lme(prim_prod~MEAN_prec, random = ~1|AN, na.action = na.omit),
  lme(prim_prod~MEAN_temp, random = ~1|AN, na.action = na.omit)
)
# Get goodness-of-fit and AIC
sem.fit(test, SEM, conditional = T)
# Extract path coefficients
sem.coefs(test, SEM)
