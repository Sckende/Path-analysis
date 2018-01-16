#####-------------------------PATH ANALYSIS PhD v3-------------------------#####

rm(list = ls()) #clean R memory

setwd(dir = "/Users/nicolas/Documents/Claire/Doc doc doc !/R analysis/Data")
nini<-read.table("Path analysis_data 3.txt", h=T, dec=".", sep = ",")

summary(nini)

############ NEST - Données avec les valeurs de procuctivité primaire #################
#traitement des NAs
nest<-na.omit(nini)

############ NEST 1 - Données avec les valeurs de DPN (Date de Pic de Nitrogène) basées sur 50NDVI ####################
#The best equation to predict the date of peak nitrogen is: DPN = -10.1 + 1.07*date 50% NDVI (95%CI of slope: 0.65, 1.48).

nini<-read.table("Path analysis_data 3.txt", h=T, dec=".", sep = ",")
NDVI<-read.csv("VEG-NDVI 1991-2010_Doiron.txt", sep = "\t", dec = ".")
summary(NDVI)

m<-unique(nini$ID)
date_NDVI50<-NULL

for(n in m){
  d<-nini$AN[nini$ID == n]
  NDVI50<-NDVI$Date_50NDVI[NDVI$year == d]
  medianDate_Hatch<-NDVI$Median_Hatch[NDVI$year == d]
  DPN<-(-10.1 + 1.07*NDVI50)
  
  s<-data.frame(NDVI50,medianDate_Hatch,DPN)
  date_NDVI50<-rbind(date_NDVI50,s)
}

nest1<-cbind(nini,date_NDVI50)
#traitement des NAs
nest1<-na.omit(nest1)

summary(nest1)




#Package nécessaires
require(nlme)
require(lme4)
require(piecewiseSEM)
require(ggm)



###### Modèles basés sur "nest" de 1998 à 2016 ########
##### Modèle 1 #####
M1<-list(
  lme(fox_dens~lmg_abun, data = nest, random = ~1|AN),
  glmer(SN~prim_prod+fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lme(prim_prod~MEAN_prec+MEAN_temp, data = nest, random = ~1|AN),
  lme(MEAN_temp~sprAO, data = nest, random = ~1|AN),
  lme(MEAN_prec~sprAO, data = nest, random = ~1|AN))

# Get goodness-of-fit and AIC
sem.fit(M1, nest, conditional = T)
#Significative missing paths
#prim_prod ~ lmg_abun + MEAN_prec + MEAN_temp  -2.9204    0.3742   17    -7.8053  0.0000 ***
#fox_dens ~ sprAO + lmg_abun -25.8862    0.5801   16   -44.6239  0.0000 ***
#prim_prod ~ sprAO + MEAN_prec + MEAN_temp  -3.1854    1.3225   17    -2.4086  0.0276   *
#MEAN_prec ~ fox_dens + lmg_abun + sprAO  -0.0255    0.0092   15    -2.7786  0.0141   *
#prim_prod ~ fox_dens + lmg_abun + MEAN_prec + MEAN_temp  -0.2868    0.0328   16    -8.7484  0.0000 ***
#MEAN_temp ~ MEAN_prec + sprAO   0.1062    0.0380 2641     2.7927  0.0053  **

##### Modèle 1-a #####
#Ajout du lien (prim_prod --> lmg_abun)
M1a<-list(
  lme(fox_dens~lmg_abun, data = nest, random = ~1|AN),
  glmer(SN~prim_prod+fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lme(prim_prod~MEAN_prec+MEAN_temp, data = nest, random = ~1|AN),
  lme(MEAN_temp~sprAO, data = nest, random = ~1|AN),
  lme(MEAN_prec~sprAO, data = nest, random = ~1|AN),
  lme(lmg_abun~prim_prod, data = nest, random = ~1|AN))

# Get goodness-of-fit and AIC
sem.fit(M1a, nest, conditional = T)

#Significative missing paths
#fox_dens ~ sprAO + lmg_abun -25.8862    0.5801   16   -44.6239  0.0000 ***
#lmg_abun ~ sprAO + prim_prod  -0.3377    0.0668   16    -5.0522  0.0001 ***
#prim_prod ~ sprAO + MEAN_prec + MEAN_temp  -3.1854    1.3225   17    -2.4086  0.0276   *
#MEAN_temp ~ MEAN_prec + sprAO   0.1062    0.0380 2641     2.7927  0.0053  **
#fox_dens ~ MEAN_temp + sprAO + lmg_abun   0.0000    0.0000 2641    -2.7068  0.0068  **
#prim_prod ~ fox_dens + lmg_abun + MEAN_prec + MEAN_temp  -0.2868    0.0328   16    -8.7484  0.0000 ***

##### Modèle 1-b #####
#Ajout du lien (prim_prod <-- sprAO)
M1b<-list(
  lme(fox_dens~lmg_abun, data = nest, random = ~1|AN),
  glmer(SN~prim_prod+fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lme(prim_prod~MEAN_prec+MEAN_temp+sprAO, data = nest, random = ~1|AN),
  lme(MEAN_temp~sprAO, data = nest, random = ~1|AN),
  lme(MEAN_prec~sprAO, data = nest, random = ~1|AN),
  lme(lmg_abun~prim_prod, data = nest, random = ~1|AN))

# Get goodness-of-fit and AIC
sem.fit(M1b, nest, conditional = T)

#Significative missing paths
#fox_dens ~ sprAO + lmg_abun -25.8862    0.5801   16   -44.6239  0.0000 ***
#lmg_abun ~ sprAO + prim_prod  -0.3377    0.0668   16    -5.0522  0.0001 ***
#MEAN_temp ~ MEAN_prec + sprAO   0.1062    0.0380 2641     2.7927  0.0053  **
#fox_dens ~ MEAN_temp + sprAO + lmg_abun   0.0000    0.0000 2641    -2.7068  0.0068  **
#prim_prod ~ fox_dens + lmg_abun + sprAO + MEAN_prec + MEAN_temp  -0.6194    0.0421   15   -14.7037  0.0000 ***

##### Modèle 1-c #####
#Ajout du lien (fox_dens<-- sprAO)
M1c<-list(
  lme(fox_dens~lmg_abun+sprAO, data = nest, random = ~1|AN),
  glmer(SN~prim_prod+fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lme(prim_prod~MEAN_prec+MEAN_temp+sprAO, data = nest, random = ~1|AN),
  lme(MEAN_temp~sprAO, data = nest, random = ~1|AN),
  lme(MEAN_prec~sprAO, data = nest, random = ~1|AN),
  lme(lmg_abun~prim_prod, data = nest, random = ~1|AN))

# Get goodness-of-fit and AIC
sem.fit(M1c, nest, conditional = T)

#Significative missing paths
#lmg_abun ~ sprAO + prim_prod  -0.3377    0.0668   16    -5.0522  0.0001 ***
#MEAN_temp ~ MEAN_prec + sprAO   0.1062    0.0380 2641     2.7927  0.0053  **
#fox_dens ~ MEAN_temp + sprAO + lmg_abun   0.0000    0.0000 2641    -2.7068  0.0068  **
#prim_prod ~ fox_dens + sprAO + lmg_abun + MEAN_prec + MEAN_temp  -0.6194    0.0421   15   -14.7037  0.0000 ***

##### Modèle 1-d #####
#Ajout du lien (lmg_abun<-- sprAO)
M1d<-list(
  lme(fox_dens~lmg_abun+sprAO, data = nest, random = ~1|AN),
  glmer(SN~prim_prod+fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lme(prim_prod~MEAN_prec+MEAN_temp+sprAO, data = nest, random = ~1|AN),
  lme(MEAN_temp~sprAO, data = nest, random = ~1|AN),
  lme(MEAN_prec~sprAO, data = nest, random = ~1|AN),
  lme(lmg_abun~prim_prod+sprAO, data = nest, random = ~1|AN))

# Get goodness-of-fit and AIC
sem.fit(M1d, nest, conditional = T)

#Significative missing paths
#MEAN_temp ~ MEAN_prec + sprAO   0.1062    0.0380 2641     2.7927  0.0053  **
#fox_dens ~ MEAN_temp + sprAO + lmg_abun   0.0000    0.0000 2641    -2.7068  0.0068  **
#prim_prod ~ fox_dens + sprAO + lmg_abun + MEAN_prec + MEAN_temp  -0.6194    0.0421   15   -14.7037  0.0000 ***

##### Modèle 1-e #####
#Ajout du lien (MEAN_temp<-- MEAN_prec)
M1e<-list(
  lme(fox_dens~lmg_abun+sprAO, data = nest, random = ~1|AN),
  glmer(SN~prim_prod+fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lme(prim_prod~MEAN_prec+MEAN_temp+sprAO, data = nest, random = ~1|AN),
  lme(MEAN_temp~sprAO+MEAN_prec, data = nest, random = ~1|AN),
  lme(MEAN_prec~sprAO, data = nest, random = ~1|AN),
  lme(lmg_abun~prim_prod+sprAO, data = nest, random = ~1|AN))

# Get goodness-of-fit and AIC
sem.fit(M1e, nest, conditional = T)
#Significative missing paths
#prim_prod ~ fox_dens + sprAO + lmg_abun + MEAN_prec + MEAN_temp  -0.6194    0.0421   15   -14.7037  0.0000 ***

##### Modèle 1-f **** #####
#Ajout du lien (prim_prod --> fox_dens)
M1f<-list(
  lme(fox_dens~lmg_abun+sprAO+prim_prod, data = nest, random = ~1|AN),
  glmer(SN~prim_prod+fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lme(prim_prod~MEAN_prec+MEAN_temp+sprAO, data = nest, random = ~1|AN),
  lme(MEAN_temp~sprAO+MEAN_prec, data = nest, random = ~1|AN),
  lme(MEAN_prec~sprAO, data = nest, random = ~1|AN),
  lme(lmg_abun~prim_prod+sprAO, data = nest, random = ~1|AN))

# Get goodness-of-fit and AIC
sem.fit(M1f, nest, conditional = T)
# Extract path coefficients
sem.coefs(M1f, nest)

#Graph drawing
M1fGRAPH<-DAG(
  fox_dens~lmg_abun+sprAO+prim_prod,
  SN~prim_prod+fox_dens+HABITAT+MEAN_prec+MEAN_temp,
  prim_prod~MEAN_prec+MEAN_temp+sprAO,
  MEAN_temp~sprAO+MEAN_prec,
  MEAN_prec~sprAO,
  lmg_abun~prim_prod+sprAO
)
drawGraph(M1fGRAPH, adjust = T)

###################### Retrait de variables à P-value forte à partir du modèle obtenu#################### 
##### Modèle 1-f-1 #####
#Retrait de la piste avec la P-value la plus forte (SN -- HABITAT)
M1f1<-list(
  lme(fox_dens~lmg_abun+sprAO+prim_prod, data = nest, random = ~1|AN),
  glmer(SN~prim_prod+fox_dens+MEAN_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lme(prim_prod~MEAN_prec+MEAN_temp+sprAO, data = nest, random = ~1|AN),
  lme(MEAN_temp~sprAO+MEAN_prec, data = nest, random = ~1|AN),
  lme(MEAN_prec~sprAO, data = nest, random = ~1|AN),
  lme(lmg_abun~prim_prod+sprAO, data = nest, random = ~1|AN))

# Get goodness-of-fit and AIC
sem.fit(M1f1, nest, conditional = T)
# Extract path coefficients
sem.coefs(M1f1, nest)
#No significative missing paths

##### Modèle 1-f-2 #####
#Retrait de la piste avec la P-value la plus forte (sprAO -- MEAN_temp)
M1f2<-list(
  lme(fox_dens~lmg_abun+sprAO+prim_prod, data = nest, random = ~1|AN),
  glmer(SN~prim_prod+fox_dens+MEAN_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lme(prim_prod~MEAN_prec+MEAN_temp+sprAO, data = nest, random = ~1|AN),
  lme(MEAN_temp~MEAN_prec, data = nest, random = ~1|AN),
  lme(MEAN_prec~sprAO, data = nest, random = ~1|AN),
  lme(lmg_abun~prim_prod+sprAO, data = nest, random = ~1|AN))

# Get goodness-of-fit and AIC
sem.fit(M1f2, nest, conditional = T)
# Extract path coefficients
sem.coefs(M1f2, nest)
#No significative missing paths


##### Modèle 1-f-3 #####
#Ajout de la piste (sprAO -- MEAN_temp)
#Retrait de la piste avec la P-value la plus forte (sprAO -- MEAN_prec)
M1f3<-list(
  lme(fox_dens~lmg_abun+sprAO+prim_prod, data = nest, random = ~1|AN),
  glmer(SN~prim_prod+fox_dens+MEAN_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lme(prim_prod~MEAN_prec+MEAN_temp+sprAO, data = nest, random = ~1|AN),
  lme(MEAN_temp~MEAN_prec, data = nest, random = ~1|AN),
  lme(lmg_abun~prim_prod+sprAO, data = nest, random = ~1|AN))

# Get goodness-of-fit and AIC
sem.fit(M1f3, nest, conditional = T)
# Extract path coefficients
sem.coefs(M1f3, nest)
#Significative missing paths
#fox_dens ~ MEAN_temp + MEAN_prec + sprAO + lmg_abun + prim_prod   0.0000    0.0000 2640    -2.4820  0.0131 *

##### Modèle 1-f-4 **** #####
#Ajout de la piste (MEAN_temp --> fox_dens)
M1f4<-list(
  lme(fox_dens~lmg_abun+sprAO+MEAN_temp+prim_prod, data = nest, random = ~1|AN),
  glmer(SN~prim_prod+fox_dens+MEAN_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lme(prim_prod~MEAN_prec+MEAN_temp+sprAO, data = nest, random = ~1|AN),
  lme(MEAN_temp~MEAN_prec, data = nest, random = ~1|AN),
  lme(lmg_abun~prim_prod+sprAO, data = nest, random = ~1|AN))

# Get goodness-of-fit and AIC
sem.fit(M1f4, nest, conditional = T)
# Extract path coefficients
sem.coefs(M1f4, nest)
#NO Significative missing paths

##### Modèle 1-f-5 #####
#Retrait de la piste ( MEAN_temp --> prim_prod)
M1f5<-list(
  lme(fox_dens~lmg_abun+sprAO+MEAN_temp+prim_prod, data = nest, random = ~1|AN),
  glmer(SN~prim_prod+fox_dens+MEAN_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lme(prim_prod~MEAN_prec+sprAO, data = nest, random = ~1|AN),
  lme(MEAN_temp~MEAN_prec, data = nest, random = ~1|AN),
  lme(lmg_abun~prim_prod+sprAO, data = nest, random = ~1|AN))

# Get goodness-of-fit and AIC
sem.fit(M1f5, nest, conditional = T)
# Extract path coefficients
sem.coefs(M1f5, nest)
#NO Significative missing paths

##### Modèle 1-f-6 **** #####
#Ajout de la piste ( prim_prod <-- MEAN_prec)
M1f6<-list(
  lme(fox_dens~lmg_abun+sprAO+MEAN_temp+prim_prod, data = nest, random = ~1|AN),
  glmer(SN~prim_prod+fox_dens+MEAN_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lme(prim_prod~sprAO, data = nest, random = ~1|AN),
  lme(MEAN_temp~MEAN_prec, data = nest, random = ~1|AN),
  lme(lmg_abun~prim_prod+sprAO, data = nest, random = ~1|AN))

# Get goodness-of-fit and AIC
sem.fit(M1f6, nest, conditional = T)
# Extract path coefficients
sem.coefs(M1f6, nest)
#NO Significative missing paths

#Graph drawing
M1f6GRAPH<-DAG(
  fox_dens~lmg_abun+sprAO+MEAN_temp+prim_prod,
  SN~prim_prod+fox_dens+MEAN_prec+MEAN_temp,
  prim_prod~sprAO,
  MEAN_temp~MEAN_prec,
  lmg_abun~prim_prod+sprAO
)
drawGraph(M1f6GRAPH, adjust = T)

  ##### Modèle 3 ######
  # Modèles avec température minimale printanière et early summer rainfall
  summary(nest)
  
  M3<-list(
    lme(fox_dens~lmg_abun+SPR_min+eSUM_moy_prec, data = nest, random = ~1|AN),
    glmer(SN~prim_prod+fox_dens+HABITAT+eSUM_moy_prec+SPR_min+ (1|AN), data=nest, family=binomial(link = "logit")),
    lme(prim_prod~eSUM_moy_prec+SPR_min, data = nest, random = ~1|AN),
    lme(SPR_min~sprAO, data = nest, random = ~1|AN),
    lme(eSUM_moy_prec~sprAO, data = nest, random = ~1|AN))
  
  # Get goodness-of-fit and AIC
  semM3<-sem.fit(M3, nest, conditional = T)
  semM3$missing.paths
  semM3$Fisher.C
  semM3$AIC
  # Significative missing paths
  #SPR_min ~ lmg_abun + sprAO  -1.3829    0.0829   16   -16.6907  0.0000 ***
  #eSUM_moy_prec ~ lmg_abun + sprAO  -0.3736    0.0184   16   -20.3492  0.0000 ***
  #prim_prod ~ lmg_abun + SPR_min + eSUM_moy_prec  -1.9843    0.3601   15    -5.5107  0.0001 ***
  #fox_dens ~ sprAO + lmg_abun + SPR_min + eSUM_moy_prec -22.0248    0.6177   14   -35.6549  0.0000 ***
  #prim_prod ~ sprAO + SPR_min + eSUM_moy_prec  -4.7081    1.3533   15    -3.4789  0.0034  **
  #eSUM_moy_prec ~ SPR_min + sprAO   0.0635    0.0042   16    15.0649  0.0000 ***
  
  ##### Modèle 3 - a ######
  # Ajout de la piste (SPR_min --> lmg_abun) 
  M3a<-list(
    lme(fox_dens~lmg_abun+SPR_min+eSUM_moy_prec, data = nest, random = ~1|AN),
    glmer(SN~prim_prod+fox_dens+HABITAT+eSUM_moy_prec+SPR_min+ (1|AN), data=nest, family=binomial(link = "logit")),
    lme(prim_prod~eSUM_moy_prec+SPR_min, data = nest, random = ~1|AN),
    lme(SPR_min~sprAO, data = nest, random = ~1|AN),
    lme(eSUM_moy_prec~sprAO, data = nest, random = ~1|AN),
    lme(lmg_abun~SPR_min, data = nest, random = ~1|AN))
  
  # Get goodness-of-fit and AIC
  semM3a<-sem.fit(M3a, nest, conditional = T)
  semM3a$missing.paths
  
  # Significative missing paths
  #fox_dens ~ sprAO + SPR_min + eSUM_moy_prec + lmg_abun -22.0248    0.6177   14   -35.6549  0.0000 ***
  #lmg_abun ~ sprAO + SPR_min  -0.5607    0.0661   16    -8.4881  0.0000 ***
  #prim_prod ~ sprAO + SPR_min + eSUM_moy_prec  -4.7081    1.3533   15    -3.4789  0.0034  **
  #eSUM_moy_prec ~ SPR_min + sprAO   0.0635    0.0042   16    15.0649  0.0000 ***
  #lmg_abun ~ eSUM_moy_prec + sprAO + SPR_min  -0.2991    0.0180   15   -16.6118  0.0000 ***
  #prim_prod ~ lmg_abun + SPR_min + eSUM_moy_prec  -1.9843    0.3601   15    -5.5107  0.0001 ***
  
  ##### Modèle 3 - b ######
  # Ajout de la piste (sprAO --> lmg_abun) 
  M3b<-list(
    lme(fox_dens~lmg_abun+SPR_min+eSUM_moy_prec, data = nest, random = ~1|AN),
    glmer(SN~prim_prod+fox_dens+HABITAT+eSUM_moy_prec+SPR_min+ (1|AN), data=nest, family=binomial(link = "logit")),
    lme(prim_prod~eSUM_moy_prec+SPR_min, data = nest, random = ~1|AN),
    lme(SPR_min~sprAO, data = nest, random = ~1|AN),
    lme(eSUM_moy_prec~sprAO, data = nest, random = ~1|AN),
    lme(lmg_abun~SPR_min + sprAO, data = nest, random = ~1|AN))
  
  # Get goodness-of-fit and AIC
  semM3b<-sem.fit(M3b, nest, conditional = T)
  semM3b$missing.paths
  
  # Significative missing paths
  #fox_dens ~ sprAO + SPR_min + eSUM_moy_prec + lmg_abun -22.0248    0.6177   14   -35.6549  0.0000 ***
  #prim_prod ~ sprAO + SPR_min + eSUM_moy_prec  -4.7081    1.3533   15    -3.4789  0.0034  **
  #eSUM_moy_prec ~ SPR_min + sprAO   0.0635    0.0042   16    15.0649  0.0000 ***
  #lmg_abun ~ eSUM_moy_prec + sprAO + SPR_min  -0.2991    0.0180   15   -16.6118  0.0000 ***
  #prim_prod ~ lmg_abun + sprAO + SPR_min + eSUM_moy_prec  -2.5515    0.3734   14    -6.8322  0.0000 ***
  
  ##### Modèle 3 - c ######
  # Ajout de la piste (eSUM_moy_prec --> lmg_abun) 
  M3c<-list(
    lme(fox_dens~lmg_abun+SPR_min+eSUM_moy_prec, data = nest, random = ~1|AN),
    glmer(SN~prim_prod+fox_dens+HABITAT+eSUM_moy_prec+SPR_min+ (1|AN), data=nest, family=binomial(link = "logit")),
    lme(prim_prod~eSUM_moy_prec+SPR_min, data = nest, random = ~1|AN),
    lme(SPR_min~sprAO, data = nest, random = ~1|AN),
    lme(eSUM_moy_prec~sprAO, data = nest, random = ~1|AN),
    lme(lmg_abun~SPR_min + sprAO + eSUM_moy_prec, data = nest, random = ~1|AN))
  
  # Get goodness-of-fit and AIC
  semM3c<-sem.fit(M3c, nest, conditional = T)
  semM3c$missing.paths
  
  # Significative missing paths
  #fox_dens ~ sprAO + SPR_min + eSUM_moy_prec + lmg_abun -22.0248    0.6177   14   -35.6549  0.0000 ***
  #prim_prod ~ sprAO + SPR_min + eSUM_moy_prec  -4.7081    1.3533   15    -3.4789  0.0034  **
  #eSUM_moy_prec ~ SPR_min + sprAO   0.0635    0.0042   16    15.0649  0.0000 ***
  #prim_prod ~ lmg_abun + sprAO + SPR_min + eSUM_moy_prec  -2.5515    0.3734   14    -6.8322  0.0000 ***
  
  ##### Modèle 3 - d ######
  # Ajout de la piste (prim_prod <-- lmg_abun) sens de la flèche car coefficient de corrélation de la piste manquante est négatif. Ainsi le lemg a une influence négative sur la productivité primaire
  M3d<-list(
    lme(fox_dens~lmg_abun+SPR_min+eSUM_moy_prec, data = nest, random = ~1|AN),
    glmer(SN~prim_prod+fox_dens+HABITAT+eSUM_moy_prec+SPR_min+ (1|AN), data=nest, family=binomial(link = "logit")),
    lme(prim_prod~eSUM_moy_prec+SPR_min + lmg_abun, data = nest, random = ~1|AN),
    lme(SPR_min~sprAO, data = nest, random = ~1|AN),
    lme(eSUM_moy_prec~sprAO, data = nest, random = ~1|AN),
    lme(lmg_abun~SPR_min + sprAO + eSUM_moy_prec, data = nest, random = ~1|AN))
  
  # Get goodness-of-fit and AIC
  semM3d<-sem.fit(M3d, nest, conditional = T)
  semM3d$missing.paths
  
  # Significative missing paths
  #ox_dens ~ sprAO + SPR_min + eSUM_moy_prec + lmg_abun -22.0248    0.6177   14   -35.6549  0.0000 ***
  #prim_prod ~ sprAO + SPR_min + eSUM_moy_prec + lmg_abun  -7.4299    1.3988   14    -5.3118  0.0001 ***
  #eSUM_moy_prec ~ SPR_min + sprAO   0.0635    0.0042   16    15.0649  0.0000 ***
  
  ##### Modèle 3 - e ######
  # Ajout de la piste (prim_prod <-- sprAO)
  M3e<-list(
    lme(fox_dens~lmg_abun+SPR_min+eSUM_moy_prec, data = nest, random = ~1|AN),
    glmer(SN~prim_prod+fox_dens+HABITAT+eSUM_moy_prec+SPR_min+ (1|AN), data=nest, family=binomial(link = "logit")),
    lme(prim_prod~eSUM_moy_prec+SPR_min + lmg_abun + sprAO, data = nest, random = ~1|AN),
    lme(SPR_min~sprAO, data = nest, random = ~1|AN),
    lme(eSUM_moy_prec~sprAO, data = nest, random = ~1|AN),
    lme(lmg_abun~SPR_min + sprAO + eSUM_moy_prec, data = nest, random = ~1|AN))
  
  # Get goodness-of-fit and AIC
  semM3e<-sem.fit(M3e, nest, conditional = T)
  semM3e$missing.paths
  
  # Significative missing paths
  #fox_dens ~ sprAO + SPR_min + eSUM_moy_prec + lmg_abun -22.0248    0.6177   14   -35.6549  0.0000 ***
  #eSUM_moy_prec ~ SPR_min + sprAO   0.0635    0.0042   16    15.0649  0.0000 ***
  #prim_prod ~ fox_dens + SPR_min + eSUM_moy_prec + lmg_abun + sprAO  -0.2701    0.0436   13    -6.1906  0.0000 ***
  
  ##### Modèle 3 - f ######
  # Ajout de la piste (fox_dens <-- sprAO)
  M3f<-list(
    lme(fox_dens~lmg_abun+SPR_min+eSUM_moy_prec + sprAO, data = nest, random = ~1|AN),
    glmer(SN~prim_prod+fox_dens+HABITAT+eSUM_moy_prec+SPR_min+ (1|AN), data=nest, family=binomial(link = "logit")),
    lme(prim_prod~eSUM_moy_prec+SPR_min + lmg_abun + sprAO, data = nest, random = ~1|AN),
    lme(SPR_min~sprAO, data = nest, random = ~1|AN),
    lme(eSUM_moy_prec~sprAO, data = nest, random = ~1|AN),
    lme(lmg_abun~SPR_min + sprAO + eSUM_moy_prec, data = nest, random = ~1|AN))
  
  # Get goodness-of-fit and AIC
  semM3f<-sem.fit(M3f, nest, conditional = T)
  semM3f$missing.paths
  
  # Significative missing paths
  #eSUM_moy_prec ~ SPR_min + sprAO   0.0635    0.0042   16    15.0649  0.0000 ***
  #prim_prod ~ fox_dens + sprAO + SPR_min + eSUM_moy_prec + lmg_abun  -0.2701    0.0436   13    -6.1906  0.0000 ***
  
  ##### Modèle 3 - g ######
  # Ajout de la piste (eSUM_moy_prec <-- SPR_min) 
  M3g<-list(
    lme(fox_dens~lmg_abun+SPR_min+eSUM_moy_prec + sprAO, data = nest, random = ~1|AN),
    glmer(SN~prim_prod+fox_dens+HABITAT+eSUM_moy_prec+SPR_min+ (1|AN), data=nest, family=binomial(link = "logit")),
    lme(prim_prod~eSUM_moy_prec+SPR_min + lmg_abun + sprAO, data = nest, random = ~1|AN),
    lme(SPR_min~sprAO + eSUM_moy_prec, data = nest, random = ~1|AN),
    lme(eSUM_moy_prec~sprAO + SPR_min, data = nest, random = ~1|AN),
    lme(lmg_abun~SPR_min + sprAO + eSUM_moy_prec, data = nest, random = ~1|AN))
  
  # Get goodness-of-fit and AIC
  semM3g<-sem.fit(M3g, nest, conditional = T)
  semM3g$missing.paths
  
  # Significative missing paths
  #prim_prod ~ fox_dens + sprAO + eSUM_moy_prec + lmg_abun + SPR_min  -0.2701    0.0436   13    -6.1906  0.0000 ***
  
  ##### Modèle 3 - h ######
  # Ajout de la piste (prim_prod <-- fox_dens) À VERIFIER DANS LA LITTERATURE SI POSSIBLE
  M3h<-list(
    lme(fox_dens~lmg_abun+SPR_min+eSUM_moy_prec + sprAO, data = nest, random = ~1|AN),
    glmer(SN~prim_prod+fox_dens+HABITAT+eSUM_moy_prec+SPR_min+ (1|AN), data=nest, family=binomial(link = "logit")),
    lme(prim_prod~eSUM_moy_prec+SPR_min + lmg_abun + sprAO + fox_dens, data = nest, random = ~1|AN),
    lme(SPR_min~sprAO + eSUM_moy_prec, data = nest, random = ~1|AN),
    lme(eSUM_moy_prec~sprAO + SPR_min, data = nest, random = ~1|AN),
    lme(lmg_abun~SPR_min + sprAO + eSUM_moy_prec, data = nest, random = ~1|AN))
  
  # Get goodness-of-fit and AIC
  semM3h<-sem.fit(M3h, nest, conditional = T)
  semM3h$missing.paths
  semM3h$Fisher.C
  semM3h$AIC
  # NO SIGNIFICATIVE MISSING PATH
  # Extract path coefficients
  sem.coefs(M3h, nest)
  
  #MAIS SN LIÉ À AUCUNE VARIABLE
  
  ###################### Retrait de variables à P-value forte à partir du modèle obtenu#################### 
  ##### Modèle 3-h-1 #####
  #Retrait de la piste avec la P-value la plus forte (SN -- HABITAT)
  
  M3h1<-list(
    lme(fox_dens~lmg_abun+SPR_min+eSUM_moy_prec + sprAO, data = nest, random = ~1|AN),
    glmer(SN~prim_prod+fox_dens+eSUM_moy_prec+SPR_min+ (1|AN), data=nest, family=binomial(link = "logit")),
    lme(prim_prod~eSUM_moy_prec+SPR_min + lmg_abun + sprAO + fox_dens, data = nest, random = ~1|AN),
    lme(SPR_min~sprAO + eSUM_moy_prec, data = nest, random = ~1|AN),
    lme(eSUM_moy_prec~sprAO + SPR_min, data = nest, random = ~1|AN),
    lme(lmg_abun~SPR_min + sprAO + eSUM_moy_prec, data = nest, random = ~1|AN))
  
  # Get goodness-of-fit and AIC
  semM3h1<-sem.fit(M3h1, nest, conditional = T)
  semM3h1$missing.paths
  semM3h1$Fisher.C
  semM3h1$AIC
  
  # NO SIGNIFICATIVE MISSING PATH
  # Extract path coefficients
  sem.coefs(M3h1, nest) # résultats weird !!! SN toujours pas relié
  
  ##### Modèle 3-h-2 #####
  #Retrait de la piste avec la P-value la plus forte (SN -- prim_prod)
  
  M3h2<-list(
    lme(fox_dens~lmg_abun+SPR_min+eSUM_moy_prec + sprAO, data = nest, random = ~1|AN),
    glmer(SN~fox_dens+eSUM_moy_prec+SPR_min+ (1|AN), data=nest, family=binomial(link = "logit")),
    lme(prim_prod~eSUM_moy_prec+SPR_min + lmg_abun + sprAO + fox_dens, data = nest, random = ~1|AN),
    lme(SPR_min~sprAO + eSUM_moy_prec, data = nest, random = ~1|AN),
    lme(eSUM_moy_prec~sprAO + SPR_min, data = nest, random = ~1|AN),
    lme(lmg_abun~SPR_min + sprAO + eSUM_moy_prec, data = nest, random = ~1|AN))
  
  # Get goodness-of-fit and AIC
  semM3h2<-sem.fit(M3h2, nest, conditional = T)
  semM3h2$missing.paths
  semM3h2$Fisher.C
  semM3h2$AIC
  
  # NO SIGNIFICATIVE MISSING PATH
  # Extract path coefficients
  sem.coefs(M3h2, nest) # résultats weird !!! SN toujours pas relié
  
  ##### Modèle 3-h-3 #####
  #Retrait de la piste avec la P-value la plus forte (SN -- eSUM_moy_prec)
  
  M3h3<-list(
    lme(fox_dens~lmg_abun+SPR_min+eSUM_moy_prec + sprAO, data = nest, random = ~1|AN),
    glmer(SN~fox_dens+SPR_min+ (1|AN), data=nest, family=binomial(link = "logit")),
    lme(prim_prod~eSUM_moy_prec+SPR_min + lmg_abun + sprAO + fox_dens, data = nest, random = ~1|AN),
    lme(SPR_min~sprAO + eSUM_moy_prec, data = nest, random = ~1|AN),
    lme(eSUM_moy_prec~sprAO + SPR_min, data = nest, random = ~1|AN),
    lme(lmg_abun~SPR_min + sprAO + eSUM_moy_prec, data = nest, random = ~1|AN))
  
  # Get goodness-of-fit and AIC
  semM3h3<-sem.fit(M3h3, nest, conditional = T)
  semM3h3$missing.paths
  semM3h3$Fisher.C
  semM3h3$AIC
  
  # NO SIGNIFICATIVE MISSING PATH
  # Extract path coefficients
  sem.coefs(M3h3, nest) # résultats weird !!!SN toujours pas relié
  
  ##### Modèle 3-h-4 #####
  #Retrait de la piste avec la P-value la plus forte (SN -- SPR_min)
  
  M3h4<-list(
    lme(fox_dens~lmg_abun+SPR_min+eSUM_moy_prec + sprAO, data = nest, random = ~1|AN),
    glmer(SN~fox_dens+ (1|AN), data=nest, family=binomial(link = "logit")),
    lme(prim_prod~eSUM_moy_prec+SPR_min + lmg_abun + sprAO + fox_dens, data = nest, random = ~1|AN),
    lme(SPR_min~sprAO + eSUM_moy_prec, data = nest, random = ~1|AN),
    lme(eSUM_moy_prec~sprAO + SPR_min, data = nest, random = ~1|AN),
    lme(lmg_abun~SPR_min + sprAO + eSUM_moy_prec, data = nest, random = ~1|AN))
  
  # Get goodness-of-fit and AIC
  semM3h4<-sem.fit(M3h4, nest, conditional = T)
  semM3h4$missing.paths
  semM3h4$Fisher.C
  semM3h4$AIC
  
  # NO SIGNIFICATIVE MISSING PATH
  # Extract path coefficients
  sem.coefs(M3h4, nest) # résultats weird !!!
  
  # NOT GOOD BECAUSE SN IS LNKED WITH NOTHING #####
  ##### M3 is abandonned #####
  
  
##### Modèles basés sur "nest 1" de 1996 à 2009 #####

####### Modèle 2 #####
M2<-list(
  lme(fox_dens~lmg_abun+MEAN_temp+MEAN_prec, data = nest1, random = ~1|AN),
  glmer(SN~DPN+fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), data=nest1, family=binomial(link = "logit")),
  lme(DPN~MEAN_prec+MEAN_temp, data = nest1, random = ~1|AN),
  lme(MEAN_temp~sprAO, data = nest1, random = ~1|AN),
  lme(MEAN_prec~sprAO, data = nest1, random = ~1|AN))

# Get goodness-of-fit and AIC
semM2<-sem.fit(M2, nest1, conditional = T)
semM2$missing.paths
semM2$Fisher.C
semM2$AIC
# Significative missing paths
#fox_dens ~ sprAO + lmg_abun + MEAN_temp + MEAN_prec -18.2364    0.4952    9   -36.8232  0.0000 ***
#DPN ~ sprAO + MEAN_temp + MEAN_prec -95.8401    8.2516   10   -11.6147  0.0000 ***
#MEAN_prec ~ MEAN_temp + sprAO   0.0376    0.0120 1773     3.1307  0.0018  **

####### Modèle 2-a #####
# Ajout de la piste (sprAO --> DPN)
M2a<-list(
  lme(fox_dens~lmg_abun+MEAN_temp+MEAN_prec, data = nest1, random = ~1|AN),
  glmer(SN~DPN+fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), data=nest1, family=binomial(link = "logit")),
  lme(DPN~MEAN_prec+MEAN_temp+sprAO, data = nest1, random = ~1|AN),
  lme(MEAN_temp~sprAO, data = nest1, random = ~1|AN),
  lme(MEAN_prec~sprAO, data = nest1, random = ~1|AN))

# Get goodness-of-fit and AIC
semM2a<-sem.fit(M2a, nest1, conditional = T)
semM2a$missing.paths
# Significative missing paths
#ox_dens ~ sprAO + lmg_abun + MEAN_temp + MEAN_prec -18.2364    0.4952    9   -36.8232  0.0000 ***
#MEAN_prec ~ MEAN_temp + sprAO   0.0376    0.0120 1773     3.1307  0.0018  **
#DPN ~ fox_dens + lmg_abun + MEAN_temp + MEAN_prec + sprAO  -4.1298    0.3824    8   -10.7986  0.0000 ***

####### Modèle 2-b #####
# Ajout de la piste (sprAO --> fox_dens)
M2b<-list(
  lme(fox_dens~lmg_abun+MEAN_temp+MEAN_prec+sprAO, data = nest1, random = ~1|AN),
  glmer(SN~DPN+fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), data=nest1, family=binomial(link = "logit")),
  lme(DPN~MEAN_prec+MEAN_temp+sprAO, data = nest1, random = ~1|AN),
  lme(MEAN_temp~sprAO, data = nest1, random = ~1|AN),
  lme(MEAN_prec~sprAO, data = nest1, random = ~1|AN))

# Get goodness-of-fit and AIC
semM2b<-sem.fit(M2b, nest1, conditional = T)
semM2b$missing.paths

# Significative missing paths
#MEAN_prec ~ MEAN_temp + sprAO   0.0376    0.0120 1773     3.1307  0.0018  **
#DPN ~ fox_dens + lmg_abun + sprAO + MEAN_temp + MEAN_prec  -4.1298    0.3824    8   -10.7986  0.0000 ***

####### Modèle 2-c #####
# Ajout de la piste (MEAN_prec --> MEAN_temp)
M2c<-list(
  lme(fox_dens~lmg_abun+MEAN_temp+MEAN_prec+sprAO, data = nest1, random = ~1|AN),
  glmer(SN~DPN+fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), data=nest1, family=binomial(link = "logit")),
  lme(DPN~MEAN_prec+MEAN_temp+sprAO, data = nest1, random = ~1|AN),
  lme(MEAN_temp~sprAO+MEAN_prec, data = nest1, random = ~1|AN),
  lme(MEAN_prec~sprAO, data = nest1, random = ~1|AN))

# Get goodness-of-fit and AIC
semM2c<-sem.fit(M2c, nest1, conditional = T)
semM2c$missing.paths

# Significative missing paths
#DPN ~ fox_dens + lmg_abun + sprAO + MEAN_prec + MEAN_temp  -4.1298    0.3824    8   -10.7986  0.0000 ***

####### Modèle 2-d **** #####
# Ajout de la piste (fox_dens <-- DPN)
M2d<-list(
  lme(fox_dens~lmg_abun+MEAN_temp+MEAN_prec+sprAO+DPN, data = nest1, random = ~1|AN),
  glmer(SN~DPN+fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), data=nest1, family=binomial(link = "logit")),
  lme(DPN~MEAN_prec+MEAN_temp+sprAO, data = nest1, random = ~1|AN),
  lme(MEAN_temp~sprAO+MEAN_prec, data = nest1, random = ~1|AN),
  lme(MEAN_prec~sprAO, data = nest1, random = ~1|AN))

# Get goodness-of-fit and AIC
semM2d<-sem.fit(M2d, nest1, conditional = T)
semM2d$missing.paths
semM2d$Fisher.C
semM2d$AIC
#NO SIGNIFICATIVE MISSING PATHS

# Extract path coefficients
sem.coefs(M2d, nest1)

### Attention, ici le sens de la piste qui est ajoutée entre DPN et fox_dens change les résultats. Si le sens est opposé
# par rapport à la situation actuelle, alors il manque un lien entre DPN et lmg_abun
#Graph drawing
M2dGRAPH<-DAG(
  fox_dens~lmg_abun+MEAN_temp+MEAN_prec+sprAO+DPN,
  SN~DPN+fox_dens+HABITAT+MEAN_prec+MEAN_temp,
  DPN~MEAN_prec+MEAN_temp+sprAO,
  MEAN_temp~sprAO+MEAN_prec,
  MEAN_prec~sprAO)

drawGraph(M2dGRAPH, adjust = T)
####### Modèle 2-d-BIS  #####
# Ajout de la piste (fox_dens --> DPN) à partir du modèle M2c
M2dBIS<-list(
  lme(fox_dens~lmg_abun+MEAN_temp+MEAN_prec+sprAO, data = nest1, random = ~1|AN),
  glmer(SN~DPN+fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), data=nest1, family=binomial(link = "logit")),
  lme(DPN~MEAN_prec+MEAN_temp+sprAO+fox_dens, data = nest1, random = ~1|AN),
  lme(MEAN_temp~sprAO+MEAN_prec, data = nest1, random = ~1|AN),
  lme(MEAN_prec~sprAO, data = nest1, random = ~1|AN))

# Get goodness-of-fit and AIC
semM2dBIS<-sem.fit(M2dBIS, nest1, conditional = T)
semM2dBIS$missing.paths

# SIGNIFICATIVE MISSING PATHS
#DPN ~ lmg_abun + sprAO + MEAN_prec + fox_dens + MEAN_temp  -4.9660    2.0124    8    -2.4678  0.0388 *

####### Modèle 2-d-BIS-1 **** #######
# Ajout de la piste (DPN --> lmg_abun) à partir du modèle M2c
M2dBIS1<-list(
  lme(fox_dens~lmg_abun+MEAN_temp+MEAN_prec+sprAO, data = nest1, random = ~1|AN),
  glmer(SN~DPN+fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), data=nest1, family=binomial(link = "logit")),
  lme(DPN~MEAN_prec+MEAN_temp+sprAO+fox_dens, data = nest1, random = ~1|AN),
  lme(MEAN_temp~sprAO+MEAN_prec, data = nest1, random = ~1|AN),
  lme(MEAN_prec~sprAO, data = nest1, random = ~1|AN),
  lme(lmg_abun~DPN, data = nest1, random = ~1|AN))

# Get goodness-of-fit and AIC
semM2dBIS1<-sem.fit(M2dBIS1, nest1, conditional = T)
semM2dBIS1$missing.paths
semM2dBIS1$Fisher.C
semM2dBIS1$AIC
#NO SIGNIFICATIVE MISSING PATHS

# Extract path coefficients
sem.coefs(M2dBIS1, nest1)

#Graph drawing
M2dBIS1GRAPH<-DAG(
  fox_dens~lmg_abun+MEAN_temp+MEAN_prec+sprAO,
  SN~DPN+fox_dens+HABITAT+MEAN_prec+MEAN_temp,
  DPN~MEAN_prec+MEAN_temp+sprAO+fox_dens,
  MEAN_temp~sprAO+MEAN_prec,
  MEAN_prec~sprAO,
  lmg_abun~DPN)

drawGraph(M2dBIS1GRAPH, adjust = T)
####Non utilisable car présente des relations cycliques entre DPN, lmg_abun et fox_dens####

###################### Retrait de variables à P-value forte à partir du modèle obtenu#################### 
##### Modèle 2-d-1 #####
#Retrait de la piste avec la P-value la plus forte (SN -- HABITAT)
M2d1<-list(
  lme(fox_dens~lmg_abun+MEAN_temp+MEAN_prec+sprAO+DPN, data = nest1, random = ~1|AN),
  glmer(SN~DPN+fox_dens+MEAN_prec+MEAN_temp+ (1|AN), data=nest1, family=binomial(link = "logit")),
  lme(DPN~MEAN_prec+MEAN_temp+sprAO, data = nest1, random = ~1|AN),
  lme(MEAN_temp~sprAO+MEAN_prec, data = nest1, random = ~1|AN),
  lme(MEAN_prec~sprAO, data = nest1, random = ~1|AN))

# Get goodness-of-fit and AIC
semM2d1<-sem.fit(M2d1, nest1, conditional = T)
semM2d1$missing.paths
semM2d1$Fisher.C
semM2d1$AIC
# NO SIGNIFICATIVE MISSING PATHS
sem.coefs(M2d1, nest1)

##### Modèle 2-d-2 #####
#Retrait de la piste avec la P-value la plus forte (fox_dens -- MEAN_prec)
M2d2<-list(
  lme(fox_dens~lmg_abun+MEAN_temp+sprAO+DPN, data = nest1, random = ~1|AN),
  glmer(SN~DPN+fox_dens+MEAN_prec+MEAN_temp+ (1|AN), data=nest1, family=binomial(link = "logit")),
  lme(DPN~MEAN_prec+MEAN_temp+sprAO, data = nest1, random = ~1|AN),
  lme(MEAN_temp~sprAO+MEAN_prec, data = nest1, random = ~1|AN),
  lme(MEAN_prec~sprAO, data = nest1, random = ~1|AN))

# Get goodness-of-fit and AIC
semM2d2<-sem.fit(M2d2, nest1, conditional = T)
semM2d2$missing.paths
semM2d2$Fisher.C
semM2d2$AIC
# NO SIGNIFICATIVE MISSING PATHS
sem.coefs(M2d2, nest1)

##### Modèle 2-d-3 #####
#Retrait de la piste avec la P-value la plus forte (fox_dens -- MEAN_temp)
M2d3<-list(
  lme(fox_dens~lmg_abun+sprAO+DPN, data = nest1, random = ~1|AN),
  glmer(SN~DPN+fox_dens+MEAN_prec+MEAN_temp+ (1|AN), data=nest1, family=binomial(link = "logit")),
  lme(DPN~MEAN_prec+MEAN_temp+sprAO, data = nest1, random = ~1|AN),
  lme(MEAN_temp~sprAO+MEAN_prec, data = nest1, random = ~1|AN),
  lme(MEAN_prec~sprAO, data = nest1, random = ~1|AN))

# Get goodness-of-fit and AIC
semM2d3<-sem.fit(M2d3, nest1, conditional = T)
semM2d3$missing.paths
semM2d3$Fisher.C
semM2d3$AIC
# NO SIGNIFICATIVE MISSING PATHS
sem.coefs(M2d3, nest1)

##### Modèle 2-d-4 #####
#Retrait de la piste avec la P-value la plus forte (DPN -- MEAN_prec)
M2d4<-list(
  lme(fox_dens~lmg_abun+sprAO+DPN, data = nest1, random = ~1|AN),
  glmer(SN~DPN+fox_dens+MEAN_prec+MEAN_temp+ (1|AN), data=nest1, family=binomial(link = "logit")),
  lme(DPN~MEAN_temp+sprAO, data = nest1, random = ~1|AN),
  lme(MEAN_temp~sprAO+MEAN_prec, data = nest1, random = ~1|AN),
  lme(MEAN_prec~sprAO, data = nest1, random = ~1|AN))

# Get goodness-of-fit and AIC
semM2d4<-sem.fit(M2d4, nest1, conditional = T)
semM2d4$missing.paths
semM2d4$Fisher.C
semM2d4$AIC
# NO SIGNIFICATIVE MISSING PATHS
sem.coefs(M2d4, nest1)

##### Modèle 2-d-5 #####
#Retrait de la piste avec la P-value la plus forte (DPN -- MEAN_temp)
M2d5<-list(
  lme(fox_dens~lmg_abun+sprAO+DPN, data = nest1, random = ~1|AN),
  glmer(SN~DPN+fox_dens+MEAN_prec+MEAN_temp+ (1|AN), data=nest1, family=binomial(link = "logit")),
  lme(DPN~sprAO, data = nest1, random = ~1|AN),
  lme(MEAN_temp~sprAO+MEAN_prec, data = nest1, random = ~1|AN),
  lme(MEAN_prec~sprAO, data = nest1, random = ~1|AN))

# Get goodness-of-fit and AIC
semM2d5<-sem.fit(M2d5, nest1, conditional = T)
semM2d5$missing.paths
semM2d5$Fisher.C
semM2d5$AIC
# NO SIGNIFICATIVE MISSING PATHS
sem.coefs(M2d5, nest1)

##### Modèle 2-d-6 #####
#Retrait de la piste avec la P-value la plus forte (sprAO -- MEAN_prec)
M2d6<-list(
  lme(fox_dens~lmg_abun+sprAO+DPN, data = nest1, random = ~1|AN),
  glmer(SN~DPN+fox_dens+MEAN_prec+MEAN_temp+ (1|AN), data=nest1, family=binomial(link = "logit")),
  lme(DPN~sprAO, data = nest1, random = ~1|AN),
  lme(MEAN_temp~sprAO+MEAN_prec, data = nest1, random = ~1|AN))

# Get goodness-of-fit and AIC
semM2d6<-sem.fit(M2d6, nest1, conditional = T)
semM2d6$missing.paths
semM2d6$Fisher.C
semM2d6$AIC
# NO SIGNIFICATIVE MISSING PATHS
sem.coefs(M2d6, nest1)

##### Modèle 2-d-7 #####
#Retrait de la piste avec la P-value la plus forte (sprAO -- MEAN_temp)
M2d7<-list(
  lme(fox_dens~lmg_abun+sprAO+DPN, data = nest1, random = ~1|AN),
  glmer(SN~DPN+fox_dens+MEAN_prec+MEAN_temp+ (1|AN), data=nest1, family=binomial(link = "logit")),
  lme(DPN~sprAO, data = nest1, random = ~1|AN),
  lme(MEAN_temp~MEAN_prec, data = nest1, random = ~1|AN))

# Get goodness-of-fit and AIC
semM2d7<-sem.fit(M2d7, nest1, conditional = T)
semM2d7$missing.paths
semM2d7$Fisher.C
semM2d7$AIC
# NO SIGNIFICATIVE MISSING PATHS
sem.coefs(M2d7, nest1)

##### Modèle 2-d-8 #####
#Retrait de la piste avec la P-value la plus forte (SN -- DPN )
M2d8<-list(
  lme(fox_dens~lmg_abun+sprAO+DPN, data = nest1, random = ~1|AN),
  glmer(SN~fox_dens+MEAN_temp+MEAN_prec+ (1|AN), data=nest1, family=binomial(link = "logit")),
  lme(DPN~sprAO, data = nest1, random = ~1|AN),
  lme(MEAN_temp~MEAN_prec, data = nest1, random = ~1|AN))

# Get goodness-of-fit and AIC
semM2d8<-sem.fit(M2d8, nest1, conditional = T)
semM2d8$missing.paths
semM2d8$Fisher.C
semM2d8$AIC
# NO SIGNIFICATIVE MISSING PATHS
sem.coefs(M2d8, nest1)

##### Modèle 2-d-9 #####
#Retrait de la piste avec la P-value la plus forte (SN -- MEAN_temp)
M2d9<-list(
  lme(fox_dens~lmg_abun+sprAO+DPN, data = nest1, random = ~1|AN),
  glmer(SN~fox_dens+MEAN_prec+ (1|AN), data=nest1, family=binomial(link = "logit")),
  lme(DPN~sprAO, data = nest1, random = ~1|AN),
  lme(MEAN_temp~MEAN_prec, data = nest1, random = ~1|AN))

# Get goodness-of-fit and AIC
semM2d9<-sem.fit(M2d9, nest1, conditional = T)
semM2d9$missing.paths
semM2d9$Fisher.C
semM2d9$AIC
# NO SIGNIFICATIVE MISSING PATHS
sem.coefs(M2d9, nest1)

##### Modèle 2-d-10 #####
#Retrait de la piste avec la P-value la plus forte (SN -- MEAN_prec)
M2d10<-list(
  lme(fox_dens~lmg_abun+sprAO+DPN, data = nest1, random = ~1|AN),
  glmer(SN~fox_dens+ (1|AN), data=nest1, family=binomial(link = "logit")),
  lme(DPN~sprAO, data = nest1, random = ~1|AN),
  lme(MEAN_temp~MEAN_prec, data = nest1, random = ~1|AN))

# Get goodness-of-fit and AIC
semM2d10<-sem.fit(M2d10, nest1, conditional = T)
semM2d10$missing.paths
semM2d10$Fisher.C
semM2d10$AIC
# NO SIGNIFICATIVE MISSING PATHS
sem.coefs(M2d10, nest1)

##### Modèle 2-d-11 **** #####
#Retrait de la piste avec la P-value la plus forte (SN -- MEAN_prec)
M2d11<-list(
  lme(fox_dens~lmg_abun+sprAO+DPN, data = nest1, random = ~1|AN),
  glmer(SN~fox_dens+ (1|AN), data=nest1, family=binomial(link = "logit")),
  lme(DPN~sprAO, data = nest1, random = ~1|AN))

# Get goodness-of-fit and AIC
semM2d11<-sem.fit(M2d11, nest1, conditional = T)
semM2d11$missing.paths
semM2d11$Fisher.C
semM2d11$AIC
# NO SIGNIFICATIVE MISSING PATHS
sem.coefs(M2d11, nest1)


#Graph drawing
M2d11GRAPH<-DAG(
  fox_dens~lmg_abun+sprAO+DPN,
  SN~fox_dens,
  DPN~sprAO)

drawGraph(M2d11GRAPH, adjust = T)

####### Modèle 5 #####
#Utilisation des valeurs de DPN et de la date moyenne de ponte entre DPN et SN
M5<-list(
  lme(fox_dens~lmg_abun+MEAN_temp+MEAN_prec, data = nest1, random = ~1|AN),
  glmer(SN~medianDate_Hatch+fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), data=nest1, family=binomial(link = "logit")),
  lme(medianDate_Hatch~DPN+sprAO, data = nest1, random = ~1|AN),
  lme(DPN~MEAN_prec+MEAN_temp, data = nest1, random = ~1|AN),
  lme(MEAN_temp~sprAO, data = nest1, random = ~1|AN),
  lme(MEAN_prec~sprAO, data = nest1, random = ~1|AN))

# Get goodness-of-fit and AIC
semM5<-sem.fit(M5, nest1, conditional = T)
semM5$missing.paths
semM5$Fisher.C
semM5$AIC
# Significative missing paths
#medianDate_Hatch ~ lmg_abun + sprAO + DPN  33.3545    2.3421    8    14.2415  0.0000 ***
#fox_dens ~ sprAO + lmg_abun + MEAN_temp + MEAN_prec -18.2364    0.4952    9   -36.8232  0.0000 ***
#DPN ~ sprAO + MEAN_temp + MEAN_prec -95.8401    8.2516   10   -11.6147  0.0000 ***
#MEAN_prec ~ MEAN_temp + sprAO   0.0376    0.0120 1773     3.1307  0.0018  **
#medianDate_Hatch ~ fox_dens + lmg_abun + MEAN_temp + MEAN_prec + sprAO + DPN  -7.0535    0.4304    7   -16.3875  0.0000 ***
