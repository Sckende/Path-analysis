###### Path analysis without primary production #######

rm(list = ls()) #clean R memory

setwd(dir = "/Users/nicolas/Documents/Claire/Doc doc doc !/R analysis/Data")
nini<-read.table("Path analysis_data 3.txt", h=T, dec=".", sep = ",")

nest<-nini[,-c(7,14,32)]# retrait des variables ECLO, NEST.TYPE, prim_prod
summary(nest)

#Package nécessaires
require(nlme)
require(lme4)
require(piecewiseSEM)
require(ggm)

nest<-na.omit(nest)


#### Modèle 1 ####
M1<-list(
  lme(fox_dens~lmg_abun, data = nest, random = ~1|AN),
  glmer(SN~fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lme(MEAN_temp~esumAO, data = nest, random = ~1|AN),
  lme(MEAN_prec~esumAO, data = nest, random = ~1|AN))

# Get goodness-of-fit and AIC
sem.fit(M1, nest, conditional = T)
# MISSING PATHS
#fox_dens ~ esumAO + lmg_abun  -8.8248    0.5692   17   -15.5044  0.0000 ***
#MEAN_temp ~ MEAN_prec + esumAO   0.3510    0.0263 3786    13.3240  0.0000 ***

#### Modèle 1-a ####
#Ajout de la piste MEAN_temp <-- MEAN_prec
M1a<-list(
  lme(fox_dens~lmg_abun, data = nest, random = ~1|AN),
  glmer(SN~fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lme(MEAN_temp~esumAO+MEAN_prec, data = nest, random = ~1|AN),
  lme(MEAN_prec~esumAO, data = nest, random = ~1|AN))

# Get goodness-of-fit and AIC
sem.fit(M1a, nest, conditional = T)

#MISSING PATHS
#fox_dens ~ esumAO + lmg_abun  -8.8248    0.5692   17   -15.5044  0.0000 ***

#### Modèle 1-b ***** ####
#Ajout de la piste fox_dens <-- esumAO
M1b<-list(
  lme(fox_dens~lmg_abun+esumAO, data = nest, random = ~1|AN),
  glmer(SN~fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lme(MEAN_temp~esumAO+MEAN_prec, data = nest, random = ~1|AN),
  lme(MEAN_prec~esumAO, data = nest, random = ~1|AN))

# Get goodness-of-fit and AIC
sem.fit(M1b, nest, conditional = T)

#NO MISSING PATHS
sem.coefs(M1b, nest)

#### Retrait de certains liens non-significatifs à partir du modèle 3 ####

#### Modèle 1-b-1 ####
# Retrait du lien (SN -- HAB)
M1b1<-list(
  lme(fox_dens~lmg_abun+esumAO, data = nest, random = ~1|AN),
  glmer(SN~fox_dens+MEAN_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lme(MEAN_temp~esumAO+MEAN_prec, data = nest, random = ~1|AN),
  lme(MEAN_prec~esumAO, data = nest, random = ~1|AN))

# Get goodness-of-fit and AIC
sem.fit(M1b1, nest, conditional = T)
#NO MISSING PATHS
sem.coefs(M1b1, nest)

#### Modèle 1-b-2 ####
# Retrait du lien (MEAN_temp -- esumAO)
M1b2<-list(
  lme(fox_dens~lmg_abun+esumAO, data = nest, random = ~1|AN),
  glmer(SN~fox_dens+MEAN_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lme(MEAN_temp~MEAN_prec, data = nest, random = ~1|AN),
  lme(MEAN_prec~esumAO, data = nest, random = ~1|AN))

# Get goodness-of-fit and AIC
sem.fit(M1b2, nest, conditional = T)
#NO MISSING PATHS
sem.coefs(M1b2, nest)

#### Modèle 1-b-3 *****####
# Retrait du lien (MEAN_prec -- esumAO)
M1b3<-list(
  lme(fox_dens~lmg_abun+esumAO, data = nest, random = ~1|AN),
  glmer(SN~fox_dens+MEAN_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lme(MEAN_temp~MEAN_prec, data = nest, random = ~1|AN))

# Get goodness-of-fit and AIC
sem.fit(M1b3, nest, conditional = T)
#NO MISSING PATHS
sem.coefs(M1b3, nest)

#### Modèle 1-b-4 ####
# Retrait du lien (SN -- fox_dens)
M1b4<-list(
  lme(fox_dens~lmg_abun+esumAO, data = nest, random = ~1|AN),
  glmer(SN~MEAN_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lme(MEAN_temp~MEAN_prec, data = nest, random = ~1|AN))

# Get goodness-of-fit and AIC
sem.fit(M1b4, nest, conditional = T)
#NO MISSING PATHS
sem.coefs(M1b4, nest)

### Ici le modèle 3-c conserve le lien SN--fox_dens mais qui n'est pas significativement différent de 0 (p-value = 0.1).
# Le modèle 3-d est bon mais le lien entre SN--fox_dens n'est pas présent :-(

#Graph drawing
M1b3GRAPH<-DAG(
  fox_dens~lmg_abun+esumAO,
  SN~fox_dens+MEAN_prec+MEAN_temp,
  MEAN_temp~MEAN_prec
)
drawGraph(M1b3GRAPH, adjust = T)

#Graph drawing
M1b4GRAPH<-DAG(
  fox_dens~lmg_abun+esumAO,
  SN~MEAN_prec+MEAN_temp,
  MEAN_temp~MEAN_prec
)
drawGraph(M1b4GRAPH, adjust = T)


#### Modèle 2 ####
#lmg relié au winAO / temp et prec reliées avec sprAO
M2<-list(
  lme(fox_dens~lmg_abun, data = nest, random = ~1|AN),
  lme(lmg_abun~winAO, data = nest, random = ~1|AN),
  glmer(SN~fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lme(MEAN_temp~sprAO, data = nest, random = ~1|AN),
  lme(MEAN_prec~sprAO, data = nest, random = ~1|AN))

# Get goodness-of-fit and AIC
sem.fit(M2, nest, conditional = T)
# MISSING PATHS
#fox_dens ~ winAO + lmg_abun  -1.2512    0.5752   17    -2.1752  0.0440   *
#fox_dens ~ sprAO + lmg_abun  -4.5406    0.5747   17    -7.9009  0.0000 ***
#MEAN_temp ~ MEAN_prec + sprAO   0.3506    0.0263 3786    13.3124  0.0000 ***

#### Modèle 2-a ####
#Ajout du lien (MEAN_prec--MEAN_temp)
M2a<-list(
  lme(fox_dens~lmg_abun, data = nest, random = ~1|AN),
  lme(lmg_abun~winAO, data = nest, random = ~1|AN),
  glmer(SN~fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lme(MEAN_temp~sprAO, data = nest, random = ~1|AN),
  lme(MEAN_prec~sprAO+MEAN_temp, data = nest, random = ~1|AN))

# Get goodness-of-fit and AIC
sem.fit(M2a, nest, conditional = T)
# MISSING PATHS
#fox_dens ~ winAO + lmg_abun  -1.2512    0.5752   17    -2.1752  0.0440   *
#fox_dens ~ sprAO + lmg_abun  -4.5406    0.5747   17    -7.9009  0.0000 ***
#MEAN_prec ~ fox_dens + lmg_abun + sprAO + MEAN_temp  -0.0226    0.0102   16    -2.2124  0.0418   *

#### Modèle 2-b ####
#Ajout du lien (sprAO--fox_dens)
M2b<-list(
  lme(fox_dens~lmg_abun+sprAO, data = nest, random = ~1|AN),
  lme(lmg_abun~winAO, data = nest, random = ~1|AN),
  glmer(SN~fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lme(MEAN_temp~sprAO, data = nest, random = ~1|AN),
  lme(MEAN_prec~sprAO+MEAN_temp, data = nest, random = ~1|AN))

# Get goodness-of-fit and AIC
sem.fit(M2b, nest, conditional = T)
# MISSING PATHS
#fox_dens ~ winAO + sprAO + lmg_abun  -1.3449    0.5707   16    -2.3566  0.0315 *
#MEAN_prec ~ fox_dens + sprAO + lmg_abun + MEAN_temp  -0.0226    0.0102   16    -2.2124  0.0418 *

#### Modèle 2-c ####
#Ajout du lien (MEAN_prec--fox_dens)
M2c<-list(
  lme(fox_dens~lmg_abun+sprAO+MEAN_prec, data = nest, random = ~1|AN),
  lme(lmg_abun~winAO, data = nest, random = ~1|AN),
  glmer(SN~fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lme(MEAN_temp~sprAO, data = nest, random = ~1|AN),
  lme(MEAN_prec~sprAO+MEAN_temp, data = nest, random = ~1|AN))

# Get goodness-of-fit and AIC
sem.fit(M2c, nest, conditional = T)
# MISSING PATHS
#fox_dens ~ winAO + sprAO + lmg_abun + MEAN_prec  -1.3512    0.5708   16    -2.3670  0.0309 *

#### Modèle 2-d ####
#Ajout du lien (winAO--fox_dens)
M2d<-list(
  lme(fox_dens~lmg_abun+sprAO+MEAN_prec+winAO, data = nest, random = ~1|AN),
  lme(lmg_abun~winAO, data = nest, random = ~1|AN),
  glmer(SN~fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lme(MEAN_temp~sprAO, data = nest, random = ~1|AN),
  lme(MEAN_prec~sprAO+MEAN_temp, data = nest, random = ~1|AN))

# Get goodness-of-fit and AIC
sem.fit(M2d, nest, conditional = T)
# NO MISSING PATHS
sem.coefs(M2d, nest)
#### Retrait de certains liens non-significatifs à partir du modèle A-4 ####

#### Modèle 2-d-1 ####
# Retrait du lien (SN -- HAB)

M2d1<-list(
  lme(fox_dens~lmg_abun+sprAO+MEAN_prec+winAO, data = nest, random = ~1|AN),
  lme(lmg_abun~winAO, data = nest, random = ~1|AN),
  glmer(SN~fox_dens+MEAN_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lme(MEAN_temp~sprAO, data = nest, random = ~1|AN),
  lme(MEAN_prec~sprAO+MEAN_temp, data = nest, random = ~1|AN))

# Get goodness-of-fit and AIC
sem.fit(M2d1, nest, conditional = T)
sem.coefs(M2d1, nest)
# NO MISSING PATHS

#### Modèle 2-d-2 ####
# Retrait du lien (sprAO -- MEAN_prec)

M2d2<-list(
  lme(fox_dens~lmg_abun+sprAO+MEAN_prec+winAO, data = nest, random = ~1|AN),
  lme(lmg_abun~winAO, data = nest, random = ~1|AN),
  glmer(SN~fox_dens+MEAN_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lme(MEAN_temp~sprAO, data = nest, random = ~1|AN),
  lme(MEAN_prec~MEAN_temp, data = nest, random = ~1|AN))

# Get goodness-of-fit and AIC
sem.fit(M2d2, nest, conditional = T)
sem.coefs(M2d2, nest)
# NO MISSING PATHS

#### Modèle 2-d-3 ####
# Retrait du lien (sprAO -- MEAN_temp)

M2d3<-list(
  lme(fox_dens~lmg_abun+sprAO+MEAN_prec+winAO, data = nest, random = ~1|AN),
  lme(lmg_abun~winAO, data = nest, random = ~1|AN),
  glmer(SN~fox_dens+MEAN_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lme(MEAN_prec~MEAN_temp, data = nest, random = ~1|AN))

# Get goodness-of-fit and AIC
sem.fit(M2d3, nest, conditional = T)
#sem.coefs(M2d3, nest)
# SIGNIFICATIVE MISSING PATHS
#lmg_abun ~ MEAN_temp + winAO   0.0000    0.0000 3786    -2.0635  0.0391 *

#### Modèle 2-d-4 ####
# Ajout du lien (lmg_abun -- MEAN_temp)

M2d4<-list(
  lme(fox_dens~lmg_abun+sprAO+MEAN_prec+winAO, data = nest, random = ~1|AN),
  lme(lmg_abun~winAO+MEAN_temp, data = nest, random = ~1|AN),
  glmer(SN~fox_dens+MEAN_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lme(MEAN_prec~MEAN_temp, data = nest, random = ~1|AN))

# Get goodness-of-fit and AIC
sem.fit(M2d4, nest, conditional = T)
sem.coefs(M2d4, nest)
# NO MISSING PATHS

#### Modèle 2-d-5 ***** BOF ####
# Retrait du lien (fox_dens -- MEAN_prec)

M2d5<-list(
  lme(fox_dens~lmg_abun+sprAO+winAO, data = nest, random = ~1|AN),
  lme(lmg_abun~winAO+MEAN_temp, data = nest, random = ~1|AN),
  glmer(SN~fox_dens+MEAN_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lme(MEAN_prec~MEAN_temp, data = nest, random = ~1|AN))

# Get goodness-of-fit and AIC
sem.fit(M2d5, nest, conditional = T)
sem.coefs(M2d5, nest)
# NO MISSING PATHS
####Mais lien entre fox_dens et SN non significatif ! 


########## Utilisation de cumul_prec ##########
#### Modèle 3 ####
#Utilisation de la variable cumul_prec à la place de la valeur moyenne des précipitations
nn<-nest[,c(15,25,28,29,30,31,40)]
summary(nn)
require(psych)
pairs.panels(nn)

#Attention à ne mettre des effets aléatoires dans les modèles que si nécessaire
# Modèle 3 original
M3<-list(
   lm(fox_dens~lmg_abun, data = nest),
   glmer(SN~fox_dens+cumul_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
   lm(lmg_abun~winAO, data = nest),
   lme(MEAN_temp~sprAO, data = nest, random = ~1|AN),
   lme(cumul_prec~sprAO, data = nest, random = ~1|AN))

# Get goodness-of-fit and AIC
sem.fit(M3, nest, conditional = T)
# Significant missing paths

#fox_dens ~ winAO + lmg_abun   6.2486    0.3730 3804    16.7514  0.0000 ***
#lmg_abun ~ sprAO + winAO  -0.3033    0.0342 3804    -8.8696  0.0000 ***
#fox_dens ~ sprAO + lmg_abun  -2.3738    0.4043 3804    -5.8713  0.0000 ***
#MEAN_temp ~ cumul_prec + sprAO   0.0423    0.0011 3786    40.1851  0.0000 ***
#fox_dens ~ cumul_prec + sprAO + lmg_abun  -0.3414    0.0134 3803   -25.5451  0.0000 ***
#fox_dens ~ MEAN_temp + sprAO + lmg_abun   1.0028    0.2221 3803     4.5154  0.0000 ***

#####Modèle 3-a #####
#Ajout de la piste (fox -- winAO)
M3a<-list(
  lm(fox_dens~lmg_abun+winAO, data = nest),
  glmer(SN~fox_dens+cumul_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lm(lmg_abun~winAO, data = nest),
  lme(MEAN_temp~sprAO, data = nest, random = ~1|AN),
  lme(cumul_prec~sprAO, data = nest, random = ~1|AN))
# Get goodness-of-fit and AIC
sem.fit(M3a, nest, conditional = T)
# Significant missing paths
#lmg_abun ~ sprAO + winAO  -0.3033    0.0342 3804    -8.8696  0.0000 ***
#fox_dens ~ sprAO + winAO + lmg_abun  -1.6466    0.3937 3803    -4.1829  0.0000 ***
#MEAN_temp ~ cumul_prec + sprAO   0.0423    0.0011 3786    40.1851  0.0000 ***
#fox_dens ~ cumul_prec + sprAO + winAO + lmg_abun  -0.3226    0.0130 3802   -24.7238  0.0000 ***

#####Modèle 3-b #####
#Ajout de la piste (lmg -- sprAO)
M3b<-list(
  lm(fox_dens~lmg_abun+winAO, data = nest),
  glmer(SN~fox_dens+cumul_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lm(lmg_abun~winAO+sprAO, data = nest),
  lme(MEAN_temp~sprAO, data = nest, random = ~1|AN),
  lme(cumul_prec~sprAO, data = nest, random = ~1|AN))
# Get goodness-of-fit and AIC
sem.fit(M3b, nest, conditional = T)
# Significant missing paths
#fox_dens ~ sprAO + winAO + lmg_abun  -1.6466    0.3937 3803    -4.1829  0.0000 ***
#SN ~ lmg_abun + winAO + sprAO + cumul_prec + MEAN_temp + fox_dens  -0.6473    0.3293   NA    -1.9655  0.0494   *
#MEAN_temp ~ cumul_prec + sprAO   0.0423    0.0011 3786    40.1851  0.0000 ***
#fox_dens ~ cumul_prec + sprAO + winAO + lmg_abun  -0.3226    0.0130 3802   -24.7238  0.0000 ***

#####Modèle 3-c #####
#Ajout de la piste (temp -- prec)
M3c<-list(
  lm(fox_dens~lmg_abun+winAO, data = nest),
  glmer(SN~fox_dens+cumul_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lm(lmg_abun~winAO+sprAO, data = nest),
  lme(MEAN_temp~sprAO+cumul_prec, data = nest, random = ~1|AN),
  lme(cumul_prec~sprAO, data = nest, random = ~1|AN))
# Get goodness-of-fit and AIC
sem.fit(M3c, nest, conditional = T)
# Significant missing paths
#fox_dens ~ sprAO + winAO + lmg_abun  -1.6466    0.3937 3803    -4.1829  0.0000 ***
#SN ~ lmg_abun + winAO + sprAO + cumul_prec + fox_dens + MEAN_temp  -0.6473    0.3293   NA    -1.9655  0.0494   *
#fox_dens ~ cumul_prec + sprAO + winAO + lmg_abun  -0.3226    0.0130 3802   -24.7238  0.0000 ***

#####Modèle 3-d #####
#Ajout de la piste (fox -- sprAO)
M3d<-list(
  lm(fox_dens~lmg_abun+winAO+sprAO, data = nest),
  glmer(SN~fox_dens+cumul_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lm(lmg_abun~winAO+sprAO, data = nest),
  lme(MEAN_temp~sprAO+cumul_prec, data = nest, random = ~1|AN),
  lme(cumul_prec~sprAO, data = nest, random = ~1|AN))
# Get goodness-of-fit and AIC
sem.fit(M3d, nest, conditional = T)
# Significant missing paths
#SN ~ lmg_abun + winAO + sprAO + cumul_prec + fox_dens + MEAN_temp  -0.6473    0.3293   NA    -1.9655  0.0494   *
#fox_dens ~ cumul_prec + sprAO + winAO + lmg_abun  -0.3226    0.0130 3802   -24.7238  0.0000 ***

#####Modèle 3-e #####
#Ajout de la piste (fox -- cumul_prec)
M3e<-list(
  lm(fox_dens~lmg_abun+winAO+sprAO+cumul_prec, data = nest),
  glmer(SN~fox_dens+cumul_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lm(lmg_abun~winAO+sprAO, data = nest),
  lme(MEAN_temp~sprAO+cumul_prec, data = nest, random = ~1|AN),
  lme(cumul_prec~sprAO, data = nest, random = ~1|AN))
# Get goodness-of-fit and AIC
sem.fit(M3e, nest, conditional = T)
# Significant missing paths
#SN ~ lmg_abun + winAO + sprAO + cumul_prec + fox_dens + MEAN_temp  -0.6473    0.3293 NA    -1.9655  0.0494 *

#####Modèle 3-f ***** #####
#Ajout de la piste (SN -- lmg_abun)
M3f<-list(
  lm(fox_dens~lmg_abun+winAO+sprAO+cumul_prec, data = nest),
  glmer(SN~fox_dens+cumul_prec+MEAN_temp+lmg_abun+ (1|AN), data=nest, family=binomial(link = "logit")),
  lm(lmg_abun~winAO+sprAO, data = nest),
  lme(MEAN_temp~sprAO+cumul_prec, data = nest, random = ~1|AN),
  lme(cumul_prec~sprAO, data = nest, random = ~1|AN))
# Get goodness-of-fit and AIC
sem.fit(M3f, nest, conditional = T)
# No significant missing paths
sem.coefs(M3f, nest)

sem.model.fits(M3f)

#####Modèle 3-f-1 #####
#Retrait de la piste (sprAO -- MEAN_temp)
M3f1<-list(
  lm(fox_dens~lmg_abun+winAO+sprAO+cumul_prec, data = nest),
  glmer(SN~fox_dens+cumul_prec+MEAN_temp+lmg_abun+ (1|AN), data=nest, family=binomial(link = "logit")),
  lm(lmg_abun~winAO+sprAO, data = nest),
  lme(MEAN_temp~cumul_prec, data = nest, random = ~1|AN),
  lme(cumul_prec~sprAO, data = nest, random = ~1|AN))
# Get goodness-of-fit and AIC
sem.fit(M3f1, nest, conditional = T)
# No significant missing paths
sem.coefs(M3f1, nest)
# No significant missing path

#####Modèle 3-f-2 #####
#Retrait de la piste (sprAO -- cumul_prec)
M3f2<-list(
  lm(fox_dens~lmg_abun+winAO+sprAO+cumul_prec, data = nest),
  glmer(SN~fox_dens+cumul_prec+MEAN_temp+lmg_abun+ (1|AN), data=nest, family=binomial(link = "logit")),
  lm(lmg_abun~winAO+sprAO, data = nest),
  lme(MEAN_temp~cumul_prec, data = nest, random = ~1|AN))
# Get goodness-of-fit and AIC
sem.fit(M3f2, nest, conditional = T)
# Significant missing paths
#fox_dens ~ MEAN_temp + cumul_prec + winAO + sprAO + lmg_abun   0.9062    0.2044 3801     4.4332  0.0000 ***


##### Modèle 3-f-3 #####
# ajout de la piste (fox_dens -- MEAN_temp) et retrait de la variable sprAO de tous les modèles
M3f3<-list(
  lm(fox_dens~lmg_abun+winAO+cumul_prec+MEAN_temp, data = nest),
  glmer(SN~fox_dens+cumul_prec+MEAN_temp+lmg_abun+ (1|AN), data=nest, family=binomial(link = "logit")),
  lm(lmg_abun~winAO, data = nest),
  lme(cumul_prec~MEAN_temp, data = nest, random = ~1|AN))
# Get goodness-of-fit and AIC
sem.fit(M3f3, nest, conditional = T)
# Significant missing paths
#lmg_abun ~ MEAN_temp + winAO + cumul_prec -0.1191    0.0191 3803    -6.2326  0.0000 ***
#sem.coefs(M3f3, nest)

##### Modèle 3-f-4 ****** #####
# ajout de la piste (lmg_abun -- MEAN_temp)
M3f4<-list(
  lm(fox_dens~lmg_abun+winAO+cumul_prec+MEAN_temp, data = nest),
  glmer(SN~fox_dens+cumul_prec+MEAN_temp+lmg_abun+ (1|AN), data=nest, family=binomial(link = "logit")),
  lm(lmg_abun~winAO+MEAN_temp, data = nest),
  lme(cumul_prec~MEAN_temp, data = nest, random = ~1|AN))
# Get goodness-of-fit and AIC
sem.fit(M3f4, nest, conditional = T)
# No significant missing paths
sem.coefs(M3f4, nest)

sem.model.fits(M3f4)

##### Modèle 3-f-5 *****#####
# retrait de la piste (lmg_abun -- SN)
M3f5<-list(
  lm(fox_dens~lmg_abun+winAO+cumul_prec+MEAN_temp, data = nest),
  glmer(SN~fox_dens+cumul_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lm(lmg_abun~winAO+MEAN_temp, data = nest),
  lme(cumul_prec~MEAN_temp, data = nest, random = ~1|AN))
# Get goodness-of-fit and AIC
sem.fit(M3f5, nest, conditional = T#, corr.errors = "MEAN_temp ~~ cumul_prec"
  )

# No significant missing paths
sem.coefs(M3f5, nest)
#sem.plot(M3f5, nest, show.nonsig = T)

##### Modèle 3-f-5bis #####
# retrait de la piste (MEAN_temp -- cumul_prec) pour remplacer par une covariance
M3f5bis<-list(
  lm(fox_dens~lmg_abun+winAO+cumul_prec+MEAN_temp, data = nest),
  glmer(SN~fox_dens+cumul_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lm(lmg_abun~winAO+MEAN_temp, data = nest))
# Get goodness-of-fit and AIC
sem.fit(M3f5bis, nest, conditional = T, corr.errors = "MEAN_temp ~~ cumul_prec")

# Significant missing paths
#lmg_abun ~ cumul_prec + winAO + MEAN_temp   0.0034    0.0012 3803 2.7344  0.0063 **
#sem.coefs(M3f5bis, nest)
#sem.plot(M3f5bis, nest, show.nonsig = T)

##### Modèle 3-f-6 ***** SELECTIONED #####
# ajout de la piste (lmg_abun -- cumul_prec) pour remplacer par une covariance
M3f6<-list(
  lm(fox_dens~lmg_abun+winAO+cumul_prec+MEAN_temp, data = nest),
  glmer(SN~fox_dens+cumul_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")),
  lm(lmg_abun~winAO+MEAN_temp+cumul_prec, data = nest))
# Get goodness-of-fit and AIC
sem.fit(M3f6, nest, conditional = T, corr.errors = "MEAN_temp ~~ cumul_prec")

# No significant missing paths
sem.coefs(M3f6, nest)
sem.plot(M3f6, nest, show.nonsig = T)


#Standardisation des données
nene<-nest[,c(2,3,15,25,28,29,30,40)]
nenescale<-scale(nene[,c(3,4,5,6,7)])
nenescale<-cbind(nest[,c(2,3,40)], nenescale)


##### Modèle 3-f-6 SCALE****#####

M3f6Sc<-list(
  lm(fox_dens~lmg_abun+winAO+cumul_prec+MEAN_temp, data = nenescale),
  glmer(SN~fox_dens+cumul_prec+MEAN_temp+ (1|AN), data=nenescale, family=binomial(link = "logit")),
  lm(lmg_abun~winAO+MEAN_temp+cumul_prec, data = nenescale))
# Get goodness-of-fit and AIC
sem.fit(M3f6Sc, nenescale, conditional = T, corr.errors = "MEAN_temp ~~ cumul_prec")
# No significant missing paths
sem.coefs(M3f6Sc, nenescale)
#sem.plot(M3f6Sc, nenescale, show.nonsig = T)
sem.model.fits(M3f6Sc) #calcul des R2



####### Modèle bibi ********* ########
Mbibi<-list(
  lm(fox_dens~lmg_abun+cumul_prec+MEAN_temp, data = nest),
  glmer(SN~fox_dens+cumul_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")))

# Get goodness-of-fit and AIC
sem.fit(Mbibi, nest, conditional = T)
sem.coefs(Mbibi, nest)

sem.model.fits(Mbibi)

#Graph drawing
MbibiGRAPH<-DAG(
  fox_dens~lmg_abun+cumul_prec+MEAN_temp,
  SN~fox_dens+cumul_prec+MEAN_temp
)
drawGraph(MbibiGRAPH, adjust = T)


##### Monte-Carlo test on Bibi model ####
#install.packages("devtools")
library(devtools)
# installer CauseAndCorrelation depuis le GitHub de Bill
#install_github("BillShipley/CauseAndCorrelation")
library(CauseAndCorrelation)
?MCX2
MCX2(model.df = 2, n.obs = 3807, model.chi.square = 5.99 )

Mbibi1<-list(
  lm(fox_dens~lmg_abun+cumul_prec+MEAN_temp, data = nest),
  glmer(SN~fox_dens+cumul_prec+MEAN_temp+ (1|AN), data=nest, family=binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(Mbibi1, nest, conditional = T,corr.errors = "MEAN_temp ~~ cumul_prec")
#corr.errors = "MEAN_temp~~cumul_prec"2
# dit que MEAN_temp et cumul_prec sont lié par une latente
# tu peux aussi spécifier un vecteur de variables , genre c("x~~Y","Y~~Z"
sem.coefs(Mbibi1, nest)

sem.model.fits(Mbibi1)
sem.basis.set(Mbibi1)

####### M2 ######
M3f4<-list(
  lm(fox_dens~lmg_abun+winAO+cumul_prec+MEAN_temp, data = nest),
  glmer(SN~fox_dens+cumul_prec+MEAN_temp+lmg_abun+ (1|AN), data=nest, family=binomial(link = "logit")),
  lm(lmg_abun~winAO+cumul_prec+MEAN_temp, data = nest),
  lme(cumul_prec~MEAN_temp, data = nest, random = ~1|AN))
sem.fit(M3f4, nest, conditional = T,corr.errors = "MEAN_temp ~~ cumul_prec")


##### à la mitaine ####
summary(lm(lmg_abunMEAN_temp, data = nest))

cor.test(nest$lmg_abun,nest$MEAN_temp)
summary(lm(nest$lmg_abun~nest$MEAN_temp))
cor.test(nest$lmg_abun,nest$cumul_prec)
cor.test(nest$cumul_prec,nest$MEAN_temp)

summary(lm(lmg_abun~fox_dens+MEAN_temp+cumul_prec+SN, data = nest))
summary(glmer(SN~fox_dens+MEAN_temp+cumul_prec+lmg_abun+(1|AN), data = nest, family = binomial(link = "logit")))

glmer(SN~fox_dens+MEAN_temp+cumul_prec+lmg_abun+(1|AN), data = nest, family = binomial(link = "logit"))

glmer(SN~fox_dens+cumul_prec+MEAN_temp+lmg_abun+ (1|AN), data=nest, family=binomial(link = "logit"))
