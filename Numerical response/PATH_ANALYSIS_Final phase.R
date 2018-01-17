
rm(list = ls()) #clean R memory
setwd(dir = "/Users/nicolas/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")


# COMPARER F ET F1 AVANT DE FAIRE N'IMPORTE QUOI D'AUTRE !!!!!!!!!!


f <- read.table("Path analysis_data 3.txt", sep = ",", dec = ".", h = T)
f1 <- read.table("Path analysis_data 3bis.txt", sep = ",", dec = ".", h = T)

summary(f)
summary(f1)

#Package nécessaires
require(nlme)
require(lme4)
require(piecewiseSEM)
require(ggm)

#### Path analysis with Path analysis_data 3 #####
nest<-na.omit(f)
M3f6<-list(
  lm(fox_dens~lmg_abun+winAO+cumul_prec+MEAN_temp, data = f),
  glmer(SN~fox_dens+cumul_prec+MEAN_temp+ (1|AN), data=f, family=binomial(link = "logit")),
  lm(lmg_abun~winAO+MEAN_temp+cumul_prec, data = f))
# Get goodness-of-fit and AIC
sem.fit(M3f6, f, conditional = T, corr.errors = "MEAN_temp ~~ cumul_prec")

# No significant missing paths
sem.coefs(M3f6, f)
sem.plot(M3f6, f, show.nonsig = T)

#### Path analysis with Path analysis_data 3bis #####
#remplacement de lmg_abun par lmg_C1
#remplacement de fox_dens par prop_fox_dens

f2 <- f1[,-c(7,14,32,34,35)] #retrait de variables non necessaire afin d'eviter les NAs (ECLO, NEST.TYPE, lmg_C2, lmg_C1_C2, prim_prod)

M3f6autre<-list(
  lm(prop_fox_dens~lmg_C1+winAO+cumul_prec+MEAN_temp, data = f2),
  glmer(SN~prop_fox_dens+cumul_prec+MEAN_temp+ (1|AN), data=f2, family=binomial(link = "logit")),
  lm(lmg_C1~winAO+MEAN_temp+cumul_prec, data = f2))
# Get goodness-of-fit and AIC
sem.fit(M3f6autre, f2, conditional = T, corr.errors = "MEAN_temp ~~ cumul_prec")

# No significant missing paths
sem.coefs(M3f6autre, f2)
sem.plot(M3f6autre, f2, show.nonsig = T)

#### Path analysis with Path analysis_data 3bis #####
#remplacement de lmg_abun par lmg_C2
#remplacement de fox_dens par prop_fox_dens

f3 <- f1[,-c(7,14,34,35)] #retrait de variables non necessaire afin d'eviter les NAs (ECLO, NEST.TYPE, lmg_C1_C2, prim_prod)
f3 <- na.omit(f3)

M3f6autre2<-list(
  lm(prop_fox_dens~lmg_C2+winAO+cumul_prec+MEAN_temp, data = f3),
  glmer(SN~prop_fox_dens+cumul_prec+MEAN_temp+ (1|AN), data=f3, family=binomial(link = "logit")),
  lm(lmg_C2~winAO+MEAN_temp+cumul_prec, data = f3))
# Get goodness-of-fit and AIC
sem.fit(M3f6autre2, f3, conditional = T, corr.errors = "MEAN_temp ~~ cumul_prec")

# Significant missing paths
sem.coefs(M3f6autre2, f3)
sem.plot(M3f6autre2, f3, show.nonsig = T)


##### Exploration de f2 #####
f2 <- f1[,-c(7,14,32,34,35)] #retrait de variables non necessaire afin d'eviter les NAs (ECLO, NEST.TYPE, lmg_C2, lmg_C1_C2, prim_prod)

# New initial modele
#winAO = November to April
#sprAO = 20 May to 20 June
#esumAO = 21 June to 15 July
#lsumAO = 16 July to 15 August
#sumAO = 21 June to 15 August
#AOnidif = 4 June to 19 July
hope<-list(
  lm(prop_fox_dens~lmg_C1+winAO+sprAO, data = f2),
  glmer(SN~prop_fox_dens+cumul_prec+MEAN_temp+ (1|AN), data=f2, family=binomial(link = "logit")),
  lm(lmg_C1~winAO+sprAO, data = f2),
  lme(cumul_prec~AOnidif, data = f2, random = ~1|AN),
  lme(MEAN_temp~AOnidif, data = f2, random = ~1|AN))
# Get goodness-of-fit and AIC
sem.fit(hope, f2, conditional = T)

# No significant missing paths
sem.coefs(hope, f2)
sem.plot(hope, f2, show.nonsig = T)