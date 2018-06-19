setwd(dir = "C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")

rm(list = ls()) #clean R memory
f <- read.table("Path analysis_data 3bis.txt", sep = ",", dec = ".", h = T)
# BDD a utiliser si analyses avec lmg_C1
mC1 <- f[,-c(1, 3:8, 10:14, 16, 18, 25, 30, 32, 34, 35, 42:46 )]
summary(mC1)

#Package nécessaires
require(nlme)
require(lme4)
require(piecewiseSEM)
require(ggm)

# nom des fichiers contenus dans le répertoire
list.files()

#### Association des données de renard pour les proportions dans le glm ####
fox <- read.table("FOX_abundance_Chevallier.txt", h = T, sep = "\t", dec = ",")
head(fox)

mC1$monit_dens <- fox$monit_dens[match(mC1$AN, fox$year)]
mC1$breed_dens <- fox$litter_number[match(mC1$AN, fox$year)]
mC1$prop_fox <- round(mC1$breed_dens/mC1$monit_dens, digits = 3)
mC1$prop_fox_dens <- mC1$prop_fox_dens/100

head(mC1)

#### Ajout de abondance de lemmings C1 CORRIGE (cf courriel Gilles - 12 juin 2018) ####
lmg <- read.table("LEM_1993-2017.txt", sep = "\t", dec = ",", h = T)
mC1$lmg_C1_CORR <- lmg$LMG_C1_CORR[match(mC1$AN, lmg$YEAR)]

# Creation de la variable goose_lag1 pour tester la relation entre la proportion de renard en repro et le NS des goose l'annee suivante (on s'attend a un effet negatif sur les oies pour une augmentation de la proportion des renards a se reproduire)

for(i in unique(mC1$AN)) {
  mC1$goose_lag1[mC1$AN == i] <- mC1$nest_succ[mC1$AN == i + 1] 
}

# Retrait des NAs apparus avec goose_lag1
mC1 <- na.omit(mC1)


#### Nouveau modele de piste ####

#Modele de piste
mo1 <- list(
  glm(prop_fox_dens ~ lmg_C1_CORR + winAO + sprAO, weights = monit_dens, data = mC1),
  glmer(SN ~ cumul_prec + MEAN_temp + sprAO + (1|AN), data = mC1, family = binomial(link = "logit")),
  lm(lmg_C1_CORR ~ winAO, data = mC1),
  lmer(MEAN_temp ~ sumAO + (1|AN), data = mC1),
  lmer(cumul_prec ~ sumAO + (1|AN), data = mC1),
  lm(goose_lag1 ~ prop_fox_dens, data = mC1))
# Get goodness-of-fit and AIC
sem.fit(mo1, mC1, conditional = T)

#NO significant missing paths
sem.coefs(mo1, mC1)
#sem.plot(mo1, mC1, show.nonsig = T)


#Modele de piste
mo2 <- list(
  glm(prop_fox_dens ~ lmg_C1_CORR, weights = monit_dens, data = mC1),
  glmer(SN ~ cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")),
  lm(goose_lag1 ~ prop_fox_dens, data = mC1))
# Get goodness-of-fit and AIC
sem.fit(mo2, mC1, conditional = T)

#NO significant missing paths
sem.coefs(mo2, mC1)
#sem.plot(mo1, mC1, show.nonsig = T)