setwd(dir = "C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")

rm(list = ls()) #clean R memory
f <- read.table("Path analysis_data 3bis.txt", sep = ",", dec = ".", h = T)
# BDD a utiliser si analyses avec lmg_C1
test <- f[,-c(1, 3:8, 10:14, 16, 18, 25, 30, 32, 34, 35, 42:46 )]
summary(test)

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

test$monit_dens <- fox$monit_dens[match(test$AN, fox$year)]
test$breed_dens <- fox$litter_number[match(test$AN, fox$year)]
test$prop_fox <- round(test$breed_dens/test$monit_dens, digits = 3)
test$prop_fox_dens <- test$prop_fox_dens/100

head(test)

#### Ajout de abondance de lemmings C1 CORRIGE (cf courriel Gilles - 12 juin 2018) ####
lmg <- read.table("LEM_1993-2017.txt", sep = "\t", dec = ",", h = T)
test$lmg_C1_CORR <- lmg$LMG_C1_CORR[match(test$AN, lmg$YEAR)]

# Creation de la variable goose_lag1 pour tester la relation entre la proportion de renard en repro et le NS des goose l'annee suivante (on s'attend a un effet negatif sur les oies pour une augmentation de la proportion des renards a se reproduire)

for(i in unique(test$AN)) {
  test$goose_lag1[test$AN == i] <- test$nest_succ[test$AN == i + 1] 
}

# Retrait des NAs apparus avec goose_lag1
test <- na.omit(test)


#### Nouveaux modeles de piste ####

#Modele de piste - VERSION COMPLETE
mo1 <- list(
  glm(prop_fox_dens ~ lmg_C1_CORR + winAO + sprAO, weights = monit_dens, data = test),
  glmer(SN ~ cumul_prec + MEAN_temp + sprAO + (1|AN), data = test, family = binomial(link = "logit")),
  lm(lmg_C1_CORR ~ winAO, data = test),
  lmer(MEAN_temp ~ sumAO + (1|AN), data = test),
  lmer(cumul_prec ~ sumAO + (1|AN), data = test),
  lm(goose_lag1 ~ prop_fox_dens, data = test))
# Get goodness-of-fit and AIC
sem.fit(mo1, test, conditional = T)

#NO significant missing paths
sem.coefs(mo1, test)
#sem.plot(mo1, test, show.nonsig = T)


#Modele de piste - VERSION SIMPLE
mo2 <- list(
  glm(prop_fox_dens ~ lmg_C1_CORR, weights = monit_dens, data = test),
  glmer(SN ~ cumul_prec + MEAN_temp + (1|AN), data = test, family = binomial(link = "logit")),
  lm(goose_lag1 ~ prop_fox_dens, data = test))
# Get goodness-of-fit and AIC
sem.fit(mo2, test, conditional = T)

#NO significant missing paths
sem.coefs(mo2, test)
#sem.plot(mo1, test, show.nonsig = T)