setwd(dir = "/Users/nicolas/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")

rm(list = ls()) #clean R memory
f <- read.table("Path analysis_data 3bis.txt", sep = ",", dec = ".", h = T)
# BDD a utiliser si analyses avec lmg_C1
mC1 <- f[,-c(1, 3:8, 10:14, 16, 18, 25, 30, 32, 34, 35, 42:47 )]
summary(mC1)

#Package nécessaires
require(nlme)
require(lme4)
require(piecewiseSEM)
require(ggm)

# nom des fichiers contenus dans le répertoire
list.files()

# Association des données de renard pour les proportions dans le glm
fox <- read.table("FOX_abundance_Chevallier.txt", h = T, sep = "\t", dec = ",")
head(fox)

mC1$monit_dens <- fox$monit_dens[match(mC1$AN, fox$year)]
mC1$breed_dens <- fox$litter_number[match(mC1$AN, fox$year)]
mC1$prop_fox_dens <- mC1$breed_dens/mC1$monit_dens
head(mC1)


# Comparaison des deux méthodes de glm possibles
# dans le cbind() = nombre de succèes, nombre d'échec
jo <- glm(cbind(breed_dens, monit_dens-breed_dens) ~ lmg_C1 + cumul_prec + MEAN_temp, family = binomial, data = mC1)
summary(jo)

jo2 <- glm(prop_fox_dens ~ lmg_C1 + cumul_prec + MEAN_temp, family = binomial, weights = monit_dens, data = mC1)
summary(jo2)

par(mfrow = c(2,2))
plot(jo)

#### Modèles corrigés ####
roto <- cbind(ff$breed_dens, ff$monit_dens)

#### ro2a *** ####
#Modele de piste
ro2a <- list(
  glm(roto ~ lmg_C1 + cumul_prec + MEAN_temp, family = binomial, data = ff),
  glmer(SN ~ roto + cumul_prec + MEAN_temp + (1|AN), data = ff, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2a, ff, conditional = T)

#NO significant missing paths
sem.coefs(ro2a, mC1)

#### ro2b *** ####
#Modele de piste
ro2b <- list(
  glm(roto ~ lmg_C1 + cumul_prec + MEAN_temp + winAO, family = binomial, data = ff),
  glmer(SN ~ roto + cumul_prec + MEAN_temp + (1|AN), data = ff, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2b, ff, conditional = T)

#NO significant missing paths
sem.coefs(ro2b, ff)


#### Liste de modèles à corriger ####

#### Même modele que pcdt en remplacant lmg_C1_C2 par lmg_C1 - ro2*** #####
#Changement de jeu de donnees. Utilisation de mC1 qui contient plus d'annees

#Exploration visuelle des donnees
X11()
pairs(mC1[,c(15, 16, 17, 12, 4, 23, 24)], upper.panel = panel.cor)

#Modele de piste
ro2 <- list(
  lm(prop_fox_dens ~ lmg_C1 + winAO + cumul_prec + MEAN_temp, data = mC1),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")),
  lm(lmg_C1 ~ winAO + MEAN_temp + cumul_prec, data = mC1))
# Get goodness-of-fit and AIC
sem.fit(ro2, mC1, conditional = T)

#NO significant missing paths
sem.coefs(ro2, mC1)
#sem.plot(ro2, mC1, show.nonsig = T)

##### Modèle ro2 SCALE#####
#Standardisation des données
nene<-mC1[,c(4, 12, 15 : 17)]
nenescale<-scale(nene[,1:5])
nenescale<-cbind(mC1[,c(1, 24)], as.data.frame(nenescale))
head(nenescale)

#Modele de piste
ro2sc <- list(
  lm(prop_fox_dens ~ lmg_C1 + winAO + cumul_prec + MEAN_temp, data = nenescale),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = nenescale, family = binomial(link = "logit")),
  lm(lmg_C1 ~ winAO + MEAN_temp + cumul_prec, data = nenescale))
# Get goodness-of-fit and AIC
sem.fit(ro2sc, nenescale, conditional = T)

#NO significant missing paths
sem.coefs(ro2sc, nenescale)
#sem.plot(ro1sc, nenescale, show.nonsig = T)
sem.model.fits(ro2sc) #calcul des R2

#### ro2a *** ####
#Modele de piste
ro2a <- list(
  lm(prop_fox_dens ~ lmg_C1 + cumul_prec + MEAN_temp, data = mC1),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2a, mC1, conditional = T)

#NO significant missing paths
sem.coefs(ro2a, mC1)

##### Modèle ro2a SCALE #####
#Modele de piste
ro2aSC <- list(
  lm(prop_fox_dens ~ lmg_C1 + cumul_prec + MEAN_temp, data = nenescale),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = nenescale, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2aSC, nenescale, conditional = T)

#NO significant missing paths
sem.coefs(ro2aSC, nenescale)
sem.model.fits(ro2aSC) #calcul des R2

#### ro2b *** ####

#Modele de piste
ro2b <- list(
  lm(prop_fox_dens ~ lmg_C1 + cumul_prec + MEAN_temp + winAO, data = mC1),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2b, mC1, conditional = T)

#NO significant missing paths
sem.coefs(ro2b, mC1)
##### Modèle ro2b SCALE #####
#Modele de piste
ro2bSC <- list(
  lm(prop_fox_dens ~ lmg_C1 + cumul_prec + MEAN_temp + winAO, data = nenescale),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = nenescale, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2bSC, nenescale, conditional = T)

#NO significant missing paths
sem.coefs(ro2bSC, nenescale)
sem.model.fits(ro2bSC) #calcul des R2
#### ro2c *** ####
#Modele de piste
ro2c <- list(
  lm(prop_fox_dens ~ lmg_C1 + cumul_prec + MEAN_temp + winAO + sprAO, data = mC1),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2c, mC1, conditional = T)

#NO significant missing paths
sem.coefs(ro2c, mC1)

#### ro2d *** ####
#Modele de piste
ro2d <- list(
  lm(prop_fox_dens ~ lmg_C1 + cumul_prec + MEAN_temp + winAO + sumAO + sprAO, data = mC1),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2d, mC1, conditional = T)

#NO significant missing paths
sem.coefs(ro2d, mC1)

#### ro2e *** ####
#Modele de piste
ro2e <- list(
  lm(prop_fox_dens ~ lmg_C1 + cumul_prec + MEAN_temp + winAO + sumAO + sprAO, data = mC1),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + sumAO + (1|AN), data = mC1, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2e, mC1, conditional = T)

#NO significant missing paths
sem.coefs(ro2e, mC1)
