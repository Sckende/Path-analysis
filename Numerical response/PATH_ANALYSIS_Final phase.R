########## Path analysis - Numerical response ###############
rm(list = ls()) #clean R memory
setwd(dir = "/Users/nicolas/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")


# RAPPEL
#winAO = November to April
#sprAO = 20 May to 20 June
#esumAO = 21 June to 15 July
#lsumAO = 16 July to 15 August
#sumAO = 21 June to 15 August
#AOnidif = 4 June to 19 July

#f <- read.table("Path analysis_data 3.txt", sep = ",", dec = ".", h = T)
#f1 <- read.table("Path analysis_data 3bis.txt", sep = ",", dec = ".", h = T)

#s1 <- summary(f)
#s2 <- summary(f1)

# Les deux jeux de donnees ont ete compares et sont identiques. A partir de là, utilisation de "Path analysis_data 3bis" car plus complet en terme de variables : les variables prop_f0x_dens, sumAO et AOnidif ont ete rajoutees
rm(list = ls()) #clean R memory
f <- read.table("Path analysis_data 3bis.txt", sep = ",", dec = ".", h = T)

# Premiere etape : retrait des variables inutiles pour les analyses de piste et qui pourraient engendrer des NAs (et donc de la perte de donnees) inutilement
# Retrait de : X, NO, HABITAT, INI, INICOD, ECLO, EXPO, PRED, UTM.E, UTM.N, NB_VIS, NEST.TYPE, FIN, SD_TEMP, SD_prec, prim_prod, nest_density, clutch_size, egg_abun, ratio_JUVad, brood_size, nest_succ

# BDD a utiliser si analyses avec lmg_C2 ou lmg_C1_C2
mC2 <- f[,-c(1, 3:8, 10:14, 16, 18, 25, 35, 42:47 )]
mC2 <- na.omit(mC2)
summary(mC2)
pairs(mC2)

# BDD a utiliser si analyses avec lmg_C1
mC1 <- f[,-c(1, 3:8, 10:14, 16, 18, 25, 30, 32, 34, 35, 42:47 )]
summary(mC1)


#Package nécessaires
require(nlme)
require(lme4)
require(piecewiseSEM)
require(ggm)

# Fonction pour impression coef correlation et p.value sur les pair.panels avec variables utilisees dans analyses de piste
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}


#### Path analysis with Path analysis_data 3 #####
#i.e. lmg_C2 et fox_dens avec le jeu de donnees mC2

#Exploration visuelle des donnees
X11()
pairs(mC2[,c(15, 17, 20, 12, 4, 26)], upper.panel = panel.cor)

#Modele de piste
M3f6 <- list(
  lm(fox_dens ~ lmg_C2 + winAO + cumul_prec + MEAN_temp, data = mC2),
  glmer(SN ~ fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC2, family = binomial(link = "logit")),
  lm(lmg_C2 ~ winAO + MEAN_temp + cumul_prec, data = mC2))
# Get goodness-of-fit and AIC
sem.fit(M3f6, mC2, conditional = T, corr.errors = "MEAN_temp ~~ cumul_prec")

# No significant missing paths
sem.coefs(M3f6, mC2)
sem.plot(M3f6, mC2, show.nonsig = T)
#Ceci est le dernier modele selectionne. Le probleme est que la variable pour les renards est mauvaise car il s'agit des abondances des tanieres repro et non pas des proportions. De plus il serait interessant de remplacer lmg_C2 par lmg_C1 ou lmg_C1_C2 histoire que ca corresponde plus avec la variable des renards.


#### Même modele que pcdt en remplacant fox_dens par prop_fox_dens #####
#remplacement de fox_dens par prop_fox_dens

#Exploration visuelle des donnees
X11()
pairs(mC2[,c(16, 17, 20, 12, 4, 26)], upper.panel = panel.cor)

#Modele de piste
ro <- list(
  lm(prop_fox_dens ~ lmg_C2 + winAO + cumul_prec + MEAN_temp, data = mC2),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC2, family = binomial(link = "logit")),
  lm(lmg_C2 ~ winAO + MEAN_temp + cumul_prec, data = mC2))
# Get goodness-of-fit and AIC
sem.fit(ro, mC2, conditional = T, corr.errors = "MEAN_temp ~~ cumul_prec")

#Significant missing paths !!!!
#sem.coefs(ro, mC2)
#sem.plot(ro, mC2, show.nonsig = T)
##### A RETRAVAILLER ####

#### Même modele que pcdt en remplacant lmg_C2 par lmg_C1_C2 - ro1*** #####

#Exploration visuelle des donnees
X11()
pairs(mC2[,c(16, 19, 20, 12, 4, 26)], upper.panel = panel.cor)

#Modele de piste
ro1 <- list(
  lm(prop_fox_dens ~ lmg_C1_C2 + winAO + cumul_prec + MEAN_temp, data = mC2),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC2, family = binomial(link = "logit")),
  lm(lmg_C1_C2 ~ winAO + MEAN_temp + cumul_prec, data = mC2))
# Get goodness-of-fit and AIC
sem.fit(ro1, mC2, conditional = T, corr.errors = "MEAN_temp ~~ cumul_prec")

#NO significant missing paths
sem.coefs(ro1, mC2)
sem.plot(ro1, mC2, show.nonsig = T)
##### Modele valide mais refaire les etapes ulterieures ####

#### Même modele que pcdt en remplacant lmg_C1_C2 par lmg_C1 - ro2*** #####
#Changement de jeu de donnees. Utilisation de mC1 qui contient plus d'annees

#Exploration visuelle des donnees
X11()
pairs(mC1[,c(15, 16, 17, 12, 4, 23)], upper.panel = panel.cor)

#Modele de piste
ro2 <- list(
  lm(prop_fox_dens ~ lmg_C1 + winAO + cumul_prec + MEAN_temp, data = mC1),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")),
  lm(lmg_C1 ~ winAO + MEAN_temp + cumul_prec, data = mC1))
# Get goodness-of-fit and AIC
sem.fit(ro2, mC1, conditional = T, corr.errors = "MEAN_temp ~~ cumul_prec")

#NO significant missing paths
sem.coefs(ro2, mC1)
sem.plot(ro2, mC1, show.nonsig = T)
##### Modele valide mais refaire les etapes ulterieures ####

#### Exploration du modele ro1 avec le jeu de donnees mC2 ####
#Modele de piste
ro1a <- list(
  lm(prop_fox_dens ~ lmg_C1_C2 + winAO + cumul_prec + MEAN_temp, data = mC2),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC2, family = binomial(link = "logit")),
  lm(lmg_C1_C2 ~ winAO + MEAN_temp + cumul_prec, data = mC2),
  lme(MEAN_temp ~ esumAO, random = ~ 1|AN, data = mC2),
  lme(cumul_prec ~ esumAO, random = ~ 1|AN, data = mC2))
# Get goodness-of-fit and AIC
sem.fit(ro1a, mC2, conditional = T)
#significant missing paths ?
sem.coefs(ro1a, mC2)
sem.plot(ro1, mC2, show.nonsig = T)

#### Exploration du modele ro2 avec le jeu de donnees mC1 ####
#Modele de piste
ro2 <- list(
  lm(prop_fox_dens ~ lmg_C1 + cumul_prec + MEAN_temp, data = mC1),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")),
  lm(lmg_C1 ~ winAO + MEAN_temp, data = mC1))
# Get goodness-of-fit and AIC
sem.fit(ro2, mC1, conditional = T, corr.errors = "MEAN_temp ~~ cumul_prec")

#NO significant missing paths
sem.coefs(ro2, mC1)
sem.plot(ro2, mC1, show.nonsig = T)
