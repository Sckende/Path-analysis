########## Path analysis - Numerical response ###############
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
# Retrait de : X, NO, HABITAT, INI, INICOD, ECLO, EXPO, PRED, UTM.E, UTM.N, NB_VIS, NEST.TYPE, FIN, SD_TEMP, SD_prec, nest_density, clutch_size, egg_abun, ratio_JUVad, brood_size, nest_succ

# BDD a utiliser si analyses avec lmg_C2 ou lmg_C1_C2
mC2_pp <- f[,-c(1, 3:8, 10:14, 16, 18, 25, 42:47 )] # si modeles avec utilisation de la variable prim_prod
mC2 <- f[,-c(1, 3:8, 10:14, 16, 18, 25, 35, 42:47 )]
mC2 <- na.omit(mC2)
summary(mC2)
pairs(mC2)

# BDD a utiliser si analyses avec lmg_C1
mC1 <- f[,-c(1, 3:8, 10:14, 16, 18, 25, 30, 32, 34, 35, 42:47 )]
mC1_pp <- f[,-c(1, 3:8, 10:14, 16, 18, 25, 30, 32, 34, 42:47 )] # si modeles avec utilisation de la variable prim_prod
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
pairs(mC2[,c(16, 17, 20, 12, 4, 26, 27)], upper.panel = panel.cor)

#### Modele ro *** ####
ro <- list(
  lm(prop_fox_dens ~ lmg_C2 + winAO + cumul_prec + MEAN_temp, data = mC2),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC2, family = binomial(link = "logit")),
  lm(lmg_C2 ~ winAO + MEAN_temp + cumul_prec, data = mC2))
# Get goodness-of-fit and AIC
sem.fit(ro, mC2, conditional = T, corr.errors = "MEAN_temp ~~ cumul_prec")

#NO Significant missing path !!!!
sem.coefs(ro, mC2)
sem.plot(ro, mC2, show.nonsig = T)


#### Même modele que pcdt en remplacant lmg_C2 par lmg_C1_C2 - ro1*** #####

#Exploration visuelle des donnees
X11()
pairs(mC2[,c(16, 19, 20, 12, 4, 26, 27)], upper.panel = panel.cor)

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

#Standardisation des données
nene<-mC2[,c(16, 19, 20, 12, 4, 26, 27)]
nenescale<-scale(nene[,1:5])
nenescale<-cbind(mC2[,c(1, 26, 27)], as.data.frame(nenescale))
head(nenescale)

##### Modèle ro1 SCALE#####
#Modele de piste
ro1sc <- list(
  lm(prop_fox_dens ~ lmg_C1_C2 + winAO + cumul_prec + MEAN_temp, data = nenescale),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = nenescale, family = binomial(link = "logit")),
  lm(lmg_C1_C2 ~ winAO + MEAN_temp + cumul_prec, data = nenescale))
# Get goodness-of-fit and AIC
sem.fit(ro1sc, nenescale, conditional = T, corr.errors = "MEAN_temp ~~ cumul_prec")

#NO significant missing paths
sem.coefs(ro1sc, nenescale)
#sem.plot(ro1sc, nenescale, show.nonsig = T)
sem.model.fits(ro1sc) #calcul des R2

####### MODELES AVEC LMG QUANTITATIF ##########

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


####### MODELES AVEC LMG CATEGORIE ##########


#### Nouveau modele en remplacant lmg_C1 par lmg_year - ro3*** #####
#Changement de jeu de donnees. Utilisation de mC1 qui contient plus d'annees

#Exploration visuelle des donnees
X11()
pairs(mC1[,c(15, 16, 17, 12, 4, 23, 24)], upper.panel = panel.cor)

#Modele de piste
ro3 <- list(
  lm(prop_fox_dens ~ lmg_year + cumul_prec + MEAN_temp, data = mC1),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro3, mC1, conditional = T)

#NO significant missing paths
sem.coefs(ro3, mC1)
sem.plot(ro3, mC1, show.nonsig = T)

#### Idem + winAO - ro3a*** #####
#Changement de jeu de donnees. Utilisation de mC1 qui contient plus d'annees

#Exploration visuelle des donnees
X11()
pairs(mC1[,c(15, 16, 17, 12, 4, 23, 24)], upper.panel = panel.cor)

#Modele de piste
ro3a <- list(
  lm(prop_fox_dens ~ lmg_year + cumul_prec + MEAN_temp + winAO, data = mC1),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro3a, mC1, conditional = T)

#NO significant missing paths
sem.coefs(ro3a, mC1)
sem.plot(ro3a, mC1, show.nonsig = T)

#### Idem + sprAO - ro3b*** #####
#Changement de jeu de donnees. Utilisation de mC1 qui contient plus d'annees

#Exploration visuelle des donnees
X11()
pairs(mC1[,c(15, 16, 17, 12, 4, 23, 24)], upper.panel = panel.cor)

#Modele de piste
ro3b <- list(
  lm(prop_fox_dens ~ lmg_year + cumul_prec + MEAN_temp + winAO + sprAO, data = mC1),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro3b, mC1, conditional = T)

#NO significant missing paths
sem.coefs(ro3b, mC1)
sem.plot(ro3b, mC1, show.nonsig = T)

#### Idem ro3a + sumAO - ro3c*** #####
#Changement de jeu de donnees. Utilisation de mC1 qui contient plus d'annees

#Exploration visuelle des donnees
X11()
pairs(mC1[,c(15, 16, 17, 12, 4, 23, 24)], upper.panel = panel.cor)

#Modele de piste
ro3c <- list(
  lm(prop_fox_dens ~ lmg_year + cumul_prec + MEAN_temp + winAO + sumAO + sprAO, data = mC1),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro3c, mC1, conditional = T)

#NO significant missing paths
sem.coefs(ro3c, mC1)
sem.plot(ro3c, mC1, show.nonsig = T)

#### Idem ro3c + sumAO -- SN - ro3c1*** #####
#Changement de jeu de donnees. Utilisation de mC1 qui contient plus d'annees

#Exploration visuelle des donnees
X11()
pairs(mC1[,c(15, 16, 17, 12, 4, 23, 24)], upper.panel = panel.cor)

#Modele de piste
ro3c1 <- list(
  lm(prop_fox_dens ~ lmg_year + cumul_prec + MEAN_temp + winAO + sumAO + sprAO, data = mC1),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + sumAO + (1|AN), data = mC1, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro3c1, mC1, conditional = T)

#NO significant missing paths
sem.coefs(ro3c1, mC1)
sem.plot(ro3c1, mC1, show.nonsig = T)


#### Idem + sprAO - ro3d*** #####
#Changement de jeu de donnees. Utilisation de mC1 qui contient plus d'annees

#Exploration visuelle des donnees
X11()
pairs(mC1[,c(15, 16, 17, 12, 4, 23, 24)], upper.panel = panel.cor)

#Modele de piste
ro3d <- list(
  lm(prop_fox_dens ~ lmg_year + cumul_prec + MEAN_temp + winAO + sumAO + sprAO, data = mC1),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + sumAO + (1|AN), data = mC1, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro3d, mC1, conditional = T)

#NO significant missing paths
sem.coefs(ro3d, mC1)
sem.plot(ro3d, mC1, show.nonsig = T)

#### Idem - sprAO -- fox - ro3e*** #####
#Changement de jeu de donnees. Utilisation de mC1 qui contient plus d'annees

#Exploration visuelle des donnees
X11()
pairs(mC1[,c(15, 16, 17, 12, 4, 23, 24)], upper.panel = panel.cor)

#Modele de piste
ro3e <- list(
  lm(prop_fox_dens ~ lmg_year + cumul_prec + MEAN_temp + winAO + sumAO, data = mC1),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + sumAO + (1|AN), data = mC1, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro3e, mC1, conditional = T)

#NO significant missing paths
sem.coefs(ro3e, mC1)
sem.plot(ro3e, mC1, show.nonsig = T)

#### Meme modele ro3 + prim_prod (BDD "mC1_pp") - ro4 ####
#Exploration visuelle des donnees
X11()
mC1_pp <- na.omit(mC1_pp)
pairs(mC1_pp[,c(15, 16, 17, 18, 12, 4, 24, 25)], upper.panel = panel.cor)
#Modele de piste
ro4 <- list(
  lm(prop_fox_dens ~ lmg_year + cumul_prec + MEAN_temp, data = mC1_pp),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp +prim_prod + (1|AN), data = mC1_pp, family = binomial(link = "logit")),
  lm(prim_prod ~ cumul_prec + MEAN_temp, data = mC1_pp))
# Get goodness-of-fit and AIC
sem.fit(ro4, mC1_pp, conditional = T, corr.errors = "MEAN_temp ~~ cumul_prec")

#NO significant missing paths
sem.coefs(ro4, mC1_pp)
sem.plot(ro4, mC1_pp, show.nonsig = T)

#Pas de relation avec le SN mais lien fort avec prop_fox_dens ==> ABANDON

#### Meme modele prim_prod (BDD "mC2_pp") - ro5 ####
#Exploration visuelle des donnees
X11()
mC2_pp <- na.omit(mC2_pp)
pairs(mC2_pp[,c(16, 17, 20, 12, 4, 27, 28)], upper.panel = panel.cor)
#Modele de piste
ro5 <- list(
  lm(prop_fox_dens ~ lmg_year + cumul_prec + MEAN_temp, data = mC2_pp),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp +prim_prod + (1|AN), data = mC2_pp, family = binomial(link = "logit")),
  lm(prim_prod ~ cumul_prec + MEAN_temp, data = mC2_pp))
# Get goodness-of-fit and AIC
sem.fit(ro5, mC2_pp, conditional = T, corr.errors = "MEAN_temp ~~ cumul_prec")

#NO significant missing paths
sem.coefs(ro5, mC2_pp)
sem.plot(ro5, mC2_pp, show.nonsig = T)
#Pas de relation avec le SN mais lien fort avec prop_fox_dens ==> ABANDON

#### Modele n'utilisant que les variables climatiques globales (AO) - ro7*** ####
names(f)
glo <- f[, c(2, 31, 33, 36:41, 48, 49)]
pairs(glo, upper.panel = panel.cor)
#Modele de piste
ro7 <- list(
  lm(prop_fox_dens ~ lmg_C1 + winAO + sumAO + sprAO, data = glo),
  glmer(SN ~ prop_fox_dens + sumAO + sprAO + (1|AN), data = glo, family = binomial(link = "logit")),
  lm(lmg_C1 ~ sprAO + winAO + sumAO , data = glo)
)

# Get goodness-of-fit and AIC
sem.fit(ro7, glo, conditional = T)
sem.coefs(ro7, glo)

#Tres interessant. Possibilité d'émettre trois modeles differents selon si ajout de liens entre SN et winAO/sprAO. AIC comparables. Relation forte entre le climat globale et la productivite des especes en Arctique toute l'annee

#### Idem sans sprAO -- SN - ro7a*** ####
names(f)
glo <- f[, c(2, 31, 33, 36:41, 48, 49)]
pairs(glo, upper.panel = panel.cor)
#Modele de piste
ro7a <- list(
  lm(prop_fox_dens ~ lmg_C1 + winAO + sumAO + sprAO, data = glo),
  glmer(SN ~ prop_fox_dens + sumAO + (1|AN), data = glo, family = binomial(link = "logit")),
  lm(lmg_C1 ~ sprAO + winAO + sumAO , data = glo)
)

# Get goodness-of-fit and AIC
sem.fit(ro7a, glo, conditional = T)
sem.coefs(ro7a, glo)

##### BROUILLON #####
#### Exploration du modele ro1 ####
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

#### Exploration du modele ro2 ####
#Modele de piste
ro2a <- list(
  lm(prop_fox_dens ~ lmg_C1 + cumul_prec + MEAN_temp, data = mC1),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")),
  lm(lmg_C1 ~ winAO + MEAN_temp, data = mC1))
# Get goodness-of-fit and AIC
sem.fit(ro2a, mC1, conditional = T, corr.errors = "MEAN_temp ~~ cumul_prec")

#NO significant missing paths
sem.coefs(ro2a, mC1)
sem.plot(ro2a, mC1, show.nonsig = T)

####### Possibilite de couper les n pour utiliser le meme jeu de donnees pour les trois modeles possibles (ro1, ro2, ro2a) et ainsi pouvoir les comparer entre eux. Mais attention , la comparaison peut se faire a plusieurs niveuax : entre le nombre de pistes engagees dans les modeles ou entre les variables utilisees dans les modeles


#### Cas de comparaison des trois modeles entre eux ####
#ici les trois modeles sont bases sur le meme jeu de donnees (mC2 - le plus petit, n = 3807) afin de permettre leur comparaison

#### Modele ro1 #### 
ro1 <- list(
  lm(prop_fox_dens ~ lmg_C1_C2 + winAO + cumul_prec + MEAN_temp, data = mC2),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC2, family = binomial(link = "logit")),
  lm(lmg_C1_C2 ~ winAO + MEAN_temp + cumul_prec, data = mC2))

# Get goodness-of-fit and AIC
sem.fit(ro1, mC2, conditional = T, corr.errors = "MEAN_temp ~~ cumul_prec")
#NO significant missing paths
sem.coefs(ro1, mC2)
sem.plot(ro1, mC2, show.nonsig = T)

#Standardisation des données
nene<-mC2[,c(16, 17, 18, 19, 20, 12, 4, 26)]
nenescale<-scale(nene[,1:7])
nenescale<-cbind(mC2[,c(1, 26)], as.data.frame(nenescale))
names(nenescale)[c(1,2)] <- c("AN", "SN")
head(nenescale)

#### Modèle ro1 SCALED ####
ro1sc <- list(
  lm(prop_fox_dens ~ lmg_C1_C2 + winAO + cumul_prec + MEAN_temp, data = nenescale),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = nenescale, family = binomial(link = "logit")),
  lm(lmg_C1_C2 ~ winAO + MEAN_temp + cumul_prec, data = nenescale))

# Get goodness-of-fit and AIC
sem.fit(ro1sc, nenescale, conditional = T, corr.errors = "MEAN_temp ~~ cumul_prec")
#NO significant missing paths
sem.coefs(ro1sc, nenescale)
#sem.plot(ro1sc, nenescale, show.nonsig = T)
sem.model.fits(ro1sc) #calcul des R2

#### Modele ro2 ####
ro2 <- list(
  lm(prop_fox_dens ~ lmg_C1 + winAO + cumul_prec + MEAN_temp, data = mC2),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC2, family = binomial(link = "logit")),
  lm(lmg_C1 ~ winAO + MEAN_temp + cumul_prec, data = mC2))

# Get goodness-of-fit and AIC
sem.fit(ro2, mC2, conditional = T, corr.errors = "MEAN_temp ~~ cumul_prec")
#NO significant missing paths
sem.coefs(ro2, mC2)
#sem.plot(ro2, mC2, show.nonsig = T)
#certaines paths sont NS

#### Modele ro2 SCALED ####
ro2sc <- list(
  lm(prop_fox_dens ~ lmg_C1 + winAO + cumul_prec + MEAN_temp, data = nenescale),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = nenescale, family = binomial(link = "logit")),
  lm(lmg_C1 ~ winAO + MEAN_temp + cumul_prec, data = nenescale))

# Get goodness-of-fit and AIC
sem.fit(ro2sc, nenescale, conditional = T, corr.errors = "MEAN_temp ~~ cumul_prec")
#NO significant missing paths
sem.coefs(ro2sc, nenescale)
#sem.plot(ro2sc, nenescale, show.nonsig = T)
#NO significant missing paths
#sem.plot(ro2sc, nenescale, show.nonsig = T)
sem.model.fits(ro2sc) #calcul des R2

#### Modele ro2_a ####
ro2_a <- list(
  lm(prop_fox_dens ~ lmg_C1 + cumul_prec + MEAN_temp, data = mC2),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC2, family = binomial(link = "logit")),
  lm(lmg_C1 ~ winAO + MEAN_temp, data = mC2))

# Get goodness-of-fit and AIC
sem.fit(ro2_a, mC2, conditional = T, corr.errors = "MEAN_temp ~~ cumul_prec")
#NO significant missing paths
sem.coefs(ro2_a, mC2)
#sem.plot(ro2_a, mC2, show.nonsig = T)

#### Modele ro2_a SCALED ####
ro2_asc <- list(
  lm(prop_fox_dens ~ lmg_C1 + cumul_prec + MEAN_temp, data = nenescale),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = nenescale, family = binomial(link = "logit")),
  lm(lmg_C1 ~ winAO + MEAN_temp, data = nenescale))

# Get goodness-of-fit and AIC
sem.fit(ro2_asc, nenescale, conditional = T, corr.errors = "MEAN_temp ~~ cumul_prec")
#NO significant missing paths
sem.coefs(ro2_asc, nenescale)
#sem.plot(ro2_asc, nenescale, show.nonsig = T)
sem.model.fits(ro2_asc) #calcul des R2



ro2 <- list(
  lm(prop_fox_dens ~ lmg_year + winAO + cumul_prec + MEAN_temp, data = mC1),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")),
  lm(lmg_year ~ winAO + MEAN_temp + cumul_prec, data = mC1))
# Get goodness-of-fit and AIC
sem.fit(ro2, mC1, conditional = T)

#NO significant missing paths
sem.coefs(ro2, mC1)



