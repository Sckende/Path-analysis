#### TEST MICRO ####

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
mC1$prop_fox <- round(mC1$breed_dens/mC1$monit_dens, digits = 3)
mC1$prop_fox_dens <- mC1$prop_fox_dens/100

head(mC1)


# Comparaison des deux méthodes de glm possibles
# Les deux outputs doivent être identiques ...
# dans le cbind() = nombre de succèes, nombre d'échec
jo <- glm(cbind(breed_dens, monit_dens-breed_dens) ~ lmg_C1 + cumul_prec + MEAN_temp, family = binomial, data = mC1)
summary(jo)

# Utilisation de weights = nombre de tanières suivies au total
jo2 <- glm(prop_fox_dens ~ lmg_C1 + cumul_prec + MEAN_temp, family = binomial, weights = monit_dens, data = mC1)
summary(jo2)

par(mfrow = c(2,2))
plot(jo)


#### Liste de modèles à corriger ####

#### ro2*** #####
#Modele de piste
ro2 <- list(
  glm(prop_fox_dens ~ lmg_C1 + winAO + cumul_prec + MEAN_temp, weights = monit_dens, data = mC1),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")),
  lm(lmg_C1 ~ winAO + MEAN_temp + cumul_prec, data = mC1))
# Get goodness-of-fit and AIC
sem.fit(ro2, mC1, conditional = T)

#NO significant missing paths
sem.coefs(ro2, mC1)
#sem.plot(ro2, mC1, show.nonsig = T)

#### ro2a *** ####
#Modele de piste
ro2a <- list(
  glm(prop_fox_dens ~ lmg_C1 + cumul_prec + MEAN_temp, weights = monit_dens, data = mC1),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2a, mC1, conditional = T)

#NO significant missing paths
sem.coefs(ro2a, mC1)

##### Modèle ro2a SCALE #####
# Normalisation des données
n <- mC1[, c(4, 12, 15, 16, 17)]
nsc <- scale(n)
# Ajout des variable AN, prop_fox_dens, SN & monit_dens
nsc <- cbind(mC1[, c(1, 24, 25)], nsc)

#Modele de piste
ro2aSC <- list(
  glm(prop_fox_dens ~ lmg_C1 + cumul_prec + MEAN_temp, weights = monit_dens, data = nsc),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = nsc, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2aSC, nsc, conditional = T)

#NO significant missing paths
sem.coefs(ro2aSC, nsc)
sem.model.fits(ro2aSC) #calcul des R2

#### ro2b *** ####

#Modele de piste
ro2b <- list(
  glm(prop_fox_dens ~ lmg_C1 + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = mC1),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2b, mC1, conditional = T)

#NO significant missing paths
sem.coefs(ro2b, mC1)

##### Modèle ro2b SCALE #####
#Modele de piste
ro2bSC <- list(
  glm(prop_fox_dens ~ lmg_C1 + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = nsc),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = nsc, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2bSC, nsc, conditional = T)

#NO significant missing paths
sem.coefs(ro2bSC, nsc)
sem.model.fits(ro2bSC) #calcul des R2

#### ro2c *** ####
#Modele de piste
ro2c <- list(
  glm(prop_fox_dens ~ lmg_C1 + cumul_prec + MEAN_temp + winAO + sprAO, weights = monit_dens, data = mC1),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2c, mC1, conditional = T)

#NO significant missing paths
sem.coefs(ro2c, mC1)

#### ro2d ####
#Modele de piste
ro2d <- list(
  glm(prop_fox_dens ~ lmg_C1 + cumul_prec + MEAN_temp + winAO + sumAO + sprAO, weights = monit_dens, data = mC1),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2d, mC1, conditional = T)

#Significant missing paths
sem.coefs(ro2d, mC1)

#### ro2e *** ####
#Modele de piste
ro2e <- list(
  glm(prop_fox_dens ~ lmg_C1 + cumul_prec + MEAN_temp + winAO + sumAO + sprAO, weights = monit_dens, data = mC1),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + sumAO + (1|AN), data = mC1, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2e, mC1, conditional = T)

#NO significant missing paths
sem.coefs(ro2e, mC1)


#### Vérification des résidus ####

# Changer la valeur de Y par la variable à expliquer et reg par le glm
Y <- mC1$prop_fox_dens
reg <- glm(prop_fox_dens ~ lmg_C1 + cumul_prec + MEAN_temp, weights = monit_dens, data = mC1)


summary(reg)
dev.off()

par(mfrow = c(2,2))
plot(reg)

dev.off()

plot(predict(reg),residuals(reg),col=c("blue","red")[1+Y])
abline(h=0,lty=2,col="grey")

lines(lowess(predict(reg),residuals(reg)),col="black",lwd=2)

require(splines)
rl <- lm(residuals(reg) ~ bs(predict(reg),8)) 
rl <- loess(residuals(reg) ~ predict(reg))

y <- predict(rl, se=TRUE)
segments(predict(reg), y$fit + 2*y$se.fit, predict(reg), y$fit - 2*y$se.fit, col="green") 


# Relationship with the first explicative variable
X1 <- mC1$cumul_prec

plot(X1, residuals(reg), col=c("blue","red")[1+Y])
lines(lowess(X1, residuals(reg)), col="black", lwd=2)
lines(lowess(X1[Y==0], residuals(reg)[Y==0]), col="blue")
lines(lowess(X1[Y==1], residuals(reg)[Y==1]), col="red")
abline(h=0, lty=2, col="grey")
