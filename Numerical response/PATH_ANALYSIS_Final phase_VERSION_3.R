#### TEST MICRO ####

setwd(dir = "C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")

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
require(VGAM) # For ZERO-TRUNCATED model

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

dev.off()
#### Ajout de abondance de lemmings C1 CORRIGE (cf courriel Gilles - 12 juin 2018) ####
lmg <- read.table("LEM_1993-2017.txt", sep = "\t", dec = ",", h = T)
mC1$lmg_C1_CORR <- lmg$LMG_C1_CORR[match(mC1$AN, lmg$YEAR)]
#write.table(mC1, "mC1_path_data.txt")

# Verification
plot(tapply(mC1$lmg_C1_CORR, mC1$AN, unique),
     type ="h",
     lwd = 3,
     ylim = c(0, 10),
     col = "olivedrab3")
lines(tapply(mC1$lmg_C1, mC1$AN, unique),
     type ="l",
     lwd = 3,
     col = "darkgoldenrod2")


#### Computation of difference between two consecutive years ####
# Lemming
r <- as.data.frame(tapply(mC1$lmg_C1_CORR, mC1$AN, unique))
r <- cbind(r, 1996:2016)
names(r) <- c("lmg_CORR", "YEAR")
head(r)
d <- diff(r$lmg_CORR, lag = 1); d
summary(d)

# fox
r <- as.data.frame(tapply(mC1$prop_fox_dens, mC1$AN, unique))
r <- cbind(r, 1996:2016)
names(r) <- c("fox_prop", "YEAR")
head(r)
d <- diff(r$fox_prop, lag = 1); d
summary(d)

# goose
g<- read.table("GOOSE_breeding_informations_1995_2017.txt", sep = "\t", dec = ",", h = T)
g <- g[g$YEAR >= 1996 & !g$YEAR == 2017,]; head(g); summary(g)

d <- diff(g$NEST_SUCC, lag = 1); d
summary(d)

#### ro2*** #####
#Modele de piste
ro2 <- list(
  glm(prop_fox_dens ~ lmg_C1_CORR + winAO + cumul_prec + MEAN_temp, weights = monit_dens, data = mC1, family =binomial),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")),
  lm(lmg_C1_CORR ~ winAO + MEAN_temp + cumul_prec, data = mC1))
# Get goodness-of-fit and AIC
sem.fit(ro2, mC1, conditional = T)

#NO significant missing paths
sem.coefs(ro2, mC1)
#sem.plot(ro2, mC1, show.nonsig = T)

#### ro2a *** ####
#Modele de piste
ro2a <- list(
  glm(prop_fox_dens ~ lmg_C1_CORR + cumul_prec + MEAN_temp, weights = monit_dens, data = mC1, family =binomial),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2a, mC1, conditional = T)

#NO significant missing paths
sem.coefs(ro2a, mC1)

##### Modèle ro2a SCALE #####
# Normalisation des données
n <- mC1[, c(4, 12, 15, 17,18, 28)]
nsc <- scale(n)
# Ajout des variable AN, prop_fox_dens, SN & monit_dens
nsc <- cbind(mC1[, c(1, 24, 25)], nsc)

#Modele de piste - CORRECTION LEMMING ABUNDANCE
ro2aSC <- list(
  glm(prop_fox_dens ~ lmg_C1_CORR + cumul_prec + MEAN_temp, weights = monit_dens, data = nsc, family =binomial),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = nsc, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2aSC, nsc, conditional = T)

#NO significant missing paths
sem.coefs(ro2aSC, nsc)
sem.model.fits(ro2aSC) #calcul des R2

#### ro2b *** ####

#Modele de piste
ro2b <- list(
  glm(prop_fox_dens ~ lmg_C1_CORR + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = mC1, family = binomial),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")))


ro2b <- list(
  glm(cbind(breed_dens, monit_dens-breed_dens) ~ lmg_C1_CORR + cumul_prec + MEAN_temp + winAO, family = binomial, data = mC1),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")))

# Without 2000 and with log(lmg)
mC1bis <- mC1[!(mC1$AN == 2000),]
ro2b <- list(
  glm(cbind(breed_dens, monit_dens-breed_dens) ~ log(lmg_C1_CORR) + cumul_prec + MEAN_temp + winAO, family = binomial, data = mC1bis),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1bis, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2b, mC1bis, conditional = T)

#NO significant missing paths
sem.coefs(ro2b, mC1bis)

##### Modèle ro2b SCALE - WITH CORRECTION OF LEMMING ABUNDANCE #####
#Modele de piste
ro2bSC <- list(
  glm(prop_fox_dens ~ lmg_C1_CORR + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = nsc, family =binomial),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = nsc, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2bSC, nsc, conditional = T)

#NO significant missing paths
sem.coefs(ro2bSC, nsc)
sem.model.fits(ro2bSC) #calcul des R2

#### ro2c *** ####
#Modele de piste
ro2c <- list(
  glm(prop_fox_dens ~ lmg_C1_CORR + cumul_prec + MEAN_temp + winAO + sprAO, weights = monit_dens, data = mC1, family =binomial),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2c, mC1, conditional = T)

#NO significant missing paths
sem.coefs(ro2c, mC1)

#### ro2d ####
#Modele de piste
ro2d <- list(
  glm(prop_fox_dens ~ lmg_C1_CORR + cumul_prec + MEAN_temp + winAO + sumAO + sprAO, weights = monit_dens, data = mC1, family =binomial),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2d, mC1, conditional = T)

#Significant missing paths
sem.coefs(ro2d, mC1)

#### ro2e *** ####
#Modele de piste
ro2e <- list(
  glm(prop_fox_dens ~ lmg_C1_CORR + cumul_prec + MEAN_temp + winAO + sumAO + sprAO, weights = monit_dens, data = mC1, family =binomial),
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

#### Causal link between lemming and goose ####

#### Equivalent ro2-a ####
#Modele de piste
ro2a_L <- list(
  glm(prop_fox_dens ~ lmg_C1_CORR + cumul_prec + MEAN_temp, weights = monit_dens, data = mC1, family =binomial),
  glmer(SN ~ cumul_prec + MEAN_temp + lmg_C1_CORR + (1|AN), data = mC1, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2a_L, mC1, conditional = T)

#NO significant missing paths
sem.coefs(ro2a_L, mC1)


#### Equivalent de ro2 ####
#Modele de piste
ro2_L <- list(
  glm(prop_fox_dens ~ lmg_C1_CORR + winAO + cumul_prec + MEAN_temp, weights = monit_dens, data = mC1, family =binomial),
  glmer(SN ~ lmg_C1_CORR + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")),
  lm(lmg_C1_CORR ~ winAO + MEAN_temp + cumul_prec, data = mC1))
# Get goodness-of-fit and AIC
sem.fit(ro2_L, mC1, conditional = T)

#NO significant missing paths
sem.coefs(ro2_L, mC1)

#### EQUIVALENT PATH DIAGRAM ####
library(CauseAndCorrelation)
library(ggm)

# Code for the equivalent DAG we want to test, including latent variables
# ------- both egg.conso and lmg.conso influenced by temp & prec ------ #
test <- DAG (
  lmg.conso ~ lmg + temp + prec,
  egg.conso ~ lmg.conso + prec + temp,
  fox ~ lmg.conso + egg.conso + winAO,
  goose ~ egg.conso
)

Observed.Equivalent.DAG(test, latents = c("lmg.conso", "egg.conso"))

# ------- egg.conso influenced by temp & prec ------ #
test2 <- DAG (
  lmg.conso ~ lmg,
  egg.conso ~ lmg.conso + prec + temp,
  fox ~ lmg.conso + egg.conso + winAO,
  goose ~ egg.conso
)

Observed.Equivalent.DAG(test2, latents = c("lmg.conso", "egg.conso"))


#### Resolving fox glm issues #### 
setwd(dir = "C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
mC1 <- read.table("mC1_path_data.txt", h = T)

jo <- glm(cbind(breed_dens, monit_dens-breed_dens) ~ lmg_C1_CORR + cumul_prec + MEAN_temp + winAO, family = binomial, data = mC1)
summary(jo)

x11()
par(mfrow = c(2,2))
plot(jo)

# Check overdispersion
E1 <- resid(jo, type = "pearson")
sum(E1^2) / (jo$df.residual) # Huge overdispersion !!!

# Try glm without 2000 year = outlier lmg abundance
mC1bis <- mC1[!(mC1$AN == 2000),]

jo <- glm(cbind(breed_dens, monit_dens-breed_dens) ~ lmg_C1_CORR + cumul_prec + MEAN_temp + winAO, family = binomial, data = mC1bis)
summary(jo)

# Check overdispersion
E1 <- resid(jo, type = "pearson")
sum(E1^2) / (jo$df.residual) # Still huge overdispersion !!!

# Try glm only with fox variable

jo <- glm(cbind(breed_dens, monit_dens-breed_dens) ~ lmg_C1_CORR, family = binomial, data = mC1)
summary(jo)

# Check overdispersion
E1 <- resid(jo, type = "pearson")
sum(E1^2) / (jo$df.residual) # Still huge overdispersion !!!

# Try glm with quasibinomial family
jqb <- glm(cbind(breed_dens, monit_dens-breed_dens) ~ lmg_C1_CORR + cumul_prec + MEAN_temp + winAO, family = quasibinomial, data = mC1)
summary(jqb)

x11()
par(mfrow = c(2,2))
plot(jqb)

# Check overdispersion
Eqb <- resid(jqb, type = "pearson")
sum(Eqb^2) / (jqb$df.residual) # Huge overdispersion !!!

# Try glm with Poisson family
jp <- glm(prop_fox_dens ~ lmg_C1_CORR + cumul_prec + MEAN_temp + winAO, family = poisson(link = "log"), data = mC1)
summary(jp)

x11()
par(mfrow = c(2,2))
plot(jp)

# Check overdispersion
Ep <- resid(jp, type = "pearson")
sum(Ep^2) / (jp$df.residual) # Huge overdispersion !!!

# Try by transforming data
# Log of lemming abundance *** without 2000 ***
mC1bis <- mC1[!(mC1$AN == 2000),]
g5 <- glm(cbind(breed_dens, monit_dens - breed_dens) ~ log(lmg_C1_CORR) + winAO + cumul_prec + MEAN_temp, data = mC1bis, family = binomial)

summary(g5)

Eg5 <- resid(g5, type = "pearson")
sum(Eg5^2) / (g5$df.residual) # *** Overdisp. = 1.13 for the complete modele *** 

#### Same but with no repetition of values 
lmg <- read.table("LEM_1993-2017.txt", sep = "\t", dec = ",", h = T)
lmg <- lmg[lmg$YEAR >= 1996 & !lmg$YEAR == 2017,]; head(lmg); summary(lmg)

fox <- read.table("FOX_abundance_Chevallier.txt", sep = "\t", dec = ",", h = T)
fox <- fox[fox$year >= 1996 & !fox$year == 2017,]; head(fox); summary(fox)

fox$LMG_C1_CORR <- lmg$LMG_C1_CORR[match(fox$year, lmg$YEAR)]
head(fox)

jj <- glm(cbind(natal_growth_dens, monit_dens-natal_growth_dens) ~ LMG_C1_CORR, family = binomial, data = fox)
summary(jj)

# Check overdispersion
Ejj <- resid(jj, type = "pearson")
sum(Ejj^2) / (jj$df.residual)
