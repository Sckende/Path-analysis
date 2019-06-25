### PATH MODELES with last version of package ####


setwd(dir = "C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")

list.files()
rm(list = ls()) #clean R memory

# Complete data
mC1 <- read.table("mC1_path_data.txt", h = T)[, c(1, 4, 12, 15, 17, 18, 24, 25, 28)]
mC1$AN <- as.factor(as.character(mC1$AN))
summary(mC1)
head(mC1)
names(mC1)

# Creation of new column
mC1$log.lmg <- log(mC1$lmg_C1_CORR)
mC1$prec.temp <- mC1$cumul_prec*mC1$MEAN_temp

# Scaled data
mC1.sc <- apply(mC1[, -c(1, 4, 7, 8)], MARGIN = 2, scale)
AN <- as.character(mC1$AN)
mC1.sc <- cbind(mC1.sc, AN, SN = mC1$SN, prop_fox_dens = mC1$prop_fox_dens, monit_dens = mC1$monit_dens)
mC1.sc <- apply(mC1.sc, MARGIN = 2, as.numeric)
mC1.sc <- as.data.frame(mC1.sc)
mC1.sc$AN <- as.factor(mC1.sc$AN)
summary(mC1.sc)

#Package nécessaires
require(nlme)
require(lme4)
require(piecewiseSEM)
require(ggm)
require(visreg)
require(DHARMa)
require(itsadug)
require(visreg)


#### Path analysis ####

#### M1 ####

M1.list <- list(
  glm(prop_fox_dens ~ log.lmg + cumul_prec + MEAN_temp, weights = monit_dens, data = mC1, family = binomial(link = "logit")),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")))

M1 <- psem(
  glm(prop_fox_dens ~ log.lmg + cumul_prec + MEAN_temp, weights = monit_dens, data = mC1, family = binomial(link = "logit")),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")))

# Get goodness-of-fit and AIC
sem.fit(M1.list, mC1, conditional = TRUE)
sem.fit(M1.list, mC1.sc, conditional = TRUE)
sem.coefs(M1.list, mC1)

sem.coefs(M1.list, mC1, standardize = "scale")

M1 <- as.psem(M1.list)
sum.M1 <- summary(M1)
sum.M1$Cstat # Fisher C
sum.M1$IC$AIC #AIC
sum.M1$dTable # D-separation tests
sum.M1$coefficients # Path coefficients
sem.coefs(sum.M1, mC1, standardize = "scale")
coefs(M1, standardize = "scale", standardize.type = "latent.linear")

# j <- glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit"))
# coefs(j, standardize.type = "latent.linear")

sem.coefs(M1.list, mC1, standardize = "scale") # Old version of package
#### M2 ####

M2.list <- list(
  glm(prop_fox_dens ~ log.lmg + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = mC1, family = binomial(link = "logit")),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")))

# Get goodness-of-fit and AIC
sem.fit(M2.list, mC1, conditional = TRUE)
sem.coefs(M2.list, mC1, standardize = "none")
sem.coefs(M2.list, mC1, standardize = "scale")

M2 <- as.psem(M2.list)
sum.M2 <- summary(M2)
sum.M2$Cstat # Fisher C
sum.M2$IC$AIC #AIC
sum.M2$dTable # D-separation tests
sum.M2$coefficients # Path coefficients


#### M3 ####

M3.list <- list(
  glm(prop_fox_dens ~ log.lmg + cumul_prec + MEAN_temp + winAO + sprAO, weights = monit_dens, data = mC1, family =binomial(link = "logit")),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")))

# Get goodness-of-fit and AIC
M3 <- as.psem(M3.list)
sum.M3 <- summary(M3)
sum.M3$Cstat # Fisher C
sum.M3$IC$AIC #AIC
sum.M3$dTable # D-separation tests
sum.M3$coefficients # Path coefficient

#### M4 ####

M4.list <- list(
  glm(prop_fox_dens ~ log.lmg + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = mC1, family =binomial(link = "logit")),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")),
  lm(log.lmg ~ winAO + cumul_prec + MEAN_temp, data = mC1))

# Get goodness-of-fit and AIC
sem.fit(M4.list, mC1, conditional = TRUE)
sem.coefs(M4.list, mC1, standardize = "scale")

M4 <- as.psem(M4.list)
sum.M4 <- summary(M4)
sum.M4$IC$AIC #AIC
sum.M4$dTable # D-separation tests
sum.M4$Cstat # Fisher C
sum.M4$coefficients # Path coefficient

#### M5 ####

M5.list <- list(
  glm(prop_fox_dens ~ log.lmg + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = mC1, family =binomial(link = "logit")),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")),
  lm(log.lmg ~ cumul_prec + MEAN_temp, data = mC1))

# Get goodness-of-fit and AIC
sem.fit(M5.list, mC1, conditional = TRUE)
sem.coefs(M5.list, mC1, standardize = "scale")

M5 <- as.psem(M5.list)
sum.M5 <- summary(M5)
sum.M5$IC$AIC #AIC
sum.M5$dTable # D-separation tests
sum.M5$Cstat # Fisher C
sum.M5$coefficients # Path coefficient

#### M6 ####

M6.list <- list(
  glm(prop_fox_dens ~ log.lmg + cumul_prec + MEAN_temp + prec.temp, weights = monit_dens, data = mC1.sc, family = binomial(link = "logit")),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + prec.temp + (1|AN), data = mC1.sc, family = binomial(link = "logit")))

# Get goodness-of-fit and AIC
sem.fit(M6.list, mC1.sc, conditional = TRUE)
sem.coefs(M6.list, mC1.sc)

M6 <- as.psem(M6.list)
sum.M6 <- summary(M6); sum.M6
sum.M6$Cstat # Fisher C
sum.M6$IC$AIC #AIC
sum.M6$dTable # D-separation tests
sum.M6$coefficients # Path coefficients

#### M7 ####

M7.list <- list(
  glm(prop_fox_dens ~ log.lmg + winAO + cumul_prec + MEAN_temp + prec.temp, weights = monit_dens, data = mC1.sc, family = binomial(link = "logit")),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + prec.temp + (1|AN), data = mC1.sc, family = binomial(link = "logit")))

# Get goodness-of-fit and AIC
sem.fit(M7.list, mC1.sc, conditional = TRUE)
sem.coefs(M7.list, mC1.sc)

#### ------------------ ####
#### REVIEWER COMMENTS ####
#### ---------------- ####

#### TEMPORAL AUTO-CORRELATION ####
# CF script "PATH_PAPER_Figures_production.R"


#### NON-STATIONARY VARIANCE ####
j <-  glmer(SN ~ scale(prop_fox_dens) + scale(cumul_prec) + scale(MEAN_temp)  + (1|AN), data = mC1, family = binomial(link = "logit")); summary(j)
acf(residuals(j))

h <-   glm(prop_fox_dens ~ I(log(lmg_C1_CORR)) + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = mC1, family = binomial(link = "logit")); summary(h)
acf(residuals(h)) #here problem with repetition of variables; need to have one value per year



#### Interaction between temperature and precipitation ####
j <-  glmer(SN ~ scale(prop_fox_dens) + scale(cumul_prec) + scale(MEAN_temp)  + (1|AN), data = mC1, family = binomial(link = "logit")); summary(j) # AIC = 2389
visreg(j)
plot(simulateResiduals(j))
     
jj <-  glmer(SN ~ scale(prop_fox_dens) + scale(cumul_prec)*scale(MEAN_temp)  + (1|AN), data = mC1, family = binomial(link = "logit")); summary(jj) # AIC = 2385
visreg(jj)
plot(simulateResiduals(jj))

# LOG(LMG) - SCALED DATA - PREC & TEMP INTERACTIONS
ro2bSCinter <- list(
  glm(prop_fox_dens ~ scale(I(log(lmg_C1_CORR))) + scale(cumul_prec) + scale(MEAN_temp) + scale(winAO), weights = monit_dens, data = mC1, family = binomial(link = "logit")),
  glmer(SN ~ scale(prop_fox_dens) + scale(cumul_prec)*scale(MEAN_temp)  + (1|AN), data = mC1, family = binomial(link = "logit"))) # *** do not work with interaction **** 

# Get goodness-of-fit and AIC
sem.fit(ro2bSCinter, mC1, conditional = T)
#NO significant missing paths
sem.coefs(ro2bSCinter, mC1)
sem.model.fits(ro2bSCinter) #calcul des R2

#### TRASH SECTION ####

M1.list <- list(
  glm(prop_fox_dens ~ log.lmg + cumul_prec + MEAN_temp, weights = monit_dens, data = mC1.sc, family = binomial(link = "logit")),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1.sc, family = binomial(link = "logit")))

# Get goodness-of-fit and AIC
sem.fit(M1.list, mC1.sc, conditional = TRUE)
sem.coefs(M1.list, mC1.sc)
