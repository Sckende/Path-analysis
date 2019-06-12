### PATH MODELES in order to correct the overdispersion ####


setwd(dir = "C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")

list.files()
rm(list = ls()) #clean R memory

# Complete data
mC1 <- read.table("mC1_path_data.txt", h = T)
mC1$AN <- as.factor(as.character(mC1$AN))
summary(mC1)
head(mC1)

# Data without 2000 year
mC1_2000 <- mC1[!(mC1$AN == "2000"),]
summary(mC1_2000)

#Package nécessaires
require(nlme)
require(lme4)
require(piecewiseSEM)
require(ggm)
require(visreg)
require(DHARMa)
require(itsadug)

#### Warnings message and overdispersion in glm model (proportion of breeding fox dens)
# Test 1
jo_1 <- glm(prop_fox_dens ~ lmg_C1_CORR + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = mC1, family = quasibinomial) # Using quasi binomial copes the warning message "In eval(family$initialize) : non-integer #successes in a binomial glm!"
summary(jo_1)

# Test 2.1
jo_tr <- glm(prop_fox_dens ~ I(log(lmg_C1_CORR)) + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = mC1, family = binomial(link = "logit")) # I() allows automatic back-transformation by the package
summary(jo_tr)

# Test 2.2
jo_tr_quasi <- glm(prop_fox_dens ~ I(log(lmg_C1_CORR)) + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = mC1, family = quasibinomial)
summary(jo_tr_quasi)

# Test 3 
jo_2000 <- glm(prop_fox_dens ~ lmg_C1_CORR + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = mC1_2000, family = binomial(link = "logit"))
summary(jo_2000)

# Test 4
jo_2000_tr <- glm(prop_fox_dens ~ I(log(lmg_C1_CORR)) + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = mC1_2000, family = binomial(link = "logit"))
summary(jo_2000_tr)

#### Checking for overdispersion ####
  # glm model choice
jo <- jo_2000_tr
  
  # Plot model
x11()
par(mfrow = c(2, 2))
plot(jo)

  # Estimation of overdispersion
E1 <- resid(jo, type = "pearson") # Pearson residuals
F1 <- fitted(jo) # Fitted values

N <- nrow(mC1) # Number of observations
p <- length(coef(jo)) # Number of parameters

Dispersion <- sum(E1^2) / (N-p)
Dispersion

# Disp. jo_1 = 3.71
# Disp. jo_tr = 1.57
# Disp. jo_2000 = 2.54
# Disp. jo_2000_tr = 1.05

#### Checking for the good fit of predictions - Fran script ####

require(visreg)

# Model choice
m <- glm(prop_fox_dens ~ I(lmg_C1_CORR) + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = mC1, family = binomial(link="logit"))

m <- glm(prop_fox_dens ~ log(lmg_C1_CORR) + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = mC1, family = binomial(link="logit"))
summary(m)

x11()
par(mfrow=c(2,2))

# Plot Y vs. each explicative variables
visreg(m,scale="response")

# Predictions plot

plot(mC1$lmg_C1_CORR,mC1$prop_fox_dens,xlim=c(-20,50),ylim=0:1)

v<-seq(min(mC1$lmg_C1_CORR)-20,max(mC1$lmg_C1_CORR)+40,by=0.01)

newdat<-data.frame(lmg_C1_CORR=v,cumul_prec=mean(mC1$cumul_prec),MEAN_temp=mean(mC1$MEAN_temp),winAO=mean(mC1$winAO))

p<-predict(m,newdata=newdat,type="response")

lines(v,p)

#### Plots paper style ####
x11()
par(mfrow = c(1, 2))
# With 200 data
m <- glm(prop_fox_dens ~ I(log(lmg_C1_CORR)) + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = mC1, family = binomial(link="logit"))
plot(mC1$lmg_C1_CORR, mC1$prop_fox_dens, xlim = c(0, 10), ylim = c(0, 0.5), bty = "n", las = 1, main = "With 2000")

v <- seq(0, 10, by = 0.01)

newdat <- data.frame(lmg_C1_CORR = v, cumul_prec = mean(mC1$cumul_prec), MEAN_temp = mean(mC1$MEAN_temp), winAO = mean(mC1$winAO))

p <- predict(m, newdata = newdat, type = "response")
lines(v, p)
abline(h = max(p), lty = "dotted", col = "orange")

# Without 2000 data

m <- glm(prop_fox_dens ~ I(log(lmg_C1_CORR)) + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = mC1_2000, family = binomial(link="logit"))
plot(mC1_2000$lmg_C1_CORR, mC1_2000$prop_fox_dens, xlim = c(0, 10), ylim = c(0, 0.5), bty = "n", las = 1, main = "Without 2000")

v <- seq(0, 10, by = 0.01)

newdat <- data.frame(lmg_C1_CORR = v, cumul_prec = mean(mC1_2000$cumul_prec), MEAN_temp = mean(mC1_2000$MEAN_temp), winAO = mean(mC1_2000$winAO))

p <- predict(m, newdata = newdat, type = "response")
lines(v, p)
abline(h = max(p), lty = "dotted", col = "orange")

#
dev.off()

#### Path analysis - Model Ro2b ####

# ORIGINAL MODEL
ro2b <- list(
  glm(prop_fox_dens ~ lmg_C1_CORR + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = mC1, family = binomial(link = "logit")),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")))

# Get goodness-of-fit and AIC
sem.fit(ro2b, mC1, conditional = T)
#NO significant missing paths
sem.coefs(ro2b, mC1)

# ORIGINAL MODELE - SCALED DATA
ro2bSC <- list(
  glm(prop_fox_dens ~ scale(lmg_C1_CORR) + scale(cumul_prec) + scale(MEAN_temp) + scale(winAO), weights = monit_dens, data = mC1, family = binomial(link = "logit")),
  glmer(SN ~ scale(prop_fox_dens) + scale(cumul_prec) + scale(MEAN_temp) + (1|AN), data = mC1, family = binomial(link = "logit")))

# Get goodness-of-fit and AIC
sem.fit(ro2bSC, mC1, conditional = T)
#NO significant missing paths
sem.coefs(ro2bSC, mC1)
sem.model.fits(ro2bSC) #calcul des R2

# LOG(LMG) MODEL
ro2b <- list(
  glm(prop_fox_dens ~ I(log(lmg_C1_CORR)) + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = mC1, family = binomial(link = "logit")),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")))

# Get goodness-of-fit and AIC
sem.fit(ro2b, mC1, conditional = T)
#NO significant missing paths
sem.coefs(ro2b, mC1)



# LOG(LMG) - SCALED DATA
ro2bSC <- list(
  glm(prop_fox_dens ~ scale(I(log(lmg_C1_CORR))) + scale(cumul_prec) + scale(MEAN_temp) + scale(winAO), weights = monit_dens, data = mC1, family = binomial(link = "logit")),
  glmer(SN ~ scale(prop_fox_dens) + scale(cumul_prec) + scale(MEAN_temp) + (1|AN), data = mC1, family = binomial(link = "logit")))

# Get goodness-of-fit and AIC
sem.fit(ro2bSC, mC1, conditional = T)
#NO significant missing paths
sem.coefs(ro2bSC, mC1)
sem.model.fits(ro2bSC) #calcul des R2

# LOG(LMG) - 2000 MODEL
ro2b <- list(
  glm(prop_fox_dens ~ I(log(lmg_C1_CORR)) + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = mC1_2000, family = binomial(link = "logit")),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1_2000, family = binomial(link = "logit")))

# Get goodness-of-fit and AIC
sem.fit(ro2b, mC1_2000, conditional = T)
#NO significant missing paths
sem.coefs(ro2b, mC1_2000)

# Loss of the causal link between fox and cumulative precipitation
# Delete cumul_prec variable in the model

ro2b <- list(
  glm(prop_fox_dens ~ I(log(lmg_C1_CORR)) + MEAN_temp + winAO, weights = monit_dens, data = mC1_2000, family = binomial(link = "logit")),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1_2000, family = binomial(link = "logit")))

# Get goodness-of-fit and AIC
sem.fit(ro2b, mC1_2000, conditional = T)
#NO significant missing paths
sem.coefs(ro2b, mC1_2000)

# LOG(LMG) - 2000 & without causal link between fox and cumul_prec - SCALED DATA
ro2bSC <- list(
  glm(prop_fox_dens ~ scale(I(log(lmg_C1_CORR))) + scale(MEAN_temp) + scale(winAO), weights = monit_dens, data = mC1_2000, family = binomial(link = "logit")),
  glmer(SN ~ scale(prop_fox_dens) + scale(cumul_prec) + scale(MEAN_temp) + (1|AN), data = mC1_2000, family = binomial(link = "logit")))

# Get goodness-of-fit and AIC
sem.fit(ro2bSC, mC1_2000, conditional = T)
#NO significant missing paths
sem.coefs(ro2bSC, mC1_2000)
sem.model.fits(ro2bSC) #calcul des R2

#### PATH ANALYSIS - with LOG(LMG) - 2000 year ####

#### ro2a ####
#Modele de piste
ro2a <- list(
  glm(prop_fox_dens ~ I(log(lmg_C1_CORR)) + cumul_prec + MEAN_temp, weights = monit_dens, data = mC1_2000, family =binomial(link = "logit")),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1_2000, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2a, mC1_2000, conditional = T)
#NO significant missing paths
sem.coefs(ro2a, mC1_2000)

#### ro2b ####
#Modele de piste
ro2b <- list(
  glm(prop_fox_dens ~ I(log(lmg_C1_CORR)) + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = mC1_2000, family = binomial(link = "logit")),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1_2000, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2b, mC1_2000, conditional = T)
#NO significant missing paths
sem.coefs(ro2b, mC1_2000)

#### ro2c ####
#Modele de piste
ro2c <- list(
  glm(prop_fox_dens ~ I(log(lmg_C1_CORR)) + cumul_prec + MEAN_temp + winAO + sprAO, weights = monit_dens, data = mC1_2000, family =binomial(link = "logit")),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1_2000, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2c, mC1_2000, conditional = T)
#NO significant missing paths
sem.coefs(ro2c, mC1_2000)

#### ro2d ####
#Modele de piste
ro2d <- list(
  glm(prop_fox_dens ~ I(log(lmg_C1_CORR)) + cumul_prec + MEAN_temp + winAO + sumAO + sprAO, weights = monit_dens, data = mC1_2000, family =binomial(link = "logit")),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1_2000, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2d, mC1_2000, conditional = T)
#Significant missing paths
sem.coefs(ro2d, mC1_2000)

#### ro2e ####
#Modele de piste
ro2e <- list(
  glm(prop_fox_dens ~ I(log(lmg_C1_CORR)) + cumul_prec + MEAN_temp + winAO + sumAO + sprAO, weights = monit_dens, data = mC1_2000, family =binomial(link = "logit")),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + sumAO + (1|AN), data = mC1_2000, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2e, mC1_2000, conditional = T)
#NO significant missing paths
sem.coefs(ro2e, mC1_2000)

#### ro2f ####
#Modele de piste
ro2f <- list(
  glm(prop_fox_dens ~ I(log(lmg_C1_CORR)) + MEAN_temp + winAO, weights = monit_dens, data = mC1_2000, family = binomial(link = "logit")),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1_2000, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2f, mC1_2000, conditional = T)
#NO significant missing paths
sem.coefs(ro2f, mC1_2000)


#### PATH ANALYSIS - with LOG(LMG) ####

#### ro2 ####
#Modele de piste
ro2 <- list(
  glm(prop_fox_dens ~ I(log(lmg_C1_CORR)) + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = mC1, family =binomial(link = "logit")),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")),
  lm(log(lmg_C1_CORR) ~  winAO + MEAN_temp + cumul_prec, data = mC1)
)
# Get goodness-of-fit and AIC
sem.fit(ro2, mC1, conditional = T)
#NO significant missing paths
sem.coefs(ro2, mC1)

#### ro2.1 ####
#Modele de piste
ro2.1 <- list(
  glm(prop_fox_dens ~ I(log(lmg_C1_CORR)) + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = mC1, family =binomial(link = "logit")),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")),
  lm(log(lmg_C1_CORR) ~  MEAN_temp + cumul_prec, data = mC1)
)
# Get goodness-of-fit and AIC
sem.fit(ro2.1, mC1, conditional = T)
#NO significant missing paths
sem.coefs(ro2.1, mC1)

#### ro2a ####
#Modele de piste
ro2a <- list(
  glm(prop_fox_dens ~ I(log(lmg_C1_CORR)) + cumul_prec + MEAN_temp, weights = monit_dens, data = mC1, family =binomial(link = "logit")),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2a, mC1, conditional = T)
#NO significant missing paths
sem.coefs(ro2a, mC1)

#### ro2a-SC ####
#Modele de piste
ro2aSC <- list(
  glm(prop_fox_dens ~ scale(I(log(lmg_C1_CORR))) + scale(cumul_prec) + scale(MEAN_temp), weights = monit_dens, data = mC1, family =binomial(link = "logit")),
  glmer(SN ~ scale(prop_fox_dens) + scale(cumul_prec) + scale(MEAN_temp) + (1|AN), data = mC1, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2aSC, mC1, conditional = T)
#NO significant missing paths
sem.coefs(ro2aSC, mC1)

#### ro2b ####
#Modele de piste
ro2b <- list(
  glm(prop_fox_dens ~ I(log(lmg_C1_CORR)) + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = mC1, family = binomial(link = "logit")),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2b, mC1, conditional = T)
#NO significant missing paths
sem.coefs(ro2b, mC1)

#Modele de piste SCALED
ro2bSC <- list(
  glm(prop_fox_dens ~ scale(I(log(lmg_C1_CORR))) + scale(cumul_prec) + scale(MEAN_temp) + scale(winAO), weights = monit_dens, data = mC1, family = binomial(link = "logit")),
  glmer(SN ~ scale(prop_fox_dens) + scale(cumul_prec) + scale(MEAN_temp) + (1|AN), data = mC1, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2bSC, mC1, conditional = T)
#NO significant missing paths
sem.coefs(ro2bSC, mC1)
#### ro2c ####
#Modele de piste
ro2c <- list(
  glm(prop_fox_dens ~ I(log(lmg_C1_CORR)) + cumul_prec + MEAN_temp + winAO + sprAO, weights = monit_dens, data = mC1, family =binomial(link = "logit")),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2c, mC1, conditional = T)
#NO significant missing paths
sem.coefs(ro2c, mC1)

#### ro2d ####
#Modele de piste
ro2d <- list(
  glm(prop_fox_dens ~ I(log(lmg_C1_CORR)) + cumul_prec + MEAN_temp + winAO + sumAO + sprAO, weights = monit_dens, data = mC1, family =binomial(link = "logit")),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2d, mC1, conditional = T)
#Significant missing paths
sem.coefs(ro2d, mC1)

#### ro2e ####
#Modele de piste
ro2e <- list(
  glm(prop_fox_dens ~ I(log(lmg_C1_CORR)) + cumul_prec + MEAN_temp + winAO + sumAO + sprAO, weights = monit_dens, data = mC1, family =binomial(link = "logit")),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + sumAO + (1|AN), data = mC1, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2e, mC1, conditional = T)
#NO significant missing paths
sem.coefs(ro2e, mC1)

#### ro2f ####
#Modele de piste
ro2f <- list(
  glm(prop_fox_dens ~ I(log(lmg_C1_CORR)) + MEAN_temp + winAO, weights = monit_dens, data = mC1, family = binomial(link = "logit")),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2f, mC1, conditional = T)
#NO significant missing paths
sem.coefs(ro2f, mC1)

#### Trash part ####
plot(mC1$cumul_prec, mC1$sumAO)
summary(lm(mC1$cumul_prec~mC1$sumAO))
cor.test(mC1$cumul_prec, mC1$sumAO)

plot(mC1$MEAN_temp, mC1$sumAO)
summary(lm(mC1$MEAN_temp~mC1$sumAO))
cor.test(mC1$MEAN_temp, mC1$sumAO)

m <- split(mC1, paste(mC1$AN))
m <- lapply(m, function(x){
  
})

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

