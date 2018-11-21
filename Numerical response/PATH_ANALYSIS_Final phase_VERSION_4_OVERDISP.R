### PATH MODELES in order to correct the overdispersion ####


setwd(dir = "C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")

list.files()
rm(list = ls()) #clean R memory

mC1 <- read.table("mC1_path_data.txt", h = T)
summary(mC1)
head(mC1)

#Package nécessaires
require(nlme)
require(lme4)
require(piecewiseSEM)
require(ggm)

#### Warnings message and overdispersion in glm model (proportion of breeding fox dens)
jo <- glm(prop_fox_dens ~ lmg_C1_CORR + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = mC1, family = quasibinomial) # Using quasi binomial copes the warning message "In eval(family$initialize) : non-integer #successes in a binomial glm!"
summary(jo)

#### Checking for overdispersion ####
x11()
par(mfrow = c(2, 2))
plot(jo)

jo # glm modele
E1 <- resid(jo, type = "pearson") # Pearson residuals
F1 <- fitted(jo) # Fitted values

N <- nrow(mC1) # Number of observations
p <- length(coef(jo)) # Number of parameters

Dispersion <- sum(E1^2) / (N-p)
Dispersion

#### Checking for residuals ####
Y <- mC1$prop_fox_dens
summary(jo)

x11()
par(mfrow = c(2,2))
plot(jo)

dev.off()

par(mfrow = c(1, 1))
plot(predict(jo),residuals(jo),col=c("blue","red")[1+Y])
abline(h=0,lty=2,col="grey")

lines(lowess(predict(jo),residuals(jo)),col="black",lwd=2)

require(splines)
rl <- lm(residuals(jo) ~ bs(predict(jo),8)) 

y <- predict(rl, se=TRUE)
segments(predict(jo), y$fit + 2*y$se.fit, predict(jo), y$fit - 2*y$se.fit, col="green") 


# Relationship with the first explicative variable
X1 <- mC1$lmg_C1_CORR

plot(X1, residuals(jo), col=c("blue","red")[1+Y])
lines(lowess(X1, residuals(jo)), col="black", lwd=2)
lines(lowess(X1[Y==0], residuals(jo)[Y==0]), col="blue")
lines(lowess(X1[Y==1], residuals(jo)[Y==1]), col="red")
abline(h=0, lty=2, col="grey")

#### Predicted vakues of GLM ####
# plot predicted values on raw data
range(mC1$lmg_C1_CORR)
# For creation new dataframe for lmg values simulation
v <- seq(0, 10, by = 0.1)
p <- predict(jo, newdata = data.frame(lmg_C1_CORR = v), type = "response", se.fit = TRUE)

plot(mC1$lmg_C1_CORR, mC1$prop_fox_dens)
lines(p$fit, type = "l", col = "green")


#### Path analysis ####
# BEST MODELE
ro2b <- list(
  glm(prop_fox_dens ~ lmg_C1_CORR + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = mC1, family = quasibinomial),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2b, mC1, conditional = T)
#NO significant missing paths
sem.coefs(ro2b, mC1)



# BEST MODELE - SCALED DATA
ro2bSC <- list(
  glm(prop_fox_dens ~ scale(lmg_C1_CORR) + scale(cumul_prec) + scale(MEAN_temp) + scale(winAO), weights = monit_dens, data = mC1, family = quasibinomial),
  glmer(SN ~ scale(prop_fox_dens) + scale(cumul_prec) + scale(MEAN_temp) + (1|AN), data = mC1, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2bSC, mC1, conditional = T)

#NO significant missing paths
sem.coefs(ro2bSC, mC1)
sem.model.fits(ro2bSC) #calcul des R2
