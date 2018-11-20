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

# Checking for overdispersion
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
