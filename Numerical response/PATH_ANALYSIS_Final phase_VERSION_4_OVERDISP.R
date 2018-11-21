### PATH MODELES in order to correct the overdispersion ####


setwd(dir = "C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")

list.files()
rm(list = ls()) #clean R memory

mC1 <- read.table("mC1_path_data.txt", h = T)
summary(mC1)
head(mC1)

mC1_2000 <- mC1[!(mC1$AN == "2000"),]
summary(mC1_2000)

#Package nécessaires
require(nlme)
require(lme4)
require(piecewiseSEM)
require(ggm)

#### Warnings message and overdispersion in glm model (proportion of breeding fox dens)
# Test 1
jo_1 <- glm(prop_fox_dens ~ lmg_C1_CORR + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = mC1, family = quasibinomial) # Using quasi binomial copes the warning message "In eval(family$initialize) : non-integer #successes in a binomial glm!"
summary(jo_1)

# Test 2.1
jo_tr <- glm(prop_fox_dens ~ I(log(lmg_C1_CORR)) + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = mC1, family = binomial(link = "logit"))
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
x11()
par(mfrow = c(2, 2))
plot(jo)

jo <- jo_2000_tr # glm modele choice
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
  glm(prop_fox_dens ~ I(log(lmg_C1_CORR)) + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = mC1_2000, family = binomial(link = "logit")),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = mC1_2000, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2b, mC1_2000, conditional = T)
#NO significant missing paths
sem.coefs(ro2b, mC1_2000)



# BEST MODELE - SCALED DATA
ro2bSC <- list(
  glm(prop_fox_dens ~ scale(lmg_C1_CORR) + scale(cumul_prec) + scale(MEAN_temp) + scale(winAO), weights = monit_dens, data = mC1, family = quasibinomial),
  glmer(SN ~ scale(prop_fox_dens) + scale(cumul_prec) + scale(MEAN_temp) + (1|AN), data = mC1, family = binomial(link = "logit")))
# Get goodness-of-fit and AIC
sem.fit(ro2bSC, mC1, conditional = T)

#NO significant missing paths
sem.coefs(ro2bSC, mC1)
sem.model.fits(ro2bSC) #calcul des R2

#### Fran version ####

library(visreg)



m <- glm(prop_fox_dens ~ I(log(lmg_C1_CORR)) + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = mC1, family = binomial(link="logit"))
summary(m)

x11()
par(mfrow=c(2,2))

visreg(m)
visreg(m,scale="response")
visreg(m,"lmg_C1_CORR",scale="response",xtrans=function(x){x+0})


mC1$loglmg_C1_CORR<-log(mC1$lmg_C1_CORR)

m <- glm(prop_fox_dens ~ loglmg_C1_CORR + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = mC1, family = binomial(link="logit"))

par(mfrow=c(2,2))
visreg(m)
visreg(m,scale="response")
visreg(m,"loglmg_C1_CORR",scale="response",xtrans=function(x){exp(x)})


m <- glm(prop_fox_dens ~ I(lmg_C1_CORR) + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = mC1, family = binomial(link="logit"))



plot(mC1$lmg_C1_CORR,mC1$prop_fox_dens,xlim=c(-20,50),ylim=0:1)

v<-seq(min(mC1$lmg_C1_CORR)-20,max(mC1$lmg_C1_CORR)+40,by=0.01)

newdat<-data.frame(lmg_C1_CORR=v,cumul_prec=mean(mC1$cumul_prec),MEAN_temp=mean(mC1$MEAN_temp),winAO=mean(mC1$winAO))

p<-predict(m,newdata=newdat,type="response")

lines(v,p)

#### Plot for paper ####
x11()
par(mfrow = c(1, 2))
# With 200 data
m <- glm(prop_fox_dens ~ I(log(lmg_C1_CORR)) + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = mC1, family = binomial(link="logit"))
plot(mC1$lmg_C1_CORR,mC1$prop_fox_dens,xlim=c(0,10),ylim = c(0, 0.5), bty = "n", las = 1, main = "With 2000")

v<-seq(0,10,by=0.01)

newdat<-data.frame(lmg_C1_CORR=v,cumul_prec=mean(mC1$cumul_prec),MEAN_temp=mean(mC1$MEAN_temp),winAO=mean(mC1$winAO))

p<-predict(m,newdata=newdat,type="response")
lines(v,p)
abline(h = max(p), lty = "dotted", col = "orange")

# Without 2000 data

m <- glm(prop_fox_dens ~ I(log(lmg_C1_CORR)) + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = mC1_2000, family = binomial(link="logit"))
plot(mC1_2000$lmg_C1_CORR,mC1_2000$prop_fox_dens,xlim=c(0,10),ylim = c(0, 0.5), bty = "n", las = 1, main = "Without 2000")

v<-seq(0,10,by=0.01)

newdat<-data.frame(lmg_C1_CORR=v,cumul_prec=mean(mC1_2000$cumul_prec),MEAN_temp=mean(mC1_2000$MEAN_temp),winAO=mean(mC1_2000$winAO))

p<-predict(m,newdata=newdat,type="response")
lines(v,p)
abline(h = max(p), lty = "dotted", col = "orange")

#
dev.off()