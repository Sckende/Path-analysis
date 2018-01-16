setwd(dir = "/Users/nicolas/Documents/Claire/Doc doc doc !/R analysis/Data")
fox<-read.table("Fox_abundance_Chevallier.txt",sep = "\t", dec = ",", h=T)
summary(fox)

par(mfrow=c(2,2))
plot(fox$year,fox$monit_dens, cex=.3, xlab="year", ylab="number of monitored dens", type="b")
plot(fox$year,fox$natal_growth_dens, cex=.3, xlab="year", ylab="number of natal and growth dens", type="b")
plot(fox$year,fox$prop_natal_dens, cex=.3, xlab="year", ylab="proportion of natal dens - %", type="b")
plot(fox$year,fox$litter_number, cex=.3, xlab="year", ylab="number of litter", type="b")
#####Analyse de piste - données package ggm#####
require(ggm)

amat <- DAG(y ~ x+z, z~u+v)

AG(amat,M=c(),C=c(),showmat=TRUE,plot=FALSE, plotfun = plotGraph)
essentialGraph(amat)

#####Analyse de piste - données Ph.D.#####
setwd(dir = "/Users/nicolas/Documents/Claire/Doc doc doc !/R analysis/Data")
SEM<-read.table("Path analysis_data 3.txt", sep = ",", dec = ".", h=T)
summary(SEM)

#Nettoyage de la variable HABITAT
SEM$HABITAT[SEM$HABITAT == "N/A"] <- NA
SEM$SN[SEM$ISSUE == 1]<-1
SEM$SN[SEM$ISSUE == 2 |SEM$ISSUE == 3 ]<-0

mesHAB<-c(1996,1631,1394,1608)

for (q in mesHAB){
  SEM$HABITAT[SEM$ID == q]<-"Mesic"
}
SEM$HABITAT[SEM$HABITAT == "Wetland/mesic"] <-"Wetland"

#which(is.na(SEM$SN)) #localisation of NAs in the vector
dim(SEM)
#SEM2<-SEM[,-c(1,2,5,7,8)]
#summary(SEM2)
#par(mfrow=c(1,1))
#pairs(SEM2)
attach(SEM) 

#####Package ggm#####
#définition du diagramme causal
require(ggm)
path<-DAG(
  #HABITAT~Ø
  #temp~ Ø
  #prec~ Ø
  #lemg_abun~ Ø
  fox_dens~lmg_abun,
  prim_prod~HABITAT+MEAN_temp+MEAN_prec,
  SN~fox_dens+prim_prod+HABITAT+MEAN_temp+MEAN_prec)

#création du set de base Bu
#Each output vector contains the names of two non-adjacent nodes 
#followed by the names of nodes in the conditioning set (which may be empty).
basiSet(path)

#Test de d-sep
#dsep of hab et temp given an empty conditioning vertices set
dSep(path, first = "HABITAT", second = "MEAN_temp", cond = NULL)

#dsep of lemg et oie given conditioning vertices veg, prec, temp, hab and fox
dSep(path, first = "lmg_abun", second = "SN", cond = c("prim_prod","MEAN_prec", "MEAN_temp", "HABITAT", "fox_dens"))

require(nlme)
require(lme4)

#Independence claim: (hab,temp)|{empty} 
plot(mean_prec)
plot(lemg_abun)
plot(lemg_abun,mean_temp)
lem<-SEM$lemg_abun
class(lem)
temp<-SEM$mean_temp
class(temp)

lme(temp,lem)



#####Bu set#####
#(hab,mean_temp)|{Ø}
#(hab,mean_prec)|{Ø}
#(hab,lemg_abun)|{Ø}
#(hab,prop_repro_dens)|{lemg_abun}
#(mean_temp,lemg_abun)|{Ø}
#(mean_temp, mean_prec)|{Ø}
#(mean_temp, prop_repro_dens)|{lemg_abun}
#(mean_prec, prop_repro_dens)|{lemg_abun}
#(mean_prec, lemg_abun)|{Ø}
#(veg_bioM_C2, prop_repro_dens)|{mean_prec, mean_temp, hab, lemg_abun}
#(veg_bioM_C2, lemg_abun)|{mean_prec, mean_temp, hab}
#(lemg_abun, SN)|{veg_bioM_C2, mean_prec, mean_temp, hab, prop_repro_dens}


########GGM package method - Ph.D. data#########
#Here random effect = year
#Put the conditioning variable in first in the model because we want to control it

#(hab,mean_temp)|{Ø}
summary(glmer(HABITAT~MEAN_temp+(1|AN), na.action=na.omit, family=binomial(link="logit")))
#(hab,mean_prec)|{Ø}
summary(glmer(HABITAT~MEAN_prec+(1|AN), na.action=na.omit, family=binomial(link="logit")))
#(hab,lemg_abun)|{Ø}
summary(glmer(HABITAT~lmg_abun+(1|AN), na.action=na.omit, family=binomial(link="logit")))
#(hab,prop_repro_dens)|{lemg_abun}
summary(glmer(HABITAT~lmg_abun + fox_dens+(1|AN), na.action=na.omit, family=binomial(link="logit")))
#(mean_temp,lemg_abun)|{Ø}
summary(lme(MEAN_temp~lmg_abun, data = SEM,random =~1|AN, na.action = na.omit))
#(mean_temp, mean_prec)|{Ø}
summary(lme(MEAN_temp~MEAN_prec, data = SEM,random =~1|AN, na.action = na.omit))
#In the case of conditioning variables, put them in first in the group of explicative variables
#(mean_temp, prop_repro_dens)|{lemg_abun}
summary(lme(MEAN_temp~lmg_abun +fox_dens, data = SEM,random =~1|AN, na.action = na.omit))
#(mean_prec, prop_repro_dens)|{lemg_abun}
summary(lme(MEAN_prec~lmg_abun +fox_dens, data = SEM,random =~1|AN, na.action = na.omit))
#(mean_prec, lemg_abun)|{Ø}
summary(lme(MEAN_prec~lmg_abun, data = SEM,random =~1|AN, na.action = na.omit))
#(veg_bioM_C2, prop_repro_dens)|{mean_prec, mean_temp, hab, lemg_abun}
summary(lme(prim_prod~lmg_abun+HABITAT+MEAN_prec+MEAN_temp+fox_dens, data = SEM,random =~1|AN, na.action = na.omit))
#(veg_bioM_C2, lemg_abun)|{mean_prec, mean_temp, hab}
summary(lme(prim_prod~MEAN_prec+MEAN_temp+HABITAT+lmg_abun, data = SEM,random =~1|AN, na.action = na.omit))
#(lemg_abun, SN)|{veg_bioM_C2, mean_prec, mean_temp, hab, prop_repro_dens}
summary(lme(lmg_abun~prim_prod+MEAN_prec+MEAN_temp+HABITAT+fox_dens+SN, data = SEM,random =~1|AN, na.action = na.omit))

summary(lme(SN~MEAN_temp,data = SEM,random =~1|AN, na.action = na.omit))
#From now, need to calculate the C probability

#####piecewiseSEM package method - example##### 
#install.packages("piecewiseSEM")
#require(piecewiseSEM)
library(piecewiseSEM)
# Load example data
data(shipley2009)
# Reduce dataset for example
shipley2009.reduced = shipley2009[1:200, ]
# Load model packages
library(lme4)
library(nlme)
# Create list of models
shipley2009.reduced.modlist = list(
  lme(DD ~ lat, random = ~1|site/tree, na.action = na.omit,
    data = shipley2009.reduced),
  lme(Date ~ DD, random = ~1|site/tree, na.action = na.omit,
    data = shipley2009.reduced),
  lme(Growth ~ Date, random = ~1|site/tree, na.action = na.omit,
    data = shipley2009.reduced),
  glmer(Live ~ Growth+(1|site)+(1|tree),
    family=binomial(link = "logit"), data = shipley2009.reduced)
)
# Get goodness-of-fit and AIC
sem.fit(shipley2009.reduced.modlist, shipley2009.reduced, conditional = T)
# Extract path coefficients
sem.coefs(shipley2009.reduced.modlist, shipley2009.reduced)
## Not run:
# Repeat with full dataset as in Shipley (2009)
# Create list of models
shipley2009.modlist = list(
  lme(DD ~ lat, random = ~1|site/tree, na.action = na.omit,
    data = shipley2009),
  lme(Date ~ DD, random = ~1|site/tree, na.action = na.omit,
    data = shipley2009),
  lme(Growth ~ Date, random = ~1|site/tree, na.action = na.omit,
    data = shipley2009),
  glmer(Live ~ Growth+(1|site)+(1|tree),
    family=binomial(link = "logit"), data = shipley2009)
)
# Get goodness-of-fit and AIC
sem.fit(shipley2009.modlist, shipley2009, conditional = T)
# Extract path coefficients
sem.coefs(shipley2009.modlist, shipley2009)
## End(Not run)

#####piecewiseSEM package method - Ph.D. data##### 
library(piecewiseSEM)
# Load model packages
library(lme4)
library(nlme)
###### Causal path #1 #####
#Causal model #1
test1<-list(
  lme(fox_dens~lmg_abun, random = ~1|AN, na.action = na.omit),
  lme(prim_prod~HABITAT+MEAN_temp+MEAN_prec, random = ~1|AN, na.action = na.omit),
  glmer(SN~fox_dens+prim_prod+HABITAT+MEAN_temp+MEAN_prec + (1|AN), family=binomial(link = "logit"))
)
# Get goodness-of-fit and AIC
sem.fit(test1, SEM, conditional = T)
# Extract path coefficients
sem.coefs(test1, SEM)

#Causal model #2
test2<-list(
  lme(fox_dens~lmg_abun, random = ~1|AN, na.action = na.omit),
  lme(prim_prod~HABITAT+MEAN_temp+MAX_prec, random = ~1|AN, na.action = na.omit),
  glmer(SN~fox_dens+prim_prod+HABITAT+MEAN_temp+MAX_prec + (1|AN), family=binomial(link = "logit"))
)
# Get goodness-of-fit and AIC
sem.fit(test2, SEM, conditional = T)
# Extract path coefficients
sem.coefs(test2, SEM)

#Causal model #3
test3<-list(
  lme(fox_dens~lmg_abun, random = ~1|AN, na.action = na.omit),
  lme(prim_prod~HABITAT+MAX_temp+MEAN_prec, random = ~1|AN, na.action = na.omit),
  glmer(SN~fox_dens+prim_prod+HABITAT+MAX_temp+MEAN_prec + (1|AN), family=binomial(link = "logit"))
)
# Get goodness-of-fit and AIC
sem.fit(test3, SEM, conditional = T)
# Extract path coefficients
sem.coefs(test3, SEM)

#Causal model #4
test4<-list(
  lme(fox_dens~lmg_abun, random = ~1|AN, na.action = na.omit),
  lme(prim_prod~HABITAT+MAX_temp+MAX_prec, random = ~1|AN, na.action = na.omit),
  glmer(SN~fox_dens+prim_prod+HABITAT+MAX_temp+MAX_prec + (1|AN), family=binomial(link = "logit"))
)
# Get goodness-of-fit and AIC
sem.fit(test4, SEM, conditional = T)
# Extract path coefficients
sem.coefs(test4, SEM)

#Causal model #5
test5<-list(
  lme(fox_dens~lmg_abun, random = ~1|AN, na.action = na.omit),
  lme(prim_prod~HABITAT+MIN_temp+MEAN_prec, random = ~1|AN, na.action = na.omit),
  glmer(SN~fox_dens+prim_prod+HABITAT+MIN_temp+MEAN_prec + (1|AN), family=binomial(link = "logit"))
)
# Get goodness-of-fit and AIC
sem.fit(test5, SEM, conditional = T)
# Extract path coefficients
sem.coefs(test5, SEM)

#Causal model #6
test6<-list(
  lme(fox_dens~lmg_abun, random = ~1|AN, na.action = na.omit),
  lme(prim_prod~HABITAT+MIN_temp+MAX_prec, random = ~1|AN, na.action = na.omit),
  glmer(SN~fox_dens+prim_prod+HABITAT+MIN_temp+MAX_prec + (1|AN), family=binomial(link = "logit"))
)
# Get goodness-of-fit and AIC
sem.fit(test6, SEM, conditional = T)
# Extract path coefficients
sem.coefs(test6, SEM)

#####Causal path #11 #####
#Causal model #1
test<-list(
  lme(fox_dens~lmg_abun, random = ~1|AN, na.action = na.omit),
  #glmer(SN~fox_dens+HABITAT+lmg_abun+prim_prod+MEAN_prec + (1|AN), family=binomial(link = "logit")),
  glmer(SN~prim_prod+fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), family=binomial(link = "logit"))
)
# Get goodness-of-fit and AIC
sem.fit(test, SEM, conditional = T)
# Extract path coefficients
sem.coefs(test_MEAN, SEM)

#####Causal path #16 #####
#avec températures mean
test_MEAN<-list(
  lme(fox_dens~lmg_abun, random = ~1|AN, na.action = na.omit),
  lme(prim_prod~HABITAT+MEAN_temp+MEAN_prec, random = ~1|AN, na.action = na.omit),
  glmer(SN~fox_dens+prim_prod+HABITAT+ (1|AN), family=binomial(link = "logit"))
)
# Get goodness-of-fit and AIC
sem.fit(test_MEAN, SEM, conditional = T)
# Extract path coefficients
sem.coefs(test_MEAN, SEM)

#Modèle #16 avec températures max
test_MAX<-list(
  lme(fox_dens~lmg_abun, random = ~1|AN, na.action = na.omit),
  lme(prim_prod~HABITAT+MAX_temp+MAX_prec, random = ~1|AN, na.action = na.omit),
  glmer(SN~fox_dens+prim_prod+HABITAT+ (1|AN), family=binomial(link = "logit"))
)
# Get goodness-of-fit and AIC
sem.fit(test_MAX, SEM, conditional = T)
# Extract path coefficients
sem.coefs(test_MAX, SEM)

#####BROUILLON#####

#Modèle #1 avec températures maximum
test_MAX<-list(
  lme(fox_dens~lmg_abun, random = ~1|AN, na.action = na.omit),
  lme(lmg_abun~prim_prod+MEAN_prec+MEAN_temp+HABITAT, random = ~1|AN, na.action = na.omit),
  lme(fox_dens~prim_prod+MEAN_temp+MEAN_prec+HABITAT+lmg_abun, random = ~1|AN, na.action = na.omit),
  glmer(SN~fox_dens+prim_prod+HABITAT+MEAN_temp+MEAN_prec+lmg_abun + (1|AN), family=binomial(link = "logit"))
)
# Get goodness-of-fit and AIC
sem.fit(test_MAX, SEM, conditional = T)
# Extract path coefficients
sem.coefs(test_MAX, SEM)

test0<-list(
  lme(fox_dens~lmg_abun+prim_prod, random = ~1|AN, na.action = na.omit),
  lme(lmg_abun~prim_prod+MEAN_prec+MEAN_temp+HABITAT, random = ~1|AN, na.action = na.omit),
  lme(fox_dens~prim_prod+MEAN_temp+MEAN_prec+HABITAT+lmg_abun, random = ~1|AN, na.action = na.omit),
  glmer(SN~fox_dens+prim_prod+HABITAT+MEAN_temp+MEAN_prec+lmg_abun + (1|AN), family=binomial(link = "logit"))
)
# Get goodness-of-fit and AIC
sem.fit(test0, SEM, conditional = T)


#####ALMOST #1#####
test<-list(
  lme(fox_dens~lmg_abun+prim_prod, random = ~1|AN, na.action = na.omit),
  #glmer(SN~fox_dens+HABITAT+lmg_abun+prim_prod+MEAN_prec + (1|AN), family=binomial(link = "logit")),
  glmer(SN~prim_prod+fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), family=binomial(link = "logit"))
)
# Get goodness-of-fit and AIC
sem.fit(test, SEM, conditional = T)
# Extract path coefficients
sem.coefs(test, SEM)


test<-list(
  lme(fox_dens~lmg_abun+prim_prod, random = ~1|AN, na.action = na.omit),
  glmer(SN~fox_dens+HABITAT+lmg_abun+prim_prod+MEAN_prec + (1|AN), family=binomial(link = "logit")),
  glmer(SN~prim_prod+fox_dens+HABITAT+MEAN_prec+MEAN_temp+ (1|AN), family=binomial(link = "logit")),
  glmer(SN~HABITAT+ (1|AN), family=binomial(link = "logit")),
  glmer(SN~MEAN_prec+ (1|AN), family=binomial(link = "logit")),
  lme(prim_prod~HABITAT, random = ~1|AN, na.action = na.omit),
  lme(prim_prod~MEAN_prec, random = ~1|AN, na.action = na.omit),
  lme(prim_prod~MEAN_temp, random = ~1|AN, na.action = na.omit)
)
# Get goodness-of-fit and AIC
sem.fit(test, SEM, conditional = T)
# Extract path coefficients
sem.coefs(test, SEM)
