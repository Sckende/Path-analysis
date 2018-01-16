#####-------------------------PATH ANALYSIS PhD v4-------------------------#####

setwd(dir = "/Users/nicolas/Documents/Claire/Doc doc doc !/R analysis/Data")
fonc<-read.table("Path analysis_data 4.txt", h=T, dec=".", sep = ",")

summary(fonc)

#retrait de l'année 2016 car données de températures non complètes
fonc<-fonc[-8,]

#Package nécessaires
require(nlme)
require(lme4)
require(piecewiseSEM)
require(ggm)

##### Modèle A #####

#retrait de 1996 à cause du NA dans lmg_abun
fonc1<-fonc[-1,]
Ma<-list(
  lme(fox_dens~lmg_abun+moy_Tc+moy_pr, data = fonc1, random = ~1|year),
  glmer(nest_succ~fox_dens+moy_Tc+moy_pr+ (1|year), data=fonc1, family=binomial(link = "logit")),
  lme(moy_Tc~sprAO, data = fonc1, random = ~1|year),
  lme(moy_pr~sprAO, data = fonc1, random = ~1|year),
  lme(lmg_abun~sprAO, data = fonc1, random = ~1|year))

# Get goodness-of-fit and AIC
sem.fit(Ma, fonc1, conditional = T) # DOESN'T WORK !!!!!!

##### Modèle B #####

#utilisation de lag à la place de lmg_abun

Mb<-list(
  lme(fox_dens~lag+moy_Tc+moy_pr, data = fonc, random = ~1|year),
  glmer(nest_succ~fox_dens+moy_Tc+moy_pr+ (1|year), data=fonc, family=binomial(link = "logit")),
  lme(moy_Tc~sprAO, data = fonc, random = ~1|year),
  lme(moy_pr~sprAO, data = fonc, random = ~1|year),
  lme(lag~sprAO, data = fonc, random = ~1|year))

# Get goodness-of-fit and AIC
sem.fit(Mb, fonc, conditional = T) # DOESN'T WORK !!!!!!
