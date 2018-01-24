###### Path analysis for the functional response of foxes #######

rm(list = ls()) #clean R memory

setwd(dir = "/Users/nicolas/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
#fan<-read.table("FOX-functional response.txt", h=T, dec=".", sep = ",")
fan<-read.table("FOX-functional response V2.txt", h=T, dec=".", sep = ",") #Utilisation du fichier version 2 car MAJ avec de nouvelles variables. Necessite de regler le probleme de creation de variable de prop_fox_dens time lag + 1 annee
summary(fan)
nane <- na.omit(fan)
summary(nane)
dim(nane)

#Package nécessaires
require(nlme)
require(lme4)
require(piecewiseSEM)
require(ggm)


x11() #to open a graphic window
#dev.set(dev.prev()) #to activate the graphic device when desactivated
plot(fan$year,fan$atq_rate, ylab = "Attack rate (attack nb/second)", xlab = "year")
plot(fan$winAO,fan$atq_rate, ylab = "Attack rate (attack nb/second)", xlab = "winter AO index")
plot(fan$sprAO,fan$atq_rate, ylab = "Attack rate (attack nb/second)", xlab = "spring AO index")
plot(fan$lmg_C1,fan$atq_rate, ylab = "Attack rate (attack nb/second)", xlab = "index of lemming abundance C1")

# Graphic functional response style
plot(fan$lmg_C2, fan$atq_rate, ylab = "Attack rate (attack nb/second)", xlab = "index of lemming abundance C2")
ajout <- with(fan, smooth.spline(lmg_C2,atq_rate))
ajout
lines(ajout, col = "orange")

plot(fan$nest_density,fan$atq_rate, ylab = "Attack rate (attack nb/second)", xlab = "density of goose nests")
ajout <- with(fan, smooth.spline(nest_density, atq_rate))
ajout
lines(ajout, col = "orange")

plot(fan$lmg_C12,fan$atq_rate, ylab = "Attack rate (attack nb/second)", xlab = "index of lemming abundance C12")
plot(fan$nest_density,fan$atq_rate, ylab = "Attack rate (attack nb/second)", xlab = "nest density of geese (nest nb/ha)")

plot(fan$mean_temp,fan$atq_rate, ylab = "Attack rate (attack nb/second)", xlab = "mean temperature (c)")
ajout_temp <- with(fan, smooth.spline(mean_temp,atq_rate))
ajout_temp
lines(ajout_temp, col = "orange")

plot(fan$cumul_prec,fan$atq_rate, ylab = "Attack rate (attack nb/second)", xlab = "cumulative precipitation (mm)")
ajout_prec <- with(fan, smooth.spline(cumul_prec,atq_rate))
ajout_prec
lines(ajout_prec, col = "pink")

plot(fan$nest_succ,fan$atq_rate, ylab = "Attack rate (attack nb/second)", xlab = "nesting success of geese")

boxplot(fan$atq_rate ~ fan$lmg_C2)
boxplot(fan$atq_rate ~ fan$mean_temp)
boxplot(fan$atq_rate ~ fan$cumul_prec)

#### Modèle 1 ####
names(fan)
M1<-list(
  lme(atq_rate~lmg_C1, data = fan, random = ~1|year),
  lm(nest_succ~atq_rate+mean_temp+cumul_prec, data = fan),
  lme(mean_temp~sprAO, data = fan, random = ~1|year),
  lme(cumul_prec~sprAO, data = fan, random = ~1|year))

# Get goodness-of-fit and AIC
sem.fit(M1, fan, conditional = T)
# MISSING PATHS
#important probleme de missing path between lmg and NS, ce quelque soit l'indice de lemming utilise (C1, C2 ou C1_2)

#### Modèle 2 ####
##   *** = modele plausible. Pas de piste manquante et probabilite de C Fisher > 0.05
#Integration de la variable prim_prod en esperant que ce soit le maillon manquant entre lmg et oie
M2<-list(
  lme(atq_rate ~ lmg_C2 + cumul_prec, data = nane, random = ~1|year),
  lm(nest_succ ~ atq_rate + prim_prod , data = nane),
  lm(lmg_C2 ~ prim_prod, data = nane))

# Get goodness-of-fit and AIC
sem.fit(M2, nane, conditional = T)
# Extract path coefficients
sem.coefs(M2, nane)


#### Modèle 2a ####
M2a<-list(
  lme(atq_rate ~ lmg_C2 + cumul_prec, data = nane, random = ~1|year),
  lm(nest_succ ~ atq_rate + prim_prod + cumul_prec, data = nane),
  lm(lmg_C2 ~ prim_prod, data = nane))

# Get goodness-of-fit and AIC
sem.fit(M2a, nane, conditional = T)
# Extract path coefficients
sem.coefs(M2a, nane)



#### Modèle 2b *** ####
M2b<-list(
  lme(atq_rate ~ lmg_C2 + cumul_prec, data = nane, random = ~1|year),
  lm(nest_succ ~ atq_rate + prim_prod + cumul_prec + lmg_C2, data = nane),
  lm(lmg_C2 ~ prim_prod, data = nane))

# Get goodness-of-fit and AIC
sem.fit(M2b, nane, conditional = T)
# Extract path coefficients
sem.coefs(M2b, nane)


#### Modèle 2c ####
M2c<-list(
  lme(atq_rate ~ lmg_C2 + cumul_prec + mean_temp, data = nane, random = ~1|year),
  lm(nest_succ ~ atq_rate + cumul_prec + lmg_C2, data = nane))

# Get goodness-of-fit and AIC
sem.fit(M2c, nane, conditional = T)
# Extract path coefficients
sem.coefs(M2c, nane)


#### Modèle 2d *** ####
M2d<-list(
  lme(atq_rate ~ lmg_C2 + cumul_prec, data = nane, random = ~1|year),
  lm(nest_succ ~ atq_rate + cumul_prec + lmg_C2 + mean_temp, data = nane))

# Get goodness-of-fit and AIC
sem.fit(M2d, nane, conditional = T)
# Extract path coefficients
sem.coefs(M2d, nane)
#####Essayer de comprendre pourquoi certains modele ne fonctionnent pas
#Par exemple
M<-list(
lme(atq_rate ~ lmg_C2 + cumul_prec + mean_temp, data = nane, random = ~1|year),
lm(nest_succ ~ atq_rate + cumul_prec + mean_temp, data = nane),
lm(lmg_C2 ~ prim_prod, data = nane))
sem.fit(M, nane, conditional = TRUE)
