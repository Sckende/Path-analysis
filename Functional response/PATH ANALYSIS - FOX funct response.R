###### Path analysis for the functional response of foxes #######

rm(list = ls()) #clean R memory

setwd(dir = "/Users/nicolas/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
#fan<-read.table("FOX-functional response.txt", h=T, dec=".", sep = ",")
fox<-read.table("FOX-functional response V2.txt", h=T, dec=".", sep = ",") #Utilisation du fichier version 2 car MAJ avec de nouvelles variables. fan_pp differe de fan avec la présence de la variable prim_prod qui introduit plus de NA

fan_pp <- na.omit(fox) #A utiliser si analyse avec prim_prod

fan <- fox[, -31]
summary(fan)

fan_C2 <- na.omit(fan) # A utiliser pour les analyses avec lmg_C2
summary(fan)
dim(fan)

#Package nécessaires
require(nlme)
require(lme4)
require(piecewiseSEM)
require(ggm)

panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}


x11() #to open a graphic window
#dev.set(dev.prev()) #to activate the graphic device when desactivated
plot(fan$year,fan$atq_rate, ylab = "Attack rate (attack nb/second)", xlab = "year")
plot(fan$winAO,fan$atq_rate, ylab = "Attack rate (attack nb/second)", xlab = "winter AO index")
plot(fan$sprAO,fan$atq_rate, ylab = "Attack rate (attack nb/second)", xlab = "spring AO index")
plot(fan$lmg_C1,fan$atq_rate, ylab = "Attack rate (attack nb/second)", xlab = "index of lemming abundance C1")

# Graphic functional response style
plot(fan_C2$lmg_C2, fan_C2$atq_rate, ylab = "Attack rate (attack nb/second)", xlab = "index of lemming abundance C2")
ajout <- with(fan_C2, smooth.spline(lmg_C2,atq_rate))
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
boxplot(fan$atq_rate ~ fan$lmg_year)

#### Modèle 1 - avec lmg_C1 ####
#Exploration visuelle des donnees
names(fan)
X11()
pairs(fan[,c(8, 12, 24, 27, 29, 14)], upper.panel = panel.cor)

M1<-list(
  lme(atq_rate~lmg_C1, data = fan, random = ~1|year),
  lm(nest_succ~atq_rate+mean_temp+cumul_prec, data = fan),
  lme(mean_temp~sprAO, data = fan, random = ~1|year),
  lme(cumul_prec~sprAO, data = fan, random = ~1|year))

# Get goodness-of-fit and AIC
sem.fit(M1, fan, conditional = T)
# Extract path coefficients
sem.coefs(M1, fan)

# MISSING PATHS
#important probleme de missing path between lmg and NS, ce quelque soit l'indice de lemming utilise (C1, C2 ou C1_2)

#### Idem precedent + nest_succ -- lmg_C1 - M1a ####
#Exploration visuelle des donnees
names(fan)
X11()
pairs(fan[,c(8, 12, 24, 27, 29, 14)], upper.panel = panel.cor)

M1a <-list(
  lme(atq_rate ~ lmg_C1, data = fan, random = ~1|year),
  lm(nest_succ ~ atq_rate + mean_temp + cumul_prec + lmg_C1, data = fan),
  lme(mean_temp ~ sprAO, data = fan, random = ~1|year),
  lme(cumul_prec ~ sprAO, data = fan, random = ~1|year))

# Get goodness-of-fit and AIC
sem.fit(M1a, fan, conditional = T)
# Extract path coefficients
sem.coefs(M1a, fan)

#### Idem precedent + nest_succ -- sprAO - M1b ####
#Exploration visuelle des donnees
names(fan)
X11()
pairs(fan[,c(8, 12, 24, 27, 29, 14)], upper.panel = panel.cor)

M1b <-list(
  lme(atq_rate ~ lmg_C1, data = fan, random = ~1|year),
  lm(nest_succ ~ atq_rate + mean_temp + cumul_prec + lmg_C1 + sprAO, data = fan),
  lme(mean_temp ~ sprAO, data = fan, random = ~1|year),
  lme(cumul_prec ~ sprAO, data = fan, random = ~1|year))

# Get goodness-of-fit and AIC
sem.fit(M1b, fan, conditional = T)
# Extract path coefficients
sem.coefs(M1b, fan)

#### Idem precedent + lmg -- prec et temp - M1c ####
#Exploration visuelle des donnees
names(fan)
X11()
pairs(fan[,c(8, 12, 24, 27, 29, 14)], upper.panel = panel.cor)

M1c <-list(
  lme(atq_rate ~ lmg_C1, data = fan, random = ~1|year),
  lm(nest_succ ~ atq_rate + mean_temp + cumul_prec + lmg_C1 + sprAO, data = fan),
  lme(mean_temp ~ sprAO, data = fan, random = ~1|year),
  lme(cumul_prec ~ sprAO, data = fan, random = ~1|year),
  lm(lmg_C1 ~ mean_temp + cumul_prec, data = fan))

# Get goodness-of-fit and AIC
sem.fit(M1c, fan, conditional = T)
# Extract path coefficients
sem.coefs(M1c, fan)

#### Idem sans atq_rate -- lmg_C1 - M1d ####
#Exploration visuelle des donnees
names(fan)
X11()
pairs(fan[,c(8, 12, 24, 27, 29, 14)], upper.panel = panel.cor)

M1d <-list(
  #lme(atq_rate ~ lmg_C1, data = fan, random = ~1|year),
  lm(nest_succ ~ atq_rate + mean_temp + cumul_prec + lmg_C1 + sprAO, data = fan),
  lme(mean_temp ~ sprAO, data = fan, random = ~1|year),
  lme(cumul_prec ~ sprAO, data = fan, random = ~1|year),
  lm(lmg_C1 ~ mean_temp + cumul_prec, data = fan))

# Get goodness-of-fit and AIC
sem.fit(M1d, fan, conditional = T)
# Extract path coefficients
sem.coefs(M1d, fan)

#### Modèle 2 - avec lmg_C2 et prim_prod ####
##   *** = modele plausible. Pas de piste manquante et probabilite de C Fisher > 0.05
#Integration de la variable prim_prod en esperant que ce soit le maillon manquant entre lmg et oie
M2<-list(
  lme(atq_rate ~ lmg_C2 + cumul_prec, data = fan_pp, random = ~1|year),
  lm(nest_succ ~ atq_rate + prim_prod , data = fan_pp),
  lm(lmg_C2 ~ prim_prod, data = fan_pp))

# Get goodness-of-fit and AIC
sem.fit(M2, fan_pp, conditional = T)
# Extract path coefficients
sem.coefs(M2, fan_pp)


#### Modèle 2a ####
M2a<-list(
  lme(atq_rate ~ lmg_C2 + cumul_prec, data = fan_pp, random = ~1|year),
  lm(nest_succ ~ atq_rate + prim_prod + cumul_prec, data = fan_pp),
  lm(lmg_C2 ~ prim_prod, data = fan_pp))

# Get goodness-of-fit and AIC
sem.fit(M2a, fan_pp, conditional = T)
# Extract path coefficients
sem.coefs(M2a, fan_pp)


#### Modèle 2b *** ####
M2b<-list(
  lme(atq_rate ~ lmg_C2 + cumul_prec, data = fan_pp, random = ~1|year),
  lm(nest_succ ~ atq_rate + prim_prod + cumul_prec + lmg_C2, data = fan_pp),
  lm(lmg_C2 ~ prim_prod, data = fan_pp))

# Get goodness-of-fit and AIC
sem.fit(M2b, fan_pp, conditional = T)
# Extract path coefficients
sem.coefs(M2b, fan_pp)


#### Modèle 3 - sans prim_prod et lmg_C2 ####
M3<-list(
  lme(atq_rate ~ lmg_C2 + cumul_prec + mean_temp, data = fan_C2, random = ~1|year),
  lm(nest_succ ~ atq_rate + cumul_prec + lmg_C2, data = fan_C2))

# Get goodness-of-fit and AIC
sem.fit(M3, fan_C2, conditional = T)
# Extract path coefficients
sem.coefs(M3, fan_C2)


#### Modèle 3a *** ####
M3a <- list(
  lme(atq_rate ~ lmg_C2 + cumul_prec, data = fan_C2, random = ~1|year),
  lm(nest_succ ~ atq_rate + cumul_prec + lmg_C2 + mean_temp, data = fan_C2))

# Get goodness-of-fit and AIC
sem.fit(M3a, fan_C2, conditional = T)
# Extract path coefficients
sem.coefs(M3a, fan_C2)

#### Modele 4 - avec lmg_year ####
M4 <- list( 
  lme(atq_rate ~ lmg_year, data = fan, random = ~1|year),
  lm(nest_succ ~ atq_rate+mean_temp+cumul_prec, data = fan))
# Get goodness-of-fit and AIC
sem.fit(M4, fan, conditional = T)
# Extract path coefficients
sem.coefs(M4, fan)

#### Modele 4 a*** ####
M4a <- list( 
  lme(atq_rate ~ lmg_year, data = fan, random = ~1|year),
  lm(nest_succ ~ atq_rate + mean_temp + cumul_prec + lmg_year, data = fan))
# Get goodness-of-fit and AIC
sem.fit(M4a, fan, conditional = T)
# Extract path coefficients
sem.coefs(M4a, fan)

#### Modele 4 b*** ####
M4b <- list( 

  lm(nest_succ ~ atq_rate + mean_temp + cumul_prec + lmg_year, data = fan))
# Get goodness-of-fit and AIC
sem.fit(M4b, fan, conditional = T)
# Extract path coefficients
sem.coefs(M4a, fan)

#### les modeles a trois etoiles n'ont peut etre pas de sens biologique ####
#####Essayer de comprendre pourquoi certains modele ne fonctionnent pas
#Par exemple
M<-list(
lme(atq_rate ~ lmg_C2 + cumul_prec + mean_temp, data = nane, random = ~1|year),
lm(nest_succ ~ atq_rate + cumul_prec + mean_temp, data = nane),
lm(lmg_C2 ~ prim_prod, data = nane))
sem.fit(M, nane, conditional = TRUE)
