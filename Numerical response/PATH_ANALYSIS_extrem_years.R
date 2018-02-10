setwd(dir = "/Users/nicolas/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
rm(list = ls()) #clean R memory

#Package nécessaires
require(nlme)
require(lme4)
require(piecewiseSEM)
require(ggm)

#Chargement des données
f <- read.table("Path analysis_data 3bis.txt", sep = ",", dec = ".", h = T)

# BDD a utiliser si analyses avec lmg_C2 ou lmg_C1_C2
mC2_pp <- f[,-c(1, 3:8, 10:14, 16, 18, 25, 42:47 )] # si modeles avec utilisation de la variable prim_prod
mC2 <- f[,-c(1, 3:8, 10:14, 16, 18, 25, 35, 42:47 )]
mC2 <- na.omit(mC2)
summary(mC2)

# BDD a utiliser si analyses avec lmg_C1
mC1 <- f[,-c(1, 3:8, 10:14, 16, 18, 25, 30, 32, 34, 35, 42:47 )]
mC1_pp <- f[,-c(1, 3:8, 10:14, 16, 18, 25, 30, 32, 34, 42:47 )] # si modeles avec utilisation de la variable prim_prod
summary(mC1)

# BDD temperatures
t <- read.table("TEMP_Tair moy 1989-2017 BYLCAMP.txt", sep = "\t", dec = ",", h = T)
t <- t[t$YEAR >= 1996 & !t$YEAR == 2017,]; head(t)

# BDD precipitations
rain <- read.table("PREC_precipitation_Bylot_1996-2016.txt", h = T, sep = "\t", dec = ",")
head(rain)

#### Summer precipitation trends ####
head(rain)
rain <- na.omit(rain)
summary(rain)
h <- as.data.frame(tapply(rain$RAIN, rain$YEAR, sum))
h <- cbind(1996:2017, h); names(h) <- c("year", "rain")
head(h)
h <- h[-22,]
h$moy <- mean(h$rain)