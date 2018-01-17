###### Path analysis for the functional response of foxes #######

rm(list = ls()) #clean R memory

setwd(dir = "/Users/nicolas/Dropbox/Path analysis - UdeS/R analysis")
fan<-read.table("FOX-functional response.txt", h=T, dec=".", sep = ",")
f<-read.table("FOX-functional response V2.txt", h=T, dec=".", sep = ",")
summary(fan)

#Package nécessaires
require(nlme)
require(lme4)
require(piecewiseSEM)
require(ggm)

fan<-na.omit(fan)
summary(fan)
dim(fan)

plot(fan$year,fan$atq_rate, ylab = "Attack rate (attack nb/second)", xlab = "year")
plot(fan$winAO,fan$atq_rate, ylab = "Attack rate (attack nb/second)", xlab = "winter AO index")
plot(fan$sprAO,fan$atq_rate, ylab = "Attack rate (attack nb/second)", xlab = "spring AO index")
plot(fan$lmg_abun,fan$atq_rate, ylab = "Attack rate (attack nb/second)", xlab = "index of lemming abundance")
plot(fan$nest_density,fan$atq_rate, ylab = "Attack rate (attack nb/second)", xlab = "nest density of geese (nest nb/ha)")
plot(fan$mean_temp,fan$atq_rate, ylab = "Attack rate (attack nb/second)", xlab = "mean temperature (c)")
plot(fan$cumul_prec,fan$atq_rate, ylab = "Attack rate (attack nb/second)", xlab = "cumulative precipitation (mm)")
plot(fan$nest_succ,fan$atq_rate, ylab = "Attack rate (attack nb/second)", xlab = "nesting success of geese")


#### Modèle 1 ####
names(fan)
M1<-list(
  lme(atq_rate~lmg_abun, data = fan, random = ~1|year),
  lm(nest_succ~atq_rate+mean_temp+cumul_prec, data=fan),
  lme(mean_temp~sprAO, data = fan, random = ~1|year),
  lme(cumul_prec~sprAO, data = fan, random = ~1|year))

# Get goodness-of-fit and AIC
sem.fit(M1, fan, conditional = T)
# MISSING PATHS
