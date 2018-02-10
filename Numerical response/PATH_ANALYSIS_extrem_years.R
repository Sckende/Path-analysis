setwd(dir = "/Users/nicolas/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
rm(list = ls()) #clean R memory

#Package nécessaires
require(nlme)
require(lme4)
require(piecewiseSEM)
require(ggm)

#Chargement des données de path analysis
f <- read.table("Path analysis_data 3bis.txt", sep = ",", dec = ".", h = T)

# BDD date nidification oie pour calcul de température moyenne
g <- read.table("GOOSE_breeding_informations.txt", sep = "\t", dec = ",", h = T)
head(g)

# BDD temperatures
t <- read.table("TEMP_Tair moy 1989-2017 BYLCAMP.txt", sep = "\t", dec = ",", h = T)
t <- t[t$YEAR >= 1996 & !t$YEAR == 2017,]; head(t)

# BDD precipitations
rain <- read.table("PREC_precipitation_Bylot_1996-2016.txt", h = T, sep = "\t", dec = ",")
head(rain)

#### Summer precipitation trends ####
rain <- na.omit(rain)
summary(rain)
h <- as.data.frame(tapply(rain$RAIN, rain$YEAR, sum)) # Cumulative precipitation computation
h <- cbind(1996:2017, h); names(h) <- c("year", "rain")
head(h)
h <- h[-22, ] # retrait 2017
h$moy <- mean(h$rain)

# Graphic
plot(h$year, h$rain, xlab = "Year", ylab = "Cumulative summer precipitation", ylim = c(0, 155), xlim = c(1996, 2016), bty = "n", yaxt = "n", xaxt = "n", cex = 2, cex.lab = 2, col = "orange", pch = 19)
lines(smooth.spline(h$year, h$rain, df = 3), col = "orange", lwd = 4)
lines(h$year, h$moy, type = "l", lty = 4, col = "orange")
#Modification x axis
xtick <- seq(1996, 2016, by = 1)
axis(side = 1, at = xtick)
#Modification y axis
ytick<-seq(0, 155, by = 10)
axis(side = 2, at = ytick)
dev.off()

# Création de deux jeux de données avec années humide (supérieure à la moyenne) et année sèche (inférieure à la moyenne)

# BDD a utiliser si analyses avec lmg_C1
mC1 <- f[,-c(1, 3:8, 10:14, 16, 18, 25, 30, 32, 34, 35, 42:47 )]
summary(mC1)


mC1$prec_year <- h$rain[match(mC1$AN, h$year)]
HUM <- mC1[mC1$prec_year >= unique(h$moy),] 
SEC <- mC1[mC1$prec_year < unique(h$moy),] 



# Superposition des points sur graphique précédent
# Check-point
par(new = TRUE)
par(bg = "transparent")
points(HUM$AN, HUM$prec_year, col = "green")
points(SEC$AN, SEC$prec_year, col = "red")

# Récupération du modèle de piste sélection dans la dernière analyse et test avec les deux jeux de données différents

#### Modele de piste - Années humides ####
mHUM <- list(
  lm(prop_fox_dens ~ lmg_C1 + winAO + cumul_prec + MEAN_temp, data = HUM),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = HUM, family = binomial(link = "logit")),
  lm(lmg_C1 ~ winAO + MEAN_temp + cumul_prec, data = HUM))
# Get goodness-of-fit and AIC
sem.fit(mHUM, HUM, conditional = T, corr.errors = "MEAN_temp ~~ cumul_prec")

#NO significant missing paths
sem.coefs(mHUM, HUM)
#sem.plot(mHUM, HUM, show.nonsig = T)

#### Modele de piste - Années sèches ####
mSEC <- list(
  lm(prop_fox_dens ~ lmg_C1 + winAO + cumul_prec + MEAN_temp, data = SEC),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = SEC, family = binomial(link = "logit")),
  lm(lmg_C1 ~ winAO + MEAN_temp + cumul_prec, data = SEC))
# Get goodness-of-fit and AIC
sem.fit(mSEC, SEC, conditional = T, corr.errors = "MEAN_temp ~~ cumul_prec")

#NO significant missing paths
sem.coefs(mSEC, SEC)
#sem.plot(mSEC, SEC, show.nonsig = T)


#### Summer temperature trends ####

# Calcul des valeurs par rapport aux dates d'initiation et d'éclosion moyennes par année

# Obtention des jours juliens pour les dates GOOSE
Sys.setlocale(category = "LC_TIME", locale = "en_US") #setting in english to read month
g$lay_date_jj <- paste(g$LAY_DATE, g$YEAR, sep = " ")
g$lay_date_jj <- strptime(g$lay_date_jj, format = "%d %B %Y")
g$lay_date_jj <- g$lay_date_jj$yday
g$lay_date_jj[g$YEAR == 1996 | g$YEAR == 2000 | g$YEAR == 2004 | g$YEAR == 2008 | g$YEAR == 2012 | g$YEAR == 2016] <- g$lay_date_jj[g$YEAR == 1996 | g$YEAR == 2000 | g$YEAR == 2004 | g$YEAR == 2008 | g$YEAR == 2012 | g$YEAR == 2016] + 1

g$hatch_date_jj <- paste(g$HATCH_DATE, g$YEAR, sep = " ")
g$hatch_date_jj <- strptime(g$hatch_date_jj, format = "%d %B %Y")
g$hatch_date_jj <- g$hatch_date_jj$yday
g$hatch_date_jj[g$YEAR == 1996 | g$YEAR == 2000 | g$YEAR == 2004 | g$YEAR == 2008 | g$YEAR == 2012 | g$YEAR == 2016] <- g$hatch_date_jj[g$YEAR == 1996 | g$YEAR == 2000 | g$YEAR == 2004 | g$YEAR == 2008 | g$YEAR == 2012 | g$YEAR == 2016] + 1

# Obtention des jours juliens pour les dates TEMP 
head(t)
t$jj <- strptime(paste(t$DAY, t$MONTH, t$YEAR, sep = "-"), format = "%d-%m-%Y")
t$jj <- t$jj$yday + 1 #comme dates continues pas besoin de traiter separemment annees bissextiles et annees ordinaires

# Match des JJ date initiation et éclosion entre BDD g et t

t$INI_date <- g$lay_date_jj[match(t$YEAR, g$YEAR)]
t$ECLO_date <- g$hatch_date_jj[match(t$YEAR, g$YEAR)]

# Check-point
t$ECLO_date[t$YEAR == 1996] == g$hatch_date_jj[g$YEAR == 1996] 
 # .... À continuer si ce choix pour les analyse

# Ici moyenne de température par année entre date globale initiation et éclosion (non spécifique par année !!!!)
c <- as.data.frame(tapply(t$TEMP[t$jj >= 155 & t$jj <= 200], t$YEAR[t$jj >= 155 & t$jj <= 200], mean))
c <- cbind(1996:2016, c); names(c) <- c("year", "tempe")
c$moy <- mean(c$tempe)
head(c)

# Graphic
dev.off()
plot(c$year, c$tempe, xlab = "Year", ylab = "Mean temperature during nidification (°c)", ylim = c(3, 5.3), xlim = c(1996, 2016), bty = "n", xaxt = "n", cex = 2, cex.lab = 2, col = "orange", pch = 19)
lines(smooth.spline(c$year, c$tempe, df = 3), col = "orange", lwd = 4)
lines(c$year, c$moy, type = "l", lty = 4, col = "orange")
#Modification x axis
xtick <- seq(1996, 2016, by = 1)
axis(side = 1, at = xtick)
dev.off()

# Ici deux possibilité de découpe de BDD 
# 1 - comme les précipitations, années chaudes vs. années froides
# 2 - Découpe avant 2006 et qprès 2006

# Creation de deux BDD avec années chaudes vs. années froides
mC1$tempe_year <- c$tempe[match(mC1$AN, c$year)]
HOT <- mC1[mC1$tempe_year >= unique(c$moy),] 
COLD <- mC1[mC1$tempe_year < unique(c$moy),] 

# Superposition des points sur graphique précédent
# Check-point
par(new = TRUE)
par(bg = "transparent")
points(HOT$AN, HOT$tempe_year, col = "red")
points(COLD$AN, COLD$tempe_year, col = "blue")

#### Modele de piste - Années chaudes ####
mHOT <- list(
  lm(prop_fox_dens ~ lmg_C1 + winAO + cumul_prec + MEAN_temp, data = HOT),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = HOT, family = binomial(link = "logit")),
  lm(lmg_C1 ~ winAO + MEAN_temp + cumul_prec, data = HOT))
# Get goodness-of-fit and AIC
sem.fit(mHOT, HOT, conditional = T, corr.errors = "MEAN_temp ~~ cumul_prec")

#NO significant missing paths
sem.coefs(mHOT, HOT)
#sem.plot(mHOT, HOT, show.nonsig = T)

#### Modele de piste - Années sèches ####
mCOLD <- list(
  lm(prop_fox_dens ~ lmg_C1 + winAO + cumul_prec + MEAN_temp, data = COLD),
  glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = COLD, family = binomial(link = "logit")),
  lm(lmg_C1 ~ winAO + MEAN_temp + cumul_prec, data = COLD))
# Get goodness-of-fit and AIC
sem.fit(mCOLD, COLD, conditional = T, corr.errors = "MEAN_temp ~~ cumul_prec")

#NO significant missing paths
sem.coefs(mCOLD, COLD)
#sem.plot(mCOLD, COLD, show.nonsig = T)
