rm(list = ls()) #clean R memory

setwd(dir = "C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
list.files()

#### WINTER, SPRING & SUMMER AO ####
  # Winter AO = November to April
  # Spring AO = 20 MAy - 20 June
  # Summer AO =

AO<-read.csv("AO_saisonnier.txt", sep = ",", dec = ".")
head(AO)

# png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 3 - Path analysis/FOX numerical response/ARTICLE Ph.D. 3/VERSION FINALE V1/Figures/AO_seasons.tiff",
#     res=300,
#     width=15,
#     height= 25,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")
#x11()
par(mfrow = c(3, 1), mar = c(1, 5, 1, 1))

plot(AO$YEAR[AO$YEAR <= 2016 & AO$YEAR >= 1996],
     AO$winAO[AO$YEAR <= 2016 & AO$YEAR >= 1996],
     xlab = "",
     ylab = "",
     xaxp = c(1996, 2016, 10),
     ylim = c(-2.0, 1.5),
     bty = "n",
     yaxt = "s",
     xaxt = "n",
     cex = 1,
     cex.lab = 1,
    cex.axis = 1.5,
     col = "orange",
     pch = 19,
     lwd = 2,
     type = 'b',
     las = 2)
lines(AO$YEAR[AO$YEAR <= 2016 & AO$YEAR >= 1996],
      rep(mean(AO$winAO[AO$YEAR <= 2016 & AO$YEAR >= 1996]), 21),
      col = "orange",
      type = "l",
      lty = 4,
      lwd = 2)
legend(1995,
       1.8,
       "Winter AO index",
       bty = "n",      
       cex = 2,
       text.col = "Orange")
plot(AO$YEAR[AO$YEAR <= 2016 & AO$YEAR >= 1996],
     AO$sprAO[AO$YEAR <= 2016 & AO$YEAR >= 1996],
     xlab = "",
     ylab = "",
     xaxp = c(1996, 2016, 10),
     ylim = c(-2.0, 1.5),
     bty = "n",
     yaxt = "s",
     xaxt = "n",
     cex = 1,
     cex.lab = 1,
     cex.axis = 1.5,
     col = "orange",
     pch = 19,
     lwd = 2,
     type = 'b',
     las = 2)
lines(AO$YEAR[AO$YEAR <= 2016 & AO$YEAR >= 1996],
      rep(mean(AO$sprAO[AO$YEAR <= 2016 & AO$YEAR >= 1996]), 21),
      col = "orange",
      type = "l",
      lty = 4,
      lwd = 2)
legend(1995,
       1.8,
       "Spring AO index",
       bty = "n",      
       cex = 2,
       text.col = "Orange")
par(mar = c(2, 5, 1, 1))
plot(AO$YEAR[AO$YEAR <= 2016 & AO$YEAR >= 1996],
     AO$sumAO[AO$YEAR <= 2016 & AO$YEAR >= 1996],
     xlab = "",
     ylab = "",
     xaxp = c(1996, 2016, 10),
     ylim = c(-2.0, 1.5),
     bty = "n",
     yaxt = "s",
     xaxt = "n",
     cex = 1,
     cex.lab = 1,
     cex.axis = 1.5,
     col = "orange",
     pch = 19,
     lwd = 2,
     type = 'b',
     las = 2)
lines(AO$YEAR[AO$YEAR <= 2016 & AO$YEAR >= 1996],
      rep(mean(AO$sumAO[AO$YEAR <= 2016 & AO$YEAR >= 1996]), 21),
      col = "orange",
      type = "l",
      lty = 4,
      lwd = 2)
axis(side = 1,
     at = 1996:2016,
     lwd = 1,
     cex.axis = 1.5)
legend(1995,
       1.8,
       "Summer AO index",
       bty = "n",      
       cex = 2,
       text.col = "Orange")
dev.off()
#### TEMPERATURES AND RAINFALL PLOTS ####

gg <- read.table("GOOSE_breeding_informations_1989_2017.txt", sep = "\t", dec = ".", h = T)
head(gg); summary(gg)

tt <- read.table("TEMP_Tair moy 1989-2017 BYLCAMP.txt", sep = "\t", dec = ",", h = T)
head(tt); summary(tt)

AO <- read.table("AO_daily.txt", h = T, sep = "\t", dec = ".")
AO <- AO[AO$YEAR >= 1996 & !AO$YEAR == 2017,]; head(AO); summary(AO)

rain <- read.table("PREC_precipitation_Bylot_1995-2017.txt", h = T, sep = "\t", dec = ",")
rain <- rain[!rain$YEAR == 2017,]; head(rain); summary(rain)
rain <- na.omit(rain)


#### Traitement des jours juliens ####

#### Obtention des jours juliens pour les dates GOOSE
Sys.setlocale(category = "LC_TIME", locale = "English") #en_US.utf8 pour linux, en_US pour macOS, English pour Windows 
#setting in english to read month
gg$lay_date_jj <- paste(gg$LAY_DATE, gg$YEAR, sep = " ")
gg$lay_date_jj <- strptime(gg$lay_date_jj, format = "%d %B %Y")
gg$lay_date_jj <- gg$lay_date_jj$yday +1

gg$hatch_date_jj <- paste(gg$HATCH_DATE, gg$YEAR, sep = " ")
gg$hatch_date_jj <- strptime(gg$hatch_date_jj, format = "%d %B %Y")
gg$hatch_date_jj <- gg$hatch_date_jj$yday +1

#### Obtention des jours juliens pour les dates TEMP
head(tt)
tt$jj <- strptime(paste(tt$DAY, tt$MONTH, tt$YEAR, sep = "-"), format = "%d-%m-%Y")
tt$jj <- tt$jj$yday + 1

# Exemple de Vérification de la valeur des JJ pour les années bissextiles (366 jours - 1996, 2000, 2004, 2008, 2012, 2016) et non bissextiles
#NONbiss <- g[!(g$YEAR == 1996 | g$YEAR == 2000 | g$YEAR == 2004 | g$YEAR == 2008 | g$YEAR == 2012 | g$YEAR == 2016),]
#biss <- g[g$YEAR == 1996 | g$YEAR == 2000 | g$YEAR == 2004 | g$YEAR == 2008 | g$YEAR == 2012 | g$YEAR == 2016,]
#plot(biss$LAY_DATE, biss$lay_date_jj)
#plot(biss$HATCH_DATE, biss$hatch_date_jj)

#### Temperatures trends ####
#### Relations entre les temperatures et la periode de nidification de oies ####
x11(title = "Nidification temperature trends between 1989 & 2016 ")

#dev.off()
g <- gg[!gg$YEAR == 2017,]
t <- tt[!tt$YEAR == 2017,]
x11()
par(mfrow = c(5, 6))
#for (i in 1996:2016) {
for (i in g$YEAR){
  plot(t$jj[t$jj >= g$lay_date_jj[g$YEAR == i] & t$jj <= g$hatch_date_jj[g$YEAR == i] & t$YEAR == i],
       t$TEMP[t$jj >= g$lay_date_jj[g$YEAR == i] & t$jj <= g$hatch_date_jj[g$YEAR == i] & t$YEAR == i],
       main = i,
       xlab = "Julian day",
       ylab = "temp",
       ylim = c(-1, 14),
       xlim = c(158, 195))
  ajout <- with(t, smooth.spline(t$jj[t$jj >= g$lay_date_jj[g$YEAR == i] & t$jj <= g$hatch_date_jj[g$YEAR == i] & t$YEAR == i],
                                 t$TEMP[t$jj >= g$lay_date_jj[g$YEAR == i] & t$jj <= g$hatch_date_jj[g$YEAR == i] & t$YEAR == i],
                                 df = 2))
  ajout
  lines(ajout, col = "blue")
}
#dev.copy2pdf("Nidifi_temp_trends_1996-2015.pdf")
dev.off()

#### Rainfall trends ####
#### Relations entre les précipitations et la periode de nidification de oies ####
x11(title = "Nidification rainfall trends between 1995 & 2016 ")

#dev.off()
x11()
par(mfrow = c(4, 6))
for (i in unique(rain$YEAR)) {
  plot(rain$JJ[rain$JJ >= g$lay_date_jj[g$YEAR == i] & rain$JJ <= g$hatch_date_jj[g$YEAR == i] & rain$YEAR == i],
       rain$RAIN[rain$JJ >= g$lay_date_jj[g$YEAR == i] & rain$JJ <= g$hatch_date_jj[g$YEAR == i] & rain$YEAR == i],
       main = i,
       xlab = "Julian day",
       ylab = "Rainfall",
       ylim = c(-1, max(rain$RAIN)),
       xlim = c(158, 195))
  ajout <- with(rain,
                smooth.spline(rain$JJ[rain$JJ >= g$lay_date_jj[g$YEAR == i] & rain$JJ <= g$hatch_date_jj[g$YEAR == i] & rain$YEAR == i],
                              rain$RAIN[rain$JJ >= g$lay_date_jj[g$YEAR == i] & rain$JJ <= g$hatch_date_jj[g$YEAR == i] & rain$YEAR == i],
                              df = 2))
  ajout
  lines(ajout, col = "blue")
}
#dev.copy2pdf("Nidifi_temp_trends_1996-2015.pdf")
dev.off()


#### Calcul des valeurs de précipitations cumulées et de températures moyennes par années entre les dates moyennes d'initiation et d'éclosion

WEA <- NULL

for(i in g$YEAR){
  YEAR <- i
  LAY <- g$lay_date_jj[g$YEAR == i]
  HATCH <- g$hatch_date_jj[g$YEAR == i]
  meanTEMP <- mean(t$TEMP[t$YEAR == i & t$jj <= HATCH & t$jj >= LAY])
  sdTEMP <- sd(t$TEMP[t$YEAR == i & t$jj <= HATCH & t$jj >= LAY])
  varTEMP <- var(t$TEMP[t$YEAR == i & t$jj <= HATCH & t$jj >= LAY])
  
  c <- data.frame(YEAR, LAY, HATCH, meanTEMP, sdTEMP, varTEMP)
  
  WEA <- rbind(WEA, c)
}

summary(WEA)


WEA.1 <- NULL
for(i in unique(rain$YEAR)){
  YEAR <- i
  LAY <- g$lay_date_jj[g$YEAR == i]
  HATCH <- g$hatch_date_jj[g$YEAR == i]
  summerRAIN <- sum(rain$RAIN[rain$YEAR == i])
  cumRAIN <- sum(rain$RAIN[rain$YEAR == i & rain$JJ <= HATCH & rain$JJ >= LAY])
  c <- data.frame(YEAR, LAY, HATCH, summerRAIN,cumRAIN)
  
  WEA.1 <- rbind(WEA.1, c)
}

WEA <- merge(WEA, WEA.1, all.x = TRUE)

#### Superposition of temperature and precipitation in time ####
# 
# png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 3 - Path analysis/FOX numerical response/ARTICLE Ph.D. 3/VERSION FINALE V1/Figures/prec_temp.tiff",
#     res=300,
#     width=20,
#     height=15,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")

#x11()
#par(oma=c(0,0,0,3)) # outer margin
par(mar=c(5,5,1,5)) # inner margin - default parameter is par("mar") <- 5.1 4.1 4.1 2.1

plot(WEA$YEAR,
               WEA$cumRAIN,
               xlab = "Year",
               ylab = "",
               xaxp = c(1996, 2016, 10),
               ylim = c(0, 150),
               bty = "n",
               yaxt = "n",
               xaxt = "n",
               cex = 1,
               cex.lab = 1,
               col = "darkblue",
               pch = 19,
              lwd = 2,
               type = 'b')

lines(WEA$YEAR,
      rep(mean(WEA$cumRAIN), 21),
      col = "darkblue",
      type = "l",
      lty = 4,
      lwd = 2)

axis(side = 4,
     lwd = 1,
     las = 2)
#mtext(side = 4,
     # line = 3,
     # "Rainfall (mm)",
     # las = 2)

par(new = T)

plot(WEA$YEAR,
              WEA$meanTEMP,
                    xlab = "",
                    ylab = "",
                    ylim = c(0, 7),
                    bty = "n",
                    yaxt = "n",
                    xaxt = "n",
                    cex = 1,
                    cex.lab = 1,
                    col = "chocolate",
                    pch = 17,
                    type = 'b',
                    lwd = 2)
lines(WEA$YEAR,
      rep(mean(WEA$meanTEMP), 21),
      col = "chocolate",
      type = "l",
      lty = 4,
      lwd = 2)

axis(side = 1,
     at = 1996:2016,
     lwd = 1)
axis(side = 2,
     lwd = 1,
     las = 2,
     at = 0:7)


#mtext(side = 2,
 #     line = 3,
  #    "Mean temperature (c)",
   #   las = 2)

dev.off()


#### GOOSE - LEMMING - FOX PLOT ####

lmg <- read.table("LEM_1993-2017.txt", sep = "\t", dec = ",", h = T)
lmg <- lmg[lmg$YEAR >= 1996 & !lmg$YEAR == 2017,]; head(lmg); summary(lmg)

fox <- read.table("FOX_abundance_Chevallier.txt", sep = "\t", dec = ",", h = T)
fox <- fox[fox$year >= 1996 & !fox$year == 2017,]; head(fox); summary(fox)

# png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 3 - Path analysis/FOX numerical response/ARTICLE Ph.D. 3/VERSION FINALE V2/Figures/fox_lmg_gee.tiff",
#     res=300,
#    width=20,
#   height=15,
#  pointsize=12,
#  unit="cm",
# bg="transparent")

#x11()

#par(oma=c(0,0,0,3)) # outer margin
par(mar=c(5,5,1,5)) # inner margin - default parameter is par("mar") <- 5.1 4.1 4.1 2.1

plot(lmg$YEAR,
     lmg$LMG_C1_CORR,
     xlab = "",
     ylab = "",
     xaxp = c(1996, 2016, 10),
     ylim = c(0, 12),
     bty = "n",
     yaxt = "n",
     xaxt = "n",
     cex = 1,
     cex.lab = 1,
     cex.axis = 1,
     col = "chartreuse3",
     type = 'h',
     lwd = 4)

axis(side = 2,
     lwd = 1, 
     las = 2,
     cex.axis = 1)
axis(side = 1,
     at = 1996:2016,
     lwd = 1,
     cex.axis = 1)

par(new = T)

plot(g$YEAR,
     g$NEST_SUCC,
     xlab = "",
     ylab = "",
     ylim = c(0, 1),
     bty = "n",
     yaxt = "n",
     xaxt = "n",
     cex = 1,
     cex.lab = 1,
     col = "darkolivegreen4",
     pch = 17,
     type = 'b',
     lwd = 2)
lines(fox$year,
      fox$prop_natal_dens/100,
      col = "dodgerblue4",
      pch = 19,
      type = 'b',
      lwd = 2)

axis(side = 4,
     lwd = 1,
     las = 2,
     cex.axis = 1)
#mtext(side = 4,
 #     line = 3,
  #    "Goose nesting succeess & fox breeding dens proportion")

dev.off()


#### FOX VS. LEMMING PLOT ####
# png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 3 - Path analysis/FOX numerical response/ARTICLE Ph.D. 3/VERSION FINALE V1/Figures/fox_vs_lmg.tiff",
#     res=300,
#     width=20,
#     height=15,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")

#x11()
par(mar=c(3,3,1,1)) # inner margin - default parameter is par("mar") <- 5.1 4.1 4.1 2.1

plot(lmg$LMG_C1_CORR,
     fox$prop_natal_dens,
     xlim = c(0, 10),
     ylim = c(0, 40),
     col = "dodgerblue4",
     bty = "n",
     pch = 16,
     type = "p",
     lwd = 2,
     cex.axis = 1,
     xlab = "",
     ylab = "",
     las = 2,
     xaxt = "n")

axis(side = 1,
     lwd = 1,
     cex.axis = 1)

lines(smooth.spline(lmg$LMG_C1_CORR,
                    fox$prop_natal_dens,
                    df = 3),
      col = "dodgerblue4",
      lwd = 2)
# Plot confident intervals

fit <- smooth.spline(lmg$LMG_C1_CORR,
                     fox$prop_natal_dens, df = 3)     # smooth.spline fit
res <- (fit$yin - fit$y)/(1-fit$lev)      # jackknife residuals
sigma <- sqrt(var(res))                     # estimate sd

upper <- fit$y + 2.0*sigma*sqrt(fit$lev)   # upper 95% conf. band
lower <- fit$y - 2.0*sigma*sqrt(fit$lev)   # lower 95% conf. band
par(new = T)
matplot(fit$x, cbind(upper, lower), type="l", lty = "dotdash", ylim = c(0, 40), xaxt = "n", yaxt = "n", ylab = "", xlab = "", bty = "n", col = "dodgerblue4")


#lines(smooth.spline(lmg$LMG_C1_CORR,
#                   fox$prop_natal_dens,
#                  df = 2),
#   col = "darkgoldenrod3",
#  lwd = 3)

#legend(0,
#      40,
#     legend = c("df = 3", "df = 2"),
#    col = c("dodgerblue4", "darkgoldenrod3"),
#   pch = "-",
#  lwd = 3,
# bty = "n")

dev.off()

# OR ELSE

#rm(list = ls()) #clean R memory
setwd(dir = "C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
mC1 <- read.table("mC1_path_data.txt", h = T)

#Package nécessaires
require(nlme)
require(lme4)
require(piecewiseSEM)
require(ggm)

g <- glm(prop_fox_dens ~ lmg_C1_CORR, weights = monit_dens, data = mC1, family = binomial)
summary(g)

# plot predicted values on raw data
range(mC1$lmg_C1_CORR)
# For creation new dataframe for lmg values simulation
v <- seq(0, 10, by = 0.1)
p <- predict(g, newdata = data.frame(lmg_C1_CORR = v), type = "response", se.fit = TRUE)

plot(mC1$lmg_C1_CORR, mC1$prop_fox_dens)
lines(p$fit, type = "l", col = "green")

#
g2 <- glm(cbind(breed_dens, monit_dens - breed_dens) ~ lmg_C1_CORR, data = mC1, family = binomial)
summary(g2)


p2 <- predict(g2, newdata = data.frame(lmg_C1_CORR = v), type = "response", se.fit = TRUE)

par(las = 1)
# 
# png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 3 - Path analysis/FOX numerical response/ARTICLE Ph.D. 3/VERSION FINALE V1/Figures/fox_vs_lmg_v3.tiff",
#     res=300,
#     width=20,
#     height=15,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")

plot(mC1$lmg_C1_CORR, mC1$prop_fox_dens,
     xlim = c(0, 10),
     ylim = c(0, 0.40),
     col = "dodgerblue4",
     bty = "n",
     pch = 16,
     type = "p",
     lwd = 2,
     cex.axis = 1,
     xlab = "",
     ylab = "")
lines(v, p2$fit,
      col = "dodgerblue4",
      lwd = 1)
lines(v, (p2$fit - 1.96 * p2$se.fit), type = "l", col = "dodgerblue4", lty = "dashed")
lines(v, (p2$fit + 1.96 * p2$se.fit), type = "l", col = "dodgerblue4", lty = "dashed")
dev.off()

#
g3 <- glm(cbind(breed_dens, monit_dens - breed_dens) ~ lmg_C1_CORR + winAO + cumul_prec + MEAN_temp, data = mC1, family = binomial)
summary(g3)

plot(mC1$AN, mC1$prop_fox_dens)
lines(mC1$AN, mC1$prop_fox_dens)

lines(predict(g3, type = "response"), col = "red", lty = "dashed")

# Log of lemming abundance
#g4 <- glm(cbind(breed_dens, monit_dens - breed_dens) ~ log(lmg_C1_CORR) + winAO + cumul_prec + MEAN_temp, data = mC1, family = binomial)
g4 <- glm(cbind(breed_dens, monit_dens - breed_dens) ~ log(lmg_C1_CORR), data = mC1, family = binomial)
summary(g4)

Eg4 <- resid(g4, type = "pearson")
sum(Eg4^2) / (g4$df.residual)

# plot predicted values on raw data
range(log(mC1$lmg_C1_CORR))
# For creation new dataframe for lmg values simulation
xv <- seq(-4, 2.30, by = 0.1)
yv <- predict(g4, list(lmg_C1_CORR = exp(xv)), type = "response", se.fit = TRUE)
p <- mC1$breed_dens/(mC1$monit_dens)

plot(p ~ log(mC1$lmg_C1_CORR), ylab = "Proportion breeding dens")
lines(yv$fit ~ xv, col = "red")
lines((yv$fit - 1.96 * yv$se.fit) ~ xv, type = "l", col = "dodgerblue4", lty = "dashed")
lines((yv$fit + 1.96 * yv$se.fit) ~ xv, type = "l", col = "dodgerblue4", lty = "dashed")

# Log of lemming abundance without 2000
mC1bis <- mC1[!(mC1$AN == 2000),]
g5 <- glm(cbind(breed_dens, monit_dens - breed_dens) ~ log(lmg_C1_CORR) + winAO + cumul_prec + MEAN_temp, data = mC1bis, family = binomial)

summary(g5)

Eg5 <- resid(g5, type = "pearson")
sum(Eg5^2) / (g5$df.residual) # Overdisp. = 1.13 for the complete modele

# plot predicted values on raw data
range(log(mC1$lmg_C1_CORR))
# For creation new dataframe for lmg values simulation
xv <- seq(-4, 2.30, by = 0.1)
yv <- predict(g5, list(lmg_C1_CORR = exp(xv), winAO = rep(g5$coefficients[3], 64), cumul_prec = rep(g5$coefficients[4], 64), MEAN_temp = rep(g5$coefficients[5], 64)), type = "response", se.fit = TRUE)
utils::View(yv)
p <- mC1$breed_dens/(mC1$monit_dens)

plot(p ~ log(mC1$lmg_C1_CORR), ylab = "Proportion breeding dens")
lines(yv$fit ~ xv, col = "red")
lines((yv$fit - 1.96 * yv$se.fit) ~ xv, type = "l", col = "dodgerblue4", lty = "dashed")
lines((yv$fit + 1.96 * yv$se.fit) ~ xv, type = "l", col = "dodgerblue4", lty = "dashed")

#### FOX VS. LEMMING PLOT WITHOUT 2000 ####
# png("fox_vs_lem_WITHOUT_2000.tiff",
#     res=300,
#     width=20,
#     height=15,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")

#x11()

plot(lmg$LMG_C1_CORR[!lmg$YEAR == 2000],
     fox$prop_natal_dens[!fox$year == 2000],
     xlim = c(0, 10),
     ylim = c(0, 40),
     col = "dodgerblue4",
     bty = "n",
     pch = 16,
     #type = "p",
     lwd = 3,
     xlab = "Lemming abundance",
     ylab = "Proportion of fox breeding dens")

lines(smooth.spline(lmg$LMG_C1_CORR[!lmg$YEAR == 2000],
                    fox$prop_natal_dens[!fox$year == 2000],
                    df = 3),
      col = "dodgerblue4",
      lwd = 3)
dev.off()

#### FOX vs. LEMMING PLOT - LAST PLOT ####
m <- glm(prop_fox_dens ~ I(log(lmg_C1_CORR)) + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = mC1, family = binomial(link="logit"))
plot(mC1$lmg_C1_CORR,
     mC1$prop_fox_dens,
     xlim = c(0, 10),
     ylim = c(0, 0.5),
     bty = "n",
     las = 1,
     col = "dodgerblue4",
     pch = 16,
     #type = "p",
     lwd = 3,
     xlab = "Lemming abundance",
     ylab = "Proportion of fox breeding dens")

v <- seq(0, 10, by = 0.01)

newdat <- data.frame(lmg_C1_CORR = v, cumul_prec = mean(mC1$cumul_prec), MEAN_temp = mean(mC1$MEAN_temp), winAO = mean(mC1$winAO))

p <- predict(m, newdata = newdat, type = "response", se.fit = TRUE)
lines(v,
      p$fit,
      col = "dodgerblue4",
      lwd = 2)
lines(v,
      p$fit + 1.96*p$se.fit,
      col = "dodgerblue4",
      lwd = 1.5,
      lty = "dashed")
lines(v,
      p$fit - 1.96*p$se.fit,
      col = "dodgerblue4",
      lwd = 1.5,
      lty = "dashed")
#### SAME PLOT BUT WITH SIMPLIER MODEL ####
prop_fox_dens <- as.numeric(tapply(mC1$prop_fox_dens, mC1$AN, unique))
monit_dens <- as.numeric(tapply(mC1$monit_dens, mC1$AN, unique))
lmg_C1_CORR <- as.numeric(tapply(mC1$lmg_C1_CORR, mC1$AN, unique))
winAO <- as.numeric(tapply(mC1$winAO, mC1$AN, mean))
cumul_prec <- as.numeric(tapply(mC1$cumul_prec, mC1$AN, mean))
MEAN_temp <- as.numeric(tapply(mC1$MEAN_temp, mC1$AN, mean))

oth <- as.data.frame(cbind(YEAR = 1996:2016, prop_fox_dens, monit_dens, lmg_C1_CORR, winAO, cumul_prec, MEAN_temp))
summary(oth)

m <- glm(prop_fox_dens ~ I(log(lmg_C1_CORR)) + cumul_prec + MEAN_temp + winAO, weights = monit_dens, data = oth, family = binomial(link="logit"))

# png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 3 - Path analysis/FOX numerical response/ARTICLE Ph.D. 3/VERSION FINALE V1/Figures/fox_vs_lem_LAST_PLOT_simplier model.tiff",
#     res=300,
#     width=20,
#     height=15,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")

plot(oth$lmg_C1_CORR,
     oth$prop_fox_dens,
     xlim = c(0, 10),
     ylim = c(0, 0.5),
     bty = "n",
     las = 1,
     col = "dodgerblue4",
     pch = 16,
     #type = "p",
     lwd = 3,
     xlab = "Lemming abundance",
     ylab = "Proportion of fox breeding dens")

v <- seq(0, 10, by = 0.01)

newdat <- data.frame(lmg_C1_CORR = v, cumul_prec = mean(oth$cumul_prec), MEAN_temp = mean(oth$MEAN_temp), winAO = mean(oth$winAO))

p <- predict(m, newdata = newdat, type = "response", se.fit = TRUE)
lines(v,
      p$fit,
      col = "dodgerblue4",
      lwd = 2)
lines(v,
      p$fit + 1.96*p$se.fit,
      col = "dodgerblue4",
      lwd = 1.5,
      lty = "dashed")
lines(v,
      p$fit - 1.96*p$se.fit,
      col = "dodgerblue4",
      lwd = 1.5,
      lty = "dashed")
dev.off()
#### Plot SN vs. prec ####

setwd(dir = "C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")

#rm(list = ls()) #clean R memory
f <- read.table("Path analysis_data 3bis.txt", sep = ",", dec = ".", h = T)


# png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 3 - Path analysis/FOX numerical response/ARTICLE Ph.D. 3/VERSION FINALE V1/Figures/goose vs prec_temp.tiff",
#     res=300,
#     width=25,
#     height=15,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")

par(mfrow = c(1, 2))
# SN vs. prec
k3 <- glm(SN ~ cumul_prec, data = f, family = binomial(link = "cloglog"))
summary(k3)
range(f$cumul_prec)
xprec <- seq(0, 69, 0.01)
ySN <- predict(k3, list(cumul_prec = xprec), type = 'response')
# Plot values
require(scales) # For the transparency of points - alpha()

par(mar = c(5.1, 4.1, 5, 0.1))
plot(f$cumul_prec, f$SN, pch = 16, xlab = '', ylab = '', ylim = c(0, 1), bty = 'n', col = alpha('olivedrab', 0.4), yaxt = 'n', xaxt = 'n')
lines(xprec, ySN, col = 'olivedrab', lwd = 2)
axis(side = 1, lwd = 1)
axis(side = 2, lwd = 1)

#legend(65, 1.06, "(a)", bty = "n")


# SN vs. temp
k2 <- glm(SN ~ MEAN_temp, data = f, family = binomial(link = "logit"))
summary(k2)
range(f$MEAN_temp)
xtemp <- seq(-0.85, 8.98, 0.01)
ySN <- predict(k2, list(MEAN_temp = xtemp), type = 'response')
# Plot values
require(scales) # For the transparency of points
par(mar = c(5.1, 0, 5, 2.1))
plot(f$MEAN_temp, f$SN, pch = 16, xlab = '', ylab = '', ylim = c(0, 1), bty = 'n', col = alpha('olivedrab', 0.4), yaxt = 'n', xaxt = 'n', xlim = c(-1, 9))
lines(xtemp, ySN, col = 'olivedrab', lwd = 2)
axis(side = 1, lwd = 1, xaxp = c(-1, 9, 10))
#axis(side = 2, lwd = 1)

#legend(8.5, 1.3, "(b)", bty = "n")

# dev.off()

#### Plot prop of success vs. cumul prec - LAST VERSION ####
# Same method used with lmg vs. fox plot and based on Gilles comments
require(lme4)
require(scales)
m <- glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = f, family = binomial(link = "logit"))
summary(m)

plot(f$cumul_prec, f$SN, pch = 16, xlab = '', ylab = '', ylim = c(0, 1), bty = 'n', col = alpha('olivedrab', 0.4), yaxt = 'n', xaxt = 'n')

v1 <- seq(0, 70, by = 0.01)

newdat <- data.frame(cumul_prec = v1, prop_fox_dens = mean(f$prop_fox_dens), MEAN_temp = mean(f$MEAN_temp))
p1 <- predict(m, newdata = newdat, type = "response", re.form = NA) # se.fit doesn't work with glmer

#plot(f$cumul_prec, jitter(f$SN)) # jitter() allows to see the variability of points
plot(v1, p1, ylim = c(0, 1), type = "l", bty = "n")

# Delimitation of categories to plot transformed raw data
f$rain_CAT <- cut(f$cumul_prec, breaks = seq(-5, 70, 5))# Creation of precipitation categorical variable to plot raw data

rain.DF <- split(f, f$rain_CAT) # Split dataframe into a list, based on the rainfall categorical variable rain.DF levels

PROP1 <- NULL
for (i in 1:15){
  
  succ <- sum(rain.DF[[i]]$SN)
  tot <- dim(rain.DF[[i]])[1]
  
  prop <- succ/tot
  # print(succ)
  # print(tot)
  print(prop)
  c <- c(succ, tot, prop)
  PROP1 <- as.data.frame(rbind(PROP1, c))
}
PROP1


#### Plot prop of success vs. mean temp - LAST VERSION ####
# Same method used with lmg vs. fox plot and based on Gilles comments

require(lme4)
m <- glmer(SN ~ prop_fox_dens + cumul_prec + MEAN_temp + (1|AN), data = f, family = binomial(link = "logit"))
summary(m)

summary(f$MEAN_temp)
v <- seq(-0.5, 9, by = 0.01)

newdat <- data.frame(MEAN_temp = v, prop_fox_dens = mean(f$prop_fox_dens), cumul_prec = mean(f$cumul_prec))
p <- predict(m, newdata = newdat, type = "response", re.form = NA) # se.fit doesn't work with glmer

#plot(f$cumul_prec, jitter(f$SN)) # jitter() allows to see the variability of points
plot(v, p, ylim = c(0, 1), type = "l", bty = "n")

# Delimitation of categories to plot transformed raw data
f$temp_CAT <- cut(f$MEAN_temp, breaks = seq(-1, 9, 0.1)) 

temp.DF <- split(f, f$temp_CAT) # Split dataframe into a list, based on the rainfall categorical variable rain.DF levels

PROP <- NULL
for (i in 1:100){
  
  succ <- sum(temp.DF[[i]]$SN)
  tot <- dim(temp.DF[[i]])[1]
  
  prop <- succ/tot
  # print(succ)
  # print(tot)
  print(prop)
  c <- c(succ, tot, prop)
  PROP <- as.data.frame(rbind(PROP, c))
}
PROP <- cbind(PROP, levels(f$temp_CAT))
PROP


# png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 3 - Path analysis/FOX numerical response/ARTICLE Ph.D. 3/VERSION FINALE V2/Figures/goose vs temp&prec.tiff",
#     res=300,
#     width=25,
#     height=15,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")

par(mfrow = c(1, 2))
par(mar = c(5.1, 4.1, 5, 0.1))
plot(seq(0, 70, 5), PROP1$V3,
     ylim = c(0, 1),
     pch = 16,
     xlab = '',
     ylab = '',
     bty = 'n',
     col = "olivedrab",
     yaxt = 'n',
     xaxt = 'n')
lines(v1, p1, col = 'olivedrab', lwd = 2)
axis(side = 1, lwd = 1)
axis(side = 2, lwd = 1, las = 1)

par(mar = c(5.1, 0, 5, 2.1))
plot(seq(-0.9, 9, 0.1), PROP$V3,
     ylim = c(0, 1),
     xlim = c(-1, 9),
     pch = 16,
     xlab = '',
     ylab = '',
     bty = 'n',
     col = "olivedrab",
     yaxt = 'n',
     xaxt = 'n')
lines(v, p, col = 'olivedrab', lwd = 2, xlim = c(-0.5, 9))
axis(side = 1, at = -1:9, lwd = 1)
dev.off()

#### Autocorrelation tests ####
  # For AO
AO <- read.csv("AO_saisonnier.txt", sep = ",", dec = ".")
AO.ts <- ts(AO[-length(AO$YEAR),c(3, 5, 11)], start = 1950, frequency = 1)
summary(AO.ts)

AO.ts.2 <- ts(AO[AO$YEAR >= 1989 & AO$YEAR < 2017, c(3, 5, 11)], start = 1989, frequency = 1)
summary(AO.ts.2)

# From 1950 to 2016
data <- AO.ts
ax <- 1950:2016
n <- 68
lag <- 1:n

# From 1989 to 2016
data <- AO.ts.2
ax <- 1989:2016
n <- ((2016-1989)/2)
lag <- 1:n

x11()
layout(matrix(c(1,2,3,4, 5, 6), 3, 2, byrow = FALSE))
par(mar=c(1, 4.1, 4.1, 2.1))
plot(data[,1], bty = "n", main = "", xaxt = "n", ylab = "winAO", xlab = "", type = "b")
par(mar=c(1, 4.1, 1.5, 2.1))
plot(data[,2], bty = "n", main = "", xaxt = "n", ylab = "sprAO", xlab = "", type = "b")
par(mar=c(5.1, 4.1, 1.5, 2.1))
plot(data[,3], bty = "n", main = "", xaxt = "n", ylab = "sumAO", xlab = "Time", type = "b")
axis(1, ax)

#apply(data, MARGIN = 2, acf, main = "", bty = "n") # No temporal autocorrelation

par(mar=c(1, 4.1, 4.1, 2.1))
acf(data[,1], lag.max = n, bty = "n", main = "", xaxt = "n", ylab = "winAO ACF", xlab = "")
par(mar=c(1, 4.1, 1.5, 2.1))
acf(data[,2], lag.max = n, bty = "n", main = "", xaxt = "n", ylab = "sprAO ACF", xlab = "")
par(mar=c(5.1, 4.1, 1.5, 2.1))
acf(data[,3], lag.max = n, bty = "n", main = "", xaxt = "n", ylab = "sumAO ACF", xlab = "Lag")
axis(1, lag)


  # Rain & temperature between annual initiation and hatching date - WEA dataframe
head(WEA)

WEA.ts <- ts(WEA[, c(4,8)], start = 1989, frequency = 1)
head(WEA.ts)

x11()
#ts.plot(WEA.ts)
plot(WEA.ts, bty = "n", type = "b")

#apply(WEA.ts, MARGIN = 2, acf)
x11()
layout(matrix(c(1,2,3,4), 2, 2, byrow = FALSE))
par(mar=c(1, 4.1, 4.1, 2.1))
plot(WEA.ts[,1], xaxt = "n", xlab = "", bty = "n", type = "b", ylab = "mean temperature (C)")
par(mar=c(5.1, 4.1, 1.5, 2.1))
plot(WEA.ts[,2], xaxt = "n", xlab = "Time", bty = "n", type = "b", ylab = "Cumulative precipitation (mm)")
axis(1, 1989:2016)
# --- #
par(mar=c(1, 4.1, 4.1, 2.1))
acf(WEA.ts[,1], xlab = "", bty = "n",lag.max = 27, ylab = "Mean temp. ACF", main = "", xaxt = "n")
axis(1, 1:27)
par(mar=c(5.1, 4.1, 1.5, 2.1))
rain.ts <- na.omit(WEA.ts[,2])
acf(rain.ts, xlab = "Lag", bty = "n", lag.max = 21, ylab = "Cum. prec. ACF", xaxt = "n")
axis(1, 1:21)

  # Lemming abundance
lmg <- read.table("LEM_1993-2017.txt", sep = "\t", dec = ",", h = T)
head(lmg); summary(lmg)
lmg.ts <- ts(lmg$LMG_C1_CORR, start = 1993, frequency = 1)
plot(lmg.ts, type = "b")
acf(lmg.ts, na.action = na.pass)

  # Goose nesting success
head(g)
acf(g$NEST_SUCC)

  # Fox breeding proportion
fox <- read.table("FOX_abundance_Chevallier.txt", sep = "\t", dec = ",", h = T)
head(fox); summary(fox)
names(fox)[1] <- "YEAR"
acf(fox$prop_natal_dens)

zoo <- merge(g[,c(1, 6)], fox[,c(1, 4)], all.x = TRUE )
zoo <- merge(zoo, lmg[, c(1, 6)], all.x = TRUE)
zoo.ts <- ts(zoo[,-1], start = 1989, frequency = 1)
head(zoo.ts)

x11()
layout(matrix(c(1,2,3,4, 5, 6), 3, 2, byrow = FALSE))
par(mar=c(1, 4.1, 4.1, 2.1))
plot(zoo.ts[,1], bty = "n", main = "", xaxt = "n", ylab = "Goose nesting success prop.", xlab = "", type = "b")
par(mar=c(1, 4.1, 1.5, 2.1))
plot(zoo.ts[,2], bty = "n", main = "", xaxt = "n", ylab = "Fox breed. dens prop.", xlab = "", type = "b")
par(mar=c(5.1, 4.1, 1.5, 2.1))
plot(zoo.ts[,3], bty = "n", main = "", xaxt = "n", ylab = "Lemming abun.", xlab = "Time", type = "b")
axis(1, 1989:2016)
# --- #
par(mar=c(1, 4.1, 4.1, 2.1))
acf(zoo.ts[,1], lag.max = 27, bty = "n", main = "", ylab = "Goose ACF", xlab = "", xaxt = "n")
axis(1, 1:27)
par(mar=c(1, 4.1, 1.5, 2.1))
acf(zoo.ts[,2], lag.max = 22, bty = "n", main = "", ylab = "Fox ACF", xlab = "", na.action = na.pass, xaxt = "n")
axis(1, 1:22)
par(mar=c(5.1, 4.1, 1.5, 2.1))
acf(zoo.ts[,3], lag.max = 20, bty = "n", main = "", ylab = "Lemming ACF", xlab = "Lag", na.action = na.pass, xaxt = "n")
axis(1, 1:20)