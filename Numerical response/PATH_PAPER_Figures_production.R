rm(list = ls()) #clean R memory

setwd(dir = "C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
list.files()

#### WINTER, SPRING & SUMMER AO ####
  # Winter AO = November to April
  # Spring AO = 20 MAy - 20 June
  # Summer AO =

AO<-read.csv("AO_saisonnier.txt", sep = ",", dec = ".")
head(AO)

png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 3 - Path analysis/FOX numerical response/ARTICLE Ph.D. 3/VERSION FINALE V1/Figures/AO_seasons.tiff",
    res=300,
    width=15,
    height= 25,
    pointsize=12,
    unit="cm",
    bg="transparent")
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

g <- read.table("GOOSE_breeding_informations.txt", sep = "\t", dec = ",", h = T)
g <- g[g$YEAR >= 1996 & !g$YEAR == 2017,]; head(g); summary(g)

t <- read.table("TEMP_Tair moy 1989-2017 BYLCAMP.txt", sep = "\t", dec = ",", h = T)
t <- t[t$YEAR >= 1996 & !t$YEAR == 2017,]; head(t); summary(t)

AO <- read.table("AO_daily.txt", h = T, sep = "\t", dec = ".")
AO <- AO[AO$YEAR >= 1996 & !AO$YEAR == 2017,]; head(AO); summary(AO)

rain <- read.table("PREC_precipitation_Bylot_1995-2016.txt", h = T, sep = "\t", dec = ",")
rain <- rain[rain$YEAR >= 1996 & !rain$YEAR == 2017,]; head(rain); summary(rain)
rain <- na.omit(rain)


#### Traitement des jours juliens ####

#### Obtention des jours juliens pour les dates GOOSE
Sys.setlocale(category = "LC_TIME", locale = "English") #en_US.utf8 pour linux, en_US pour macOS, English pour Windows 
#setting in english to read month
g$lay_date_jj <- paste(g$LAY_DATE, g$YEAR, sep = " ")
g$lay_date_jj <- strptime(g$lay_date_jj, format = "%d %B %Y")
g$lay_date_jj <- g$lay_date_jj$yday +1

g$hatch_date_jj <- paste(g$HATCH_DATE, g$YEAR, sep = " ")
g$hatch_date_jj <- strptime(g$hatch_date_jj, format = "%d %B %Y")
g$hatch_date_jj <- g$hatch_date_jj$yday +1

#### Obtention des jours juliens pour les dates TEMP
head(t)
t$jj <- strptime(paste(t$DAY, t$MONTH, t$YEAR, sep = "-"), format = "%d-%m-%Y")
t$jj <- t$jj$yday + 1

# Exemple de Vérification de la valeur des JJ pour les années bissextiles (366 jours - 1996, 2000, 2004, 2008, 2012, 2016) et non bissextiles
#NONbiss <- g[!(g$YEAR == 1996 | g$YEAR == 2000 | g$YEAR == 2004 | g$YEAR == 2008 | g$YEAR == 2012 | g$YEAR == 2016),]
#biss <- g[g$YEAR == 1996 | g$YEAR == 2000 | g$YEAR == 2004 | g$YEAR == 2008 | g$YEAR == 2012 | g$YEAR == 2016,]
#plot(biss$LAY_DATE, biss$lay_date_jj)
#plot(biss$HATCH_DATE, biss$hatch_date_jj)

#### Temperatures trends ####
#### Relations entre les temperatures et la periode de nidification de oies ####
x11(title = "Nidification temperature trends between 1996 & 2016 ")

#dev.off()
par(mfrow = c(3, 7))
for (i in 1996:2016) {
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
x11(title = "Nidification rainfall trends between 1996 & 2016 ")

#dev.off()
par(mfrow = c(3, 7))
for (i in 1996:2016) {
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
  summerRAIN <- sum(rain$RAIN[rain$YEAR == i])
  cumRAIN <- sum(rain$RAIN[rain$YEAR == i & rain$JJ <= HATCH & rain$JJ >= LAY])
  meanTEMP <- mean(t$TEMP[t$YEAR == i & t$jj <= HATCH & t$jj >= LAY])
  sdTEMP <- sd(t$TEMP[t$YEAR == i & t$jj <= HATCH & t$jj >= LAY])
  
  c <- data.frame(YEAR, LAY, HATCH, summerRAIN, cumRAIN, meanTEMP, sdTEMP)
  
  WEA <- rbind(WEA, c)
}

summary(WEA)
#### Superposition of temperature and precipitation in time ####

png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 3 - Path analysis/FOX numerical response/ARTICLE Ph.D. 3/VERSION FINALE V1/Figures/prec_temp.tiff",
    res=300,
    width=20,
    height=15,
    pointsize=12,
    unit="cm",
    bg="transparent")

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

png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 3 - Path analysis/FOX numerical response/ARTICLE Ph.D. 3/VERSION FINALE V1/Figures/fox_lmg_gee.tiff",
    res=300,
   width=20,
  height=15,
 pointsize=12,
 unit="cm",
bg="transparent")

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
     g$NEST_SUCC*100,
     xlab = "",
     ylab = "",
     ylim = c(0, 100),
     bty = "n",
     yaxt = "n",
     xaxt = "n",
     cex = 1,
     cex.lab = 1,
     col = "darkolivegreen4",
     pch = 19,
     type = 'b',
     lwd = 2)
lines(fox$year,
      fox$prop_natal_dens,
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
png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 3 - Path analysis/FOX numerical response/ARTICLE Ph.D. 3/VERSION FINALE V1/Figures/fox_vs_lmg.tiff",
    res=300,
    width=20,
    height=15,
    pointsize=12,
    unit="cm",
    bg="transparent")

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

#### FOX VS. LEMMING PLOT WITHOUT 2000 ####
png("fox_vs_lem_WITHOUT_2000.tiff",
    res=300,
    width=10,
    height=20,
    pointsize=12,
    unit="cm",
    bg="transparent")

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

