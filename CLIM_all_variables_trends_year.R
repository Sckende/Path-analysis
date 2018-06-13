rm(list = ls()) #clean R memory

setwd(dir = "C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
list.files()
g <- read.table("GOOSE_breeding_informations.txt", sep = "\t", dec = ",", h = T)
head(g)
t <- read.table("TEMP_Tair moy 1989-2017 BYLCAMP.txt", sep = "\t", dec = ",", h = T)
t <- t[t$YEAR >= 1996 & !t$YEAR == 2017,]; head(t)
AO <- read.table("AO_daily.txt", h = T, sep = "\t", dec = ".")
head(AO)
rain <- read.table("PREC_precipitation_Bylot_1995-2016.txt", h = T, sep = "\t", dec = ",")
rain <- rain[rain$YEAR >= 1996 & !rain$YEAR == 2017,]

#### Summer precipitation trends ####
head(rain)
rain <- na.omit(rain)
summary(rain)
h <- as.data.frame(tapply(rain$RAIN, rain$YEAR, sum))
h <- cbind(1996:2016, h); names(h) <- c("year", "rain")
head(h)
h$moy <- mean(h$rain)
#plot
tiff("Prec1.tiff", res=300, width=29, height=20, pointsize=12, unit="cm", bg="transparent")
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

#depuis 2008
h08 <- h[h$year >= 2008,]
plot(h08$year, h08$rain)
lines(smooth.spline(h08$year, h08$rain, df = 2))
lines(gam(h08$year, h08$rain))

#Utilisation de la commande gam
require(mgcv)
M3 <- gam(h$rain ~ s(h$year, fx = FALSE, k=6,bs = "cr"))#k pour knots, k=3 recommandé pour n<10
summary(M3);AIC(M3)
coef(M3)

col2rgb(col="blue")


par(new = T) # Superposition de plots
#par(bg=NA)#fond de graphique transparent
plot(h$year, h$rain, xlab = "year", ylab = "Cumulative precipitation",
  xlim = c(1996, 2016),ylim = c(0, 155),col="blue",lwd=4,cex.axis=1.8,pch=16,cex=2)#nuage de points
par(new=T)#pour superposition d'un nouveau graphique
par(bg=NA)
couleur<-rgb(0,0,0.5,0.2)#définition d'une couleur pour l'intervalle de confiance
plot.gam(M3,shade = T,shade.col =couleur,pers = T,xlab = "",yaxt="n",ylab="",xaxt="n",lwd=4)#plot de la smooth curve et de l'intervalle de confiance à 95% associée

#### Obtention des jours juliens pour les dates GOOSE ####
Sys.setlocale(category = "LC_TIME", locale = "English") #en_US.utf8 pour linux, en_US pour macOS 
#setting in english to read month
g$lay_date_jj <- paste(g$LAY_DATE, g$YEAR, sep = " ")
g$lay_date_jj <- strptime(g$lay_date_jj, format = "%d %B %Y")
g$lay_date_jj <- g$lay_date_jj$yday +1

g$hatch_date_jj <- paste(g$HATCH_DATE, g$YEAR, sep = " ")
g$hatch_date_jj <- strptime(g$hatch_date_jj, format = "%d %B %Y")
g$hatch_date_jj <- g$hatch_date_jj$yday +1


#Verfication des dates avec annees bissextiles (1996, 2000, 2004, 2008, 2012, 2016) et non
NONbiss <- g[!(g$YEAR == 1996 | g$YEAR == 2000 | g$YEAR == 2004 | g$YEAR == 2008 | g$YEAR == 2012 | g$YEAR == 2016),]
biss <- g[g$YEAR == 1996 | g$YEAR == 2000 | g$YEAR == 2004 | g$YEAR == 2008 | g$YEAR == 2012 | g$YEAR == 2016,]
plot(biss$LAY_DATE, biss$lay_date_jj)
plot(biss$HATCH_DATE, biss$hatch_date_jj)

#### Obtention des jours juliens pour les dates TEMP ####
head(t)
t$jj <- strptime(paste(t$DAY, t$MONTH, t$YEAR, sep = "-"), format = "%d-%m-%Y")
t$jj <- t$jj$yday + 1 #comme dates continues pas besoin de traiter separemment annees bissextiles et annees ordinaires

#### Temperatures trends 
#### Relations entre les temperatures et la periode de nidification de oies ####
x11(title = "Nidification temperature trends between 1996 & 2016 ")
#dev.off()
par(mfrow = c(4, 5))
for (i in 1997:2016) {
  plot(t$jj[t$jj >= g$lay_date_jj[g$YEAR == i] & t$jj <= g$hatch_date_jj[g$YEAR == i] & t$YEAR == i], t$TEMP[t$jj >= g$lay_date_jj[g$YEAR == i] & t$jj <= g$hatch_date_jj[g$YEAR == i] & t$YEAR == i], main = i, xlab = "Julian day", ylab = "temp", ylim = c(0, 12), xlim = c(160, 195))
  ajout <- with(t, smooth.spline(t$jj[t$jj >= g$lay_date_jj[g$YEAR == i] & t$jj <= g$hatch_date_jj[g$YEAR == i] & t$YEAR == i], t$TEMP[t$jj >= g$lay_date_jj[g$YEAR == i] & t$jj <= g$hatch_date_jj[g$YEAR == i] & t$YEAR == i], df = 2))
  ajout
  lines(ajout, col = "blue")
}
#dev.copy2pdf("Nidifi_temp_trends_1996-2015.pdf")
dev.off()

#### Trends of AO ####
#Obtention of DB
head(AO)
AO <- AO[AO$YEAR >= 1996,]
AO$jj <- strptime(paste(AO$DAY, AO$MONTH, AO$YEAR, sep = "-"), format = "%d-%m-%Y")
AO$jj <- AO$jj$yday + 1
#View(AO[AO$MONTH == 12,]) # to check bissextile years

# RAPPEL
#Winter AO = Novembre à avril (month 10 to 4) - Aanes et al 2002
#Spring AO = 20 May to 20 June (JJ 140 to JJ 171) - Dickey et al 2008
#early summer AO (esumAO) = 21 June to 15 July (JJ 172 to 196) - Dickey et al 2008
#late summer AO (lsumAO) = 16 July to 15 August (JJ 197 to 227) - Dickey et al 2008
#Summer AO (sumAO) = 21 June to 15 August (JJ 172 to 227)
# AO during nidification (AOnidif) = 4 June to 19 July (mean dates - JJ 155 to 200)
j<-unique(AO$YEAR)
TAB<-NULL
for (i in j){
  
  YEAR<-i

ewin<-AO$AO[AO$YEAR==i-1 & AO$MONTH>=10]
lwin<-AO$AO[AO$YEAR==i & AO$MONTH<=4]
win<-c(ewin, lwin)

winAO<-mean(win, na.rm = T)
k<-data.frame(YEAR,winAO)

TAB<-rbind(TAB, k)
}

print(TAB)



sprAO <- tapply(AO$AO[AO$jj >= 140 & AO$jj <= 171], AO$YEAR[AO$jj >= 140 & AO$jj <= 171], mean)
esumAO <- tapply(AO$AO[AO$jj >= 172 & AO$jj <= 196], AO$YEAR[AO$jj >= 172 & AO$jj <= 196], mean)
lsumAO <- tapply(AO$AO[AO$jj >= 197 & AO$jj <= 227], AO$YEAR[AO$jj >= 197 & AO$jj <= 227], mean)
sumAO <- tapply(AO$AO[AO$jj >= 172 & AO$jj <= 227], AO$YEAR[AO$jj >= 172 & AO$jj <= 227], mean)
nidAO <- tapply(AO$AO[AO$jj >= 155 & AO$jj <= 200], AO$YEAR[AO$jj >= 155 & AO$jj <= 200], mean)
nidTEMP <- tapply(t$TEMP[t$jj >= 155 & t$jj <= 200], t$YEAR[t$jj >= 155 & t$jj <= 200], mean)

p <- cbind(1996 : 2016, TAB[-22,2], as.data.frame(sprAO), esumAO, lsumAO, sumAO, nidAO, nidTEMP)
summary(p)
names(p)[1: 2] <- c("year", "winAO")
head(p)
p$motem <- mean(p$nidTEMP)


#### Plot of nidification temperatures trends ####
plot(p$year, p$nidTEMP, bty = "n", ltw = 3, xaxp = c(1996, 2016, 10), ylim = c(3, 5.5), cex = 2, pch = 19, cex.lab = 1, col = "orange", xlab = "Year", ylab = "Mean temperature (°c)")
lines(smooth.spline(p$year, p$nidTEMP, df = 2), lwd = 5, col = "orange")

tiff("Fig1.tiff", res=300, width=20, height=29, pointsize=12, unit="cm", bg="transparent")
plot(p$year, p$nidTEMP, bty = "n", ltw = 3, xaxp = c(1996, 2016, 10), ylim = c(3, 5.5), cex = 2, pch = 19, cex.lab = 1, col = "orange", xlab = "Year", ylab = "Mean summer temperature (°c)")
lines(smooth.spline(p$year, p$nidTEMP, df = 2), lwd = 3, col = "orange")
lines(p$year, p$motem, type = "l", lty = 4, col = "orange")
dev.off()



# Fonction pour impression coef correlation et p.value sur les pair.panels avec variables utilisees dans analyses de piste
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
x11()
pairs(p[, c(2 : 7)], upper.panel = panel.cor)
dev.copy2pdf()
dev.off()
#Trends of AO depending on the season
x11()
par(mfrow = c(2,2)) #Disvision of the graphic device

#Winter AO
png("winAO.png", res=300, width=10, height=10, pointsize=12, unit="cm", bg="transparent")
plot(p$year, p$winAO, xlab = "Year", ylab = "Winter AO index", ylim = c(-2, 1.5), xlim = c(1996, 2016), bty = "n", yaxt = "n", xaxt = "n", cex = 1, cex.lab = 1, col = "orange", pch = 19, type = 'b')
#lines(smooth.spline(p$year, p$winAO, df = 2), col = "orange", lwd = 2)
lines(p$year, p$mo, type = "l", lty = 4, col = "orange")
#Modification x axis
xtick <- seq(1996, 2016, by = 1)
axis(side = 1, at = xtick)
#Modification y axis
ytick<-seq(-2, 1.5, by = 0.5)
axis(side = 2, at = ytick)
p$winAOmean <- mean(p$winAO)
lines(p$year, p$winAOmean, lty = 4, col = 'orange')
dev.off()

jo <- lm(p$winAO ~ p$year)
summary(jo)

#Method 2
library(splines)
spline <- interpSpline(p$year, p$winAO)
plot(spline)
points(p$year, p$winAO)

#Spring AO
png("sprAO.png", res=300, width=10, height=10, pointsize=12, unit="cm", bg="transparent")
plot(p$year, p$sprAO, xlab = "Year", ylab = "Spring AO index", ylim = c(-2, 1.5), xlim = c(1996, 2016), bty = "n", yaxt = "n", xaxt = "n", cex = 1, cex.lab = 1, col = "orange", pch = 19, type = 'b')
#lines(smooth.spline(p$year, p$sprAO, df = 2), col = "orange", lwd = 2)
p$sprAOmoy <- mean(p$sprAO)
lines(p$year, p$sprAOmoy, type = "l", lty = 4, col = "orange")
#Modification x axis
xtick <- seq(1996, 2016, by = 1)
axis(side = 1, at = xtick)
#Modification y axis
ytick<-seq(-2, 1.5, by = 0.5)
axis(side = 2, at = ytick)
dev.off()

#Summer AO
png("sumAO.png", res=300, width=10, height=10, pointsize=12, unit="cm", bg="transparent")
plot(p$year, p$sumAO, xlab = "Year", ylab = "Summer AO index", ylim = c(-2, 1.5), xlim = c(1996, 2016), bty = "n", yaxt = "n", xaxt = "n", cex = 1, cex.lab = 1, col = "orange", pch = 19, type = 'b')
#lines(smooth.spline(p$year, p$sumAO, df = 2), col = "orange", lwd = 2)
p$sumAOmoy <- mean(p$sumAO)
lines(p$year, p$sumAOmoy, type = "l", lty = 4, col = "orange")
#Modification x axis
xtick <- seq(1996, 2016, by = 1)
axis(side = 1, at = xtick)
#Modification y axis
ytick<-seq(-2, 1.5, by = 0.5)
axis(side = 2, at = ytick)
dev.off()

#Nidification AO
plot(p$year, p$nidAO, xlab = "Year", ylab = "Nidification AO index", ylim = c(-2, 1.5), xlim = c(1996, 2016), bty = "n", yaxt = "n", xaxt = "n", cex = 1.5, cex.lab = 1.3, line = 2, col = "orange", pch = 19)
lines(smooth.spline(p$year, p$nidAO, df = 2), col = "orange", lwd = 3)
#Modification x axis
xtick <- seq(1996, 2016, by = 1)
axis(side = 1, at = xtick)
#Modification y axis
ytick<-seq(-2, 1.5, by = 0.5)
axis(side = 2, at = ytick)

#dev.copy2pdf()
dev.off()

#### Superposition of temperature and precipitation in time ####

png("prec_temp.tiff", res=300, width=10, height=15, pointsize=12, unit="cm", bg="transparent")
plot(h$year, h$rain, xlab = "Year", ylab = "", xaxp = c(1996, 2016, 10), ylim = c(0, 250), bty = "n", yaxt = "n", xaxt = "n", cex = 1, cex.lab = 1, col = "darkblue", pch = 19, type = 'b')
#lines(smooth.spline(h$year, h$rain, df = 2), col = "cadetblue", lwd = 2)

lines(h$year, h$moy, col = "darkblue", type = "l", lty = 4)
axis(side = 4, lwd = 1)
axis(side = 1, at = 1996:2016, lwd = 1)
par(bg = "transparent")
plot(p$year, p$nidTEMP, xlab = "", ylab = "", ylim = c(0, 5.5), bty = "n", yaxt = "n", xaxt = "n", cex = 1, cex.lab = 1, col = "chocolate", pch = 17, type = 'b')
axis(side = 2, lwd = 1, at = 0:5.5)
#lines(smooth.spline(p$year, p$nidTEMP, df = 2), col = "chocolate", lwd = 2)
lines(p$year, p$motem, col = "chocolate", lty = 4)

#mtext(c("Cumulative precipitation (mm)", "Mean temperature (°C)"), side = c(4, 2), line = 1)
dev.off()


jj <- lm(p$nidTEMP ~ p$year)
summary(jj)

jjj <- lm(h$rain ~ p$year)
summary(jjj)
### ATTENTION LA PERIODE DE 2016 PAS COMPLETE POUR LE CALCUL DES TEMP
x11()
par(mfrow = c(2, 3))
plot(p$nidTEMP, p$winAO, xlab = "Nidification temperature", ylab = "Winter AO")
lines(smooth.spline(p$nidTEMP, p$winAO, df = 2), col = "orange")

plot(p$nidTEMP, p$sprAO, xlab = "Nidification temperature", ylab = "Spring AO")
lines(smooth.spline(p$nidTEMP, p$sprAO, df = 2), col = "orange")

plot(p$nidTEMP, p$sumAO, xlab = "Nidification temperature", ylab = "Summer AO")
lines(smooth.spline(p$nidTEMP, p$sumAO, df = 2), col = "orange")

plot(p$nidTEMP, p$esumAO, xlab = "Nidification temperature", ylab = "Early summer AO")
lines(smooth.spline(p$nidTEMP, p$esumAO, df = 2), col = "orange")

plot(p$nidTEMP, p$lsumAO, xlab = "Nidification temperature", ylab = "Late summer AO")
lines(smooth.spline(p$nidTEMP, p$lsumAO, df = 2), col = "orange")

plot(p$nidTEMP, p$nidAO, xlab = "Nidification temperature", ylab = "Nidification AO")
lines(smooth.spline(p$nidTEMP, p$nidAO, df = 2), col = "orange")
#dev.copy2pdf()
dev.off()
### ATTENTION LA PERIODE DE 2016 PAS COMPLETE POUR LE CALCUL DES TEMP
x11()
plot(p$year, p$nidTEMP, xlab = "Year", ylab = "Mean summer temperatures")
lines(smooth.spline(p$year, p$nidTEMP, df = 4), col = "purple")
lines(smooth.spline(p$year, p$nidTEMP, df = 8), col = "blue")
#dev.copy2pdf()
dev.off()

#### Brouillon ####
x11()
plot(t$jj[t$jj >= g$lay_date_jj[g$YEAR == 2000] & t$jj <= g$hatch_date_jj[g$YEAR == 2000] & t$YEAR == 2000], t$TEMP[t$jj >= g$lay_date_jj[g$YEAR == 2000] & t$jj <= g$hatch_date_jj[g$YEAR == 2000] & t$YEAR == 2000])
ajout <- with(t, smooth.spline(t$jj[t$jj >= g$lay_date_jj[g$YEAR == 2000] & t$jj <= g$hatch_date_jj[g$YEAR == 2000] & t$YEAR == 2000], t$TEMP[t$jj >= g$lay_date_jj[g$YEAR == 2000] & t$jj <= g$hatch_date_jj[g$YEAR == 2000] & t$YEAR == 2000], df = 3))
ajout
lines(ajout, col = "green")
