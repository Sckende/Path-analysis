rm(list = ls()) #clean R memory

setwd(dir = "/Users/nicolas/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")

list.files()
g <- read.table("GOOSE_breeding_informations.txt", sep = "\t", dec = ",", h = T)
head(g)
t <- read.table("TEMP_Tair moy 1989-2016 BYLCAMP.txt", sep = "\t", dec = ",", h = T)
head(t)

#### Obtention des jours juliens pour les dates GOOSE ####
Sys.setlocale(category = "LC_TIME", locale = "en_US") #setting in english to read month
g$lay_date_jj <- paste(g$LAY_DATE, g$YEAR, sep = " ")
g$lay_date_jj <- strptime(g$lay_date_jj, format = "%d %B %Y")
g$lay_date_jj <- g$lay_date_jj$yday
g$lay_date_jj[g$YEAR == 1996 | g$YEAR == 2000 | g$YEAR == 2004 | g$YEAR == 2008 | g$YEAR == 2012 | g$YEAR == 2016] <- g$lay_date_jj[g$YEAR == 1996 | g$YEAR == 2000 | g$YEAR == 2004 | g$YEAR == 2008 | g$YEAR == 2012 | g$YEAR == 2016] + 1

g$hatch_date_jj <- paste(g$HATCH_DATE, g$YEAR, sep = " ")
g$hatch_date_jj <- strptime(g$hatch_date_jj, format = "%d %B %Y")
g$hatch_date_jj <- g$hatch_date_jj$yday
g$hatch_date_jj[g$YEAR == 1996 | g$YEAR == 2000 | g$YEAR == 2004 | g$YEAR == 2008 | g$YEAR == 2012 | g$YEAR == 2016] <- g$hatch_date_jj[g$YEAR == 1996 | g$YEAR == 2000 | g$YEAR == 2004 | g$YEAR == 2008 | g$YEAR == 2012 | g$YEAR == 2016] + 1

#Verfication des dates avec annees bissextiles (1996, 2000, 2004, 2008, 2012, 2016) et non
NONbiss <- g[!(g$YEAR == 1996 | g$YEAR == 2000 | g$YEAR == 2004 | g$YEAR == 2008 | g$YEAR == 2012 | g$YEAR == 2016),]
biss <- g[g$YEAR == 1996 | g$YEAR == 2000 | g$YEAR == 2004 | g$YEAR == 2008 | g$YEAR == 2012 | g$YEAR == 2016,]
plot(NONbiss$LAY_DATE, NONbiss$lay_date_jj)
plot(NONbiss$HATCH_DATE, NONbiss$hatch_date_jj)

#### Obtention des jours juliens pour les dates TEMP ####
t <- t[t$YEAR >= 1996,]
head(t)
t$jj <- strptime(paste(t$DAY, t$MONTH, t$YEAR, sep = "-"), format = "%d-%m-%Y")
t$jj <- t$jj$yday + 1 #comme dates continues pas besoin de traiter separemment annees bissextiles et annees ordinaires
#### Relations entre les temperatures et la periode de nidification de oies ####
x11(title = "Nidification temperature trends between 1996 & 2015 ")
par(mfrow = c(4, 5))
for (i in 1996:2015) {
  plot(t$jj[t$jj >= g$lay_date_jj[g$YEAR == i] & t$jj <= g$hatch_date_jj[g$YEAR == i] & t$YEAR == i], t$TEMP[t$jj >= g$lay_date_jj[g$YEAR == i] & t$jj <= g$hatch_date_jj[g$YEAR == i] & t$YEAR == i], main = i, xlab = "Julian day", ylab = "temp", ylim = c(0, 12), xlim = c(160, 195))
  ajout <- with(t, smooth.spline(t$jj[t$jj >= g$lay_date_jj[g$YEAR == i] & t$jj <= g$hatch_date_jj[g$YEAR == i] & t$YEAR == i], t$TEMP[t$jj >= g$lay_date_jj[g$YEAR == i] & t$jj <= g$hatch_date_jj[g$YEAR == i] & t$YEAR == i], df = 2))
  ajout
  lines(ajout, col = "blue")
}
#dev.copy2pdf("Nidifi_temp_trends_1996-2015.pdf")
dev.off()

#### Relations entre les temperatures et les differentes periode de AO ####

AO <- read.table("AO_daily.txt", h = T, sep = "\t", dec = ".")
head(AO)
AO <- AO[AO$YEAR >= 1996,]
AO$jj <- strptime(paste(AO$DAY, AO$MONTH, AO$YEAR, sep = "-"), format = "%d-%m-%Y")
AO$jj <- AO$jj$yday + 1
View(AO[AO$MONTH == 12,]) # to check bissextile years

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

#temp vs ao
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

dev.off()
#### Brouillon ####
x11()
plot(t$jj[t$jj >= g$lay_date_jj[g$YEAR == 2000] & t$jj <= g$hatch_date_jj[g$YEAR == 2000] & t$YEAR == 2000], t$TEMP[t$jj >= g$lay_date_jj[g$YEAR == 2000] & t$jj <= g$hatch_date_jj[g$YEAR == 2000] & t$YEAR == 2000])
ajout <- with(t, smooth.spline(t$jj[t$jj >= g$lay_date_jj[g$YEAR == 2000] & t$jj <= g$hatch_date_jj[g$YEAR == 2000] & t$YEAR == 2000], t$TEMP[t$jj >= g$lay_date_jj[g$YEAR == 2000] & t$jj <= g$hatch_date_jj[g$YEAR == 2000] & t$YEAR == 2000], df = 3))
ajout
lines(ajout, col = "green")
