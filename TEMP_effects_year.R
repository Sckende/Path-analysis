rm(list = ls()) #clean R memory

setwd(dir = "/Users/nicolas/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")

list.files()
g <- read.table("GOOSE_breeding_informations.txt", sep = "\t", dec = ",", h = T)
head(g)
t <- read.table("TEMP_Tair moy 1989-2016 BYLCAMP.txt", sep = "\t", dec = ",", h = T)
head(t)

#### Obtention des jours juliens pour les dates GOOSE ####
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
dev.copy2pdf("Nidifi_temp_trends_1996-2015.pdf")
dev.off()


#### Brouillon ####
x11()
plot(t$jj[t$jj >= g$lay_date_jj[g$YEAR == 2000] & t$jj <= g$hatch_date_jj[g$YEAR == 2000] & t$YEAR == 2000], t$TEMP[t$jj >= g$lay_date_jj[g$YEAR == 2000] & t$jj <= g$hatch_date_jj[g$YEAR == 2000] & t$YEAR == 2000])
ajout <- with(t, smooth.spline(t$jj[t$jj >= g$lay_date_jj[g$YEAR == 2000] & t$jj <= g$hatch_date_jj[g$YEAR == 2000] & t$YEAR == 2000], t$TEMP[t$jj >= g$lay_date_jj[g$YEAR == 2000] & t$jj <= g$hatch_date_jj[g$YEAR == 2000] & t$YEAR == 2000], df = 3))
ajout
lines(ajout, col = "green")
