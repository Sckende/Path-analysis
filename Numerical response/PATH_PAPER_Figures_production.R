rm(list = ls()) #clean R memory

setwd(dir = "C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
list.files()

g <- read.table("GOOSE_breeding_informations.txt", sep = "\t", dec = ",", h = T)
g <- g[g$YEAR >= 1996 & !g$YEAR == 2017,]; head(g); summary(g)

t <- read.table("TEMP_Tair moy 1989-2017 BYLCAMP.txt", sep = "\t", dec = ",", h = T)
t <- t[t$YEAR >= 1996 & !t$YEAR == 2017,]; head(t); summary(t)

AO <- read.table("AO_daily.txt", h = T, sep = "\t", dec = ".")
AO <- AO[AO$YEAR >= 1996 & !AO$YEAR == 2017,]; head(AO); summary(AO)

rain <- read.table("PREC_precipitation_Bylot_1996-2016.txt", h = T, sep = "\t", dec = ",")
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
NONbiss <- g[!(g$YEAR == 1996 | g$YEAR == 2000 | g$YEAR == 2004 | g$YEAR == 2008 | g$YEAR == 2012 | g$YEAR == 2016),]
biss <- g[g$YEAR == 1996 | g$YEAR == 2000 | g$YEAR == 2004 | g$YEAR == 2008 | g$YEAR == 2012 | g$YEAR == 2016,]
plot(biss$LAY_DATE, biss$lay_date_jj)
plot(biss$HATCH_DATE, biss$hatch_date_jj)