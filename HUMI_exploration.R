rm(list = ls())

setwd("/Users/nicolas/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
 
hum <- read.table("HUMI_BYLCAMP.txt", h = T, sep = "\t", dec = ".")
temp <- read.table("TEMP_BYLCAMP.txt", h = T, sep = "\t", dec = ".")
prec <- read.table("PREC_precipitation_Bylot_1996-2016.txt", h = T, sep = "\t", dec = ",")

summary(hum)
hum <- hum[,-1]

summary(temp)
summary(prec)

# Fran method
temp$date <- paste(temp$YEAR, "-", temp$MONTH, "-", temp$DAY)

j <- list(hum, temp)
for (i in 1:2) {
  for (k in 1:nrow(j[[i]])) {
    j[[i]]$date[k] <- paste(j[[i]]$YEAR[k], "-", j[[i]]$MONTH[k], "-", j[[i]]$DAY[k])

  }
  j[[i]]$date <- as.factor(j[[i]]$date)
  j[[i]]$date <- strptime(j[[i]]$date,format="%Y - %m - %d") #blank before and after dashes are important
}

hum <- j[[1]] 
temp <- j[[2]]

# Extract the julian days
hum$JJ <- hum$date$yday + 1 # Add 1 because $yday starts at 0
temp$JJ <- temp$date$yday + 1 # Add 1 because $yday starts at 0

# To check validity of JJ data
j <- list(hum, temp)
# Process min (= 0) and max (= 366) of JJ for bissextil years (e.g. 1996, 2000, 2004, 2008, 2012, 2016)
for (r in 1:2) {
  for (i in c(2008, 2012, 2016)) {
    print (max(j[[r]]$JJ[j[[r]]$YEAR == i]))
    print (min(j[[r]]$JJ[j[[r]]$YEAR == i]))
  }
}

# And normal year (e.g. 2009, 2011, 2013, 2015)
for (r in 1:2) {
  for (i in c(2009, 2011, 2013, 2015)) {
    print (max(hum$JJ[hum$YEAR == i]))
    print (min(hum$JJ[hum$YEAR == i]))
  }
}

#### Plots ####

par( bg = NA)
plot(hum$HUM, type = "l", col = "darkturquoise")
line(temp$TEMP, col = "darkorange")
plot(prec$RAIN, type = "l", col = "blue")

# Match des JJ - YEAR between 3 files

j <- list(hum, temp, prec)
for (i in 1:3) {
  
}

#### FRANY part ####

nou <- merge(hum, temp, all = T) # Associe toutes les valeurs de hum et de temp en fonction des colones qui portent un nom identique
head(nou)

nou$date <- with(nou,paste(YEAR,MONTH,DAY,sep="-")) # avec objet "nou, fais un paste
# Traitement de dates
nou$date <- as.Date(nou$date,format="%Y - %m - %d")
nou$JJ <- as.integer(format(nou$date, "%j"))

nou <- merge(nou, prec, all = T)
head(nou)

dev.off()
x11()
par(mar = c(0,4,0,1), mfrow = c(3,1), oma = c(20,0,10,0))
plot(nou$TEMP, type = "l")
plot(nou$HUM, type = "l", col = "darkgreen")
plot(nou$RAIN, type = "l", col = "pink")

x11()
plot(nou$TEMP, nou$HUM)
plot(nou$RAIN, nou$HUM)

p <- nou[( nou$MONTH >= 6 & nou$MONTH <= 7), c("JJ", "TEMP")]
p <- na.omit(p)
x11()
plot(p$JJ,p$TEMP)
rrr <- predict(loess(p$TEMP~p$JJ))
lines(p$JJ, rrr, col = "red")
#### Interannual trend of summer precipitations ####

head(prec)
pre <- na.omit(prec)
b <- cbind(1995:2016, as.data.frame(tapply(pre$RAIN, pre$YEAR, sum)))
names(b) <- c("year", "cumul_prec")
dim(b)
summary(b)

x11()
plot(b$year, b$cumul_prec, xlab = "year", ylab = "Cumulative precipitation during summer")
lines(smooth.spline(b$year, b$cumul_prec, df = 4), col = "green")
lines(smooth.spline(b$year, b$cumul_prec, df = 2), col = "blue")
dev.copy2pdf()
dev.off()
