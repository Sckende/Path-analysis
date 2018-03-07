setwd(dir = "C:/Users/God/Desktop/Claire/Data")
list.files()
f <- read.table("Path analysis_data 3bis.txt", sep = ",", dec = ".", h = T)
head(f)



tiff("fox_lmg_goose.tiff", res=300, width=15, height=15, pointsize=12, unit="cm", bg="transparent")
#dev.off()
par(bg = "transparent")
plot(f$AN, f$lmg_C1, bty = "n", type = "h", ylab = "", xlab = "", yaxt = "n", xaxt = "n", ylim = c(0, 6), col = "yellowgreen", lwd = 2)
axis(side = 2, lwd = 1)

plot(f$AN, f$nest_succ*100, xlab = "", ylab = "", bty = "n", xaxt = "n", yaxt = "n", type = "b", xaxp = c(1996, 2016, 10), ylim = c(0, 100), col = "olivedrab", lwd = 2, pch = 19)
lines(f$AN, f$prop_fox_dens, col = 'skyblue4', lwd = 2, type = 'b', pch = 19)
axis(side = 1, at = 1996:2016, lwd = 1)
axis(side = 4, lwd = 1)
dev.off() 





