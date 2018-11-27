#### Averaging models - example in Shipley, 2013 ####

df <- as.data.frame(cbind(modele = c(6, 1, 5, 3, 2, 4), AICc = c(28.239, 28.542, 29.092, 31.224, 33.983, 39.105), dAIC = c(0, 0.303, 0.853, 2.985, 5.744, 10.866)))
summary(df)
df$W <- (exp(-df$dAIC/2)/sum(exp(-df$dAIC/2)))


#### Averaging models - Path paper ####

df <- as.data.frame(cbind(modele = c(1, 2, 3, 7, 5, 6, 4), AICc = c(18.045, 20.115, 22.616, 28.525, 30.131, 30.19, 34.888), dAIC = c(0, 2.07, 4.571, 10.48, 12.086, 12.145, 16.843)))
summary(df)
df$W <- (exp(-df$dAIC/2)/sum(exp(-df$dAIC/2)))
df
sum(df$W)
