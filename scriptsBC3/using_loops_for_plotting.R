source('scriptsBC3/custom_functions_loops.R')
# Plot light (PAR) over time for each one of the campaigns
# Set graphical parameters
par(mfrow=c(2,3), mar=c(4,6.5,1.5,1.5), las=1)
for (i in 1:length(empty)){
  plot(empty[[i]]$PAR ~ empty[[i]]$datetimeFM, ylim = c(0,2300), pch = 19, col = 'blue',
       main = empty[[i]]$month[i], cex.lab = 1.4,
       ylab = expression(PPFD~(mu*mol~m^-2~s^-1)), xlab = ' ')
}
# Create a list of data frames with the predictions from the nls fit
predL <- list()
for (i in 1:length(empty)){
  predL[[i]] <- data.frame(row.names = 1:100)
  predL[[i]]$PAR <- seq(0, 2300, length.out = 100)
  predL[[i]]$predA <- results$a[i] *(1 - exp(-results$b[i] * predL[[i]]$PAR))
}
# Create a vector with different point characters
myPch <- c(15, 16, 17, 18, 19, 8)
par(mfrow = c(1, 1), mar = c(4, 6, 1.5, 1.5), las = 1, cex = 1.1)
library(scales)
plot(subset(physDay, month == myMon[1] & T_treatment == 'ambient')$A_area ~
       subset(physDay, month == myMon[1] & T_treatment == 'ambient')$PAR,
     xlim = c(0, 2300),
     ylim = c(quantile(physDay$A_area, prob = 0.001, na.rm = T), quantile(physDay$A_area, prob = 0.999, na.rm = T)),
     pch = myPch[1], col = alpha('blue', 0.5), cex.lab=1.3,
     xlab = expression(PPFD~(mu*mol~m^-2~s^-1)),
     ylab = expression(italic(A)[net]~(mu*mol~m^-2~s^-1)))
for(i in 2:length(myMon)){
  points(subset(physDay, month == myMon[i] & T_treatment == 'ambient')$A_area ~
           subset(physDay, month == myMon[i] & T_treatment == 'ambient')$PAR,
         pch = myPch[i], col =alpha( 'blue', 0.5))
}
for(i in 1:length(myMon)){
  points(subset(physDay, month == myMon[i] & T_treatment == 'warmed')$A_area ~
           subset(physDay, month == myMon[i] & T_treatment == 'warmed')$PAR,
         pch = myPch[i], col = alpha('red', 0.5))
}
for (i in 1:length(predL)){
  lines(predL[[i]]$predA ~ predL[[i]]$PAR, lwd = 0.8)
}
legend('topleft', legend = myMon, pch = myPch, bty = 'n')
legend('topright', legend = levels(as.factor(physDay$T_treatment)),
       pch = 19, col = c('blue', 'red'), bty = 'n')
