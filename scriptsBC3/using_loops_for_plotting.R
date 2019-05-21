phys <- data.table::fread('dataBC3/physio.csv')
phys <- read.csv('dataBC3/physio.csv')
phys <- phys[,c('datetimeFM', 'chamber', 'month', 'PAR', 'Tair_al', 'A_area', 'E_area', 'gmes_area', 'T_treatment')]
str(phys)
phys$datetimeFM <- lubridate::ymd_hms(phys$datetimeFM)
myMon <- c('Oct', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr')
windows(12,8)
par(mfrow=c(2,3), mar=c(4,6.5,1.5,1.5), las=1)
for (i in 1:length(myMon)){
  k <- subset(phys, month == myMon[i])
  plot(k$PAR ~ k$datetimeFM, ylim = c(0,2300), pch = 19, col = 'blue', main = k$month[i],
       ylab = expression(PPFD~(mu*mol~m^-2~s^-1)), xlab = ' ', cex.lab = 1.4)
}

phys2 <- subset(phys, PAR >= 5)
myPch <- c(15, 16, 17, 18, 19, 8)
windows(10, 10)
par(mfrow = c(1, 1), mar = c(4, 6, 1.5, 1.5), las = 1, cex = 1.1)
library(scales)
plot(subset(phys2, month == myMon[1] & T_treatment == 'ambient')$A_area ~
       subset(phys2, month == myMon[1] & T_treatment == 'ambient')$PAR,
     xlim = c(0, 2300),
     ylim = c(quantile(phys2$A_area, prob = 0.001, na.rm = T), quantile(phys2$A_area, prob = 0.999, na.rm = T)),
     pch = myPch[1], col = alpha('blue', 0.5), cex.lab=1.3,
     xlab = expression(PPFD~(mu*mol~m^-2~s^-1)),
     ylab = expression(italic(A)[net]~(mu*mol~m^-2~s^-1)))
for(i in 2:length(myMon)){
  points(subset(phys2, month == myMon[i] & T_treatment == 'ambient')$A_area ~
           subset(phys2, month == myMon[i] & T_treatment == 'ambient')$PAR,
         pch = myPch[i], col =alpha( 'blue', 0.5))
}
for(i in 1:length(myMon)){
  points(subset(phys2, month == myMon[i] & T_treatment == 'warmed')$A_area ~
           subset(phys2, month == myMon[i] & T_treatment == 'warmed')$PAR,
         pch = myPch[i], col = alpha('red', 0.5))
}
legend('topleft', legend = myMon, pch = myPch, bty = 'n')
legend('topright', legend = levels(as.factor(phys2$T_treatment)),
       pch = 19, col = c('blue', 'red'), bty = 'n')