# Read a big data set using fread from package data.table
# you can use the command read.csv instead
phys <- data.table::fread('dataBC3/physio.csv')
phys <- phys[,c('datetimeFM', 'chamber', 'month', 'PAR', 'Tair_al', 'A_area', 'E_area', 'gmes_area', 'T_treatment')]
str(phys)
# Format date and time
phys$datetimeFM <- lubridate::ymd_hms(phys$datetimeFM)
# Select daytime data
physDay <- subset(phys, PAR >= 5)
# write my custon function to fit a non-linear regression to my data
fitLightCur <- function(df){
  myFit <- nls(A_area ~ a * (1 - exp(-b * PAR)), start = list(a = 10, b = 0.001), data = df)
  return(myFit)
}
# create a list with my individual data sets using loops
myMon <- c('Oct', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr')
# create an empty list
empty <- list()
# Add elements to my list
for (i in 1:length(myMon)){
  empty[[i*2-1]] <- subset(physDay, month == myMon[i] & T_treatment == 'ambient')
  empty[[i*2]] <- subset(physDay, month == myMon[i] & T_treatment == 'warmed')
}
# have a look at the different elements in my list
length(empty)
head(empty[[1]])
lapply(empty, head)
lapply(empty, tail)
# fit my custom non-linear function to my data sets and have a look
lighCur <- lapply(empty, fitLightCur)
summary(lighCur[[1]])
confint(lighCur[[1]])
# organize non-linear regression results in a data frame
results <- data.frame(row.names = 1:length(empty))
results$T_treatment <- rep(c('ambient', 'warmed'), 6)
results$month <- c('Oct', 'Oct', 'Dec', 'Dec', 'Jan', 'Jan', 'Feb', 'Feb', 'Mar', 'Mar', 'Apr', 'Apr')
for(i in 1:length(empty)){
  results$a[i] <- summary(lighCur[[i]])$coefficients[1]
  results$a_se[i] <- summary(lighCur[[i]])$coefficients[3]
  results$a_Pval[i] <- summary(lighCur[[i]])$coefficients[7]
  results$a_CIlw[i] <- confint(lighCur[[i]])[1]
  results$a_CIup[i] <- confint(lighCur[[i]])[3]
  results$b[i] <- summary(lighCur[[i]])$coefficients[2]
  results$b_se[i] <- summary(lighCur[[i]])$coefficients[4]
  results$b_Pval[i] <- summary(lighCur[[i]])$coefficients[8]
  results$b_CIlw[i] <- confint(lighCur[[i]])[2]
  results$b_CIup[i] <- confint(lighCur[[i]])[4]
}
print(results)