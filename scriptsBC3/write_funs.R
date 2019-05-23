# Write a custom function to calculate standard error omiting NA
s.err <- function(x){
  se <- sd(x)/sqrt(length(x))
  return(se)
}
# Try my funtion in some made up data without NA
x1 <- rnorm(1:100)
mean(x1)
s.err(x1)
# Try the same in data with NAs
x2 <- c(rnorm(1:99), NA)
mean(x2)
mean(x2, na.rm = T)
s.err(x2)
# Write a custom function to calculate standard error omiting NA
s.err.na <- function(x){
  se <- sd(x, na.rm = TRUE)/sqrt(length(which(!is.na(x))))
  return(se)
}
s.err.na(x2)
# Use my function with many data sets
# Create multiple datasets and organize them in list form
many <- list()
for (i in 1:20){
  many[[i]] <- rnorm(1:100)
}
meanMany <- lapply(many, mean)
seMany <- lapply(many, s.err.na)
# Write means and SE onto a data frame
df <- data.frame(row.names = 1:length(many))
df$mean <- cbind(meanMany)
df$se <- cbind(seMany)

# An example using if else
# can I call my mother in law that is the US?
callDorothy <- function(x){
  if (lubridate::hour(x - 6*60*60) < 8){
    print('no')
  } else{
    print('yes')
  }
}
# example using current time
callDorothy(Sys.time())