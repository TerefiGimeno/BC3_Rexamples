library(lubridate)
library(data.table)
short <- fread('dataBC3/shortFile.csv')
str(short)
short$DateTime <- ymd_hms(as.character(short$DateTime))
str(short)
long <- fread('dataBC3/longFile.csv')
long$DateTime <- ymd_hms(as.character(long$DateTime))

# indicate the start and end of each time interval in the high frequency file
long$Start <- long$DateTime
long$End <- long$DateTime
head(long)

# indicate the start and end of each time interval in the low frequency file
head(short)
short$Start <- short$DateTime
short$End <- shift(short$DateTime, type='lead')
head(short)
tail(short)
short <- na.omit(short)
tail(short)
str(short)
# index low frequency file
setkey(short, Start, End)
str(short)
longIndex <- foverlaps(long, short, type='any', nomatch = 0)
# Column i.DateTime is now the original time of the long file
head(longIndex)