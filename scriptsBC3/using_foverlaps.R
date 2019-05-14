library(lubridate)
library(data.table)
short <- fread('shortFile.csv')
short$DateTime <- ymd_hms(as.character(short$DateTime))
long <- fread('longFile.csv')
long$DateTime <- ymd_hms(as.character(long$DateTime))
long$Start <- long$DateTime
long$End <- long$DateTime

# indicate the start and end of each time interval in the low frequency file
short$Start <- short$DateTime
short$End <- shift(short$DateTime, type='lead')
short <- na.omit(short)
# index low frequency file
setkey(short, Start, End)
longIndex <- foverlaps(long, short, type='any', nomatch = 0)
# Column i.DateTime is now the original time of the long file