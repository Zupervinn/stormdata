library(dplyr)

url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
if(!file.exists("Assignment2")){
        dir.create("Assignment2")
}

download.file(url, destfile="./Assignment2/stormdata.csv.bz2")

setwd("./Assignment2")
OriginalData <- read.csv("stormdata.csv.bz2")

#copying data 
intData <- arrange(OriginalData, EVTYPE)

#changing variable names and interested EVTYPE to to lower case.
names(intData) <- tolower(names(intData))
intData$evtype <- tolower(as.character(intData$evtype))

#copying data
rename <- select(intData, evtype, fatalities, injuries)

rename$evtype <- gsub("flash flooding", "flash flood", rename$evtype)
rename$evtype <- gsub("flood/flash flood|flooding", "flood", rename$evtype)
rename$evtype <- gsub("freezing drizzle", "freezing rain", rename$evtype)
rename$evtype <- gsub("gusty winds", "gusty wind", rename$evtype)
rename$evtype <- gsub("heavy surf|heavy surf/high surf|high seas", "high surf", rename$evtype)
rename$evtype <- gsub("high wind|high wind and seas|high winds|high winds/snow|strong winds", "strong wind", rename$evtype)
rename$evtype <- gsub("hurricane erin|hurricane opal|hurricane/typhoon", "hurricane", rename$evtype)
rename$evtype <- gsub("tropical storm gordon", "tropical storm", rename$evtype)
rename$evtype <- gsub("wild/forest fire", "wild fires", rename$evtype)
rename$evtype <- gsub("winds", "wind", rename$evtype)
rename$evtype <- gsub("winter storm high winds|winter storms", "winter storm", rename$evtype)
rename$evtype <- gsub("winter weather/mix", "winter weather", rename$evtype)
rename$evtype <- gsub("landslides", "landslide ", rename$evtype)
rename$evtype <- gsub(".*tornado.*", "tornado", rename$evtype)
rename$evtype <- gsub(".*hail.*", "hail", rename$evtype)
rename$evtype <- gsub(".*heat.*", "heat", rename$evtype)
rename$evtype <- gsub(".*wind.*", "wind", rename$evtype)
rename$evtype <- gsub("riverflood|coastal flood", "flood", rename$evtype)

# sum & ordering after renaming the characters
summData <- aggregate(cbind(fatalities, injuries) ~ evtype, data = rename, sum)
cleanData <- arrange(subset(subset(summData, fatalities>0), injuries>0), desc(fatalities), desc(injuries))

head(cleanData)

topInjuries <- arrange(cleanData[-2], desc(injuries))[1:5,]
topFatalities <- arrange(cleanData[-3], desc(fatalities))[1:5,]

ggplot(topInjuries, aes(x=evtype, y=injuries)) + geom_point() + xlab("Severe weather events: EVTYPE") + ylab("Number of injuries") + ggtitle("Top 5 injuries") 

ggplot(topFatalities, aes(x=evtype, y=fatalities)) + geom_point() + xlab("Severe weather events: EVTYPE") + ylab("Number of fatalities") + ggtitle("Top 5 fatalities")

