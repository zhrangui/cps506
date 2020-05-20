cars <- read.csv("auto-mpg.data", header=FALSE, stringsAsFactors=FALSE, sep="", na.strings=c("?","!", "NA"))
summary(cars)
str(cars)
head(cars)
cars$V4 <- as.numeric(cars$V4)
plot(cars$V2,cars$V3)
cars[which(is.na(cars)),]

