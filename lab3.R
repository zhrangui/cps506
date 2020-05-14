df <- read.csv("lab3dataset.csv", header=FALSE, stringsAsFactors=FALSE,na.strings=c("", "NA"))
h=head(df)
names(df) <- c("ID", "Fname", "Lname", "Email", "Gender", "Country", "Amount", "Date")
colnames(df)
str(df$ID)
length(unique(df$Country[which(!is.na(df$Country))]))
sum(is.na(df$Gende))
lab3data <- df[!is.na(df$Country), ]

lab3data$Amount <- as.numeric(gsub("[$,]", "", lab3data$Amount))
lab3data$Date <- as.Date(lab3data$Date, "%m/%d/%Y")
head(lab3data)
lab3data$Days <- as.numeric(lab3data$Date - min(lab3data$Date))
lab3data$IndEmail <- 0
lab3data$IndEmail[grep(".gov|.org|.net", lab3data$Email)] <- 1
df.na <- df[which(!is.na(df$Country)),]
lma <- lm(lab3data$Amount ~ lab3data$Days + lab3data$IndEmail)
summary(lma)
lab3data[c(1,0),]
