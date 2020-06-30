train.data<-read.csv("https://raw.githubusercontent.com/agconti/kaggle-titanic/master/data/train.csv")
str(train.data)
as.factor(train.data$Pclass)
as.factor(train.data$Survived)
summary(train.data)
str(train.data)
length(is.na(train.data$Age))
train.data$Name[grepl(" Mr\\.", train.data$Name)]
train.data$Embarked<-replace(train.data$Embarked, train.data$Embarked=="", NA) 

train.data[which(is.na(train.data$Embarked)),]$Embarked="S"

table(train.data$Embarked)
