train<-read.csv("https://raw.githubusercontent.com/agconti/kaggle-titanic/master/data/train.csv")
str(train)
as.factor(train$Pclass)
as.factor(train$Survived)
train[is.na(train$Age) | is.na(train$Name),]$Name
summary(train)
str(train)
