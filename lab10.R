library(MASS)
data(cats)
cor(cats$Bwt, cats$Hwt, method = "pearson")
x<-cats$Bwt
y<-cats$Hwt
sum( (x - mean(x)) * (y - mean(y)) ) /
  ( (length(x)-1) * sd(x) * sd(y) )
