English <- c(56,75,45, 71,62,64,58,80,76,61)
Math <- c(66, 70, 40, 60, 65, 56, 59, 77, 67, 63)

cor.test(English, Math, method = "pearson")
cor.test(English, Math, method = "spearman")
Testnames<- rep(c('DT','LR', 'NB'), each=10, rep =3)
as.factor(rep(c(1:10), 3))

