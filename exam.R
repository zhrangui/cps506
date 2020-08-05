x<-c(1,7,8,10)
y<-c(6,7,18,20)

sqrt(sum((x-y)^2))
data(iris)
pc<-princomp(iris[,1:4], cor=T, score=T)
plot(pc)
a<-c(5,20,50)
(a-5)/(50-5)
