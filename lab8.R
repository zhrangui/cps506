data(iris)
iris_ind <-iris[,1:4]
iris.pca.rawdata <- prcomp(iris_ind, scale = FALSE, center= FALSE)
iris.pca.rawdata
head(iris_ind%*%iris.pca.rawdata$rotation)
plot(iris.pca.rawdata, type = "b", main='without data normalization')
scale(c(12,34,434,343))
