library("caret") 
folds <- createFolds(iris$Species)
for (f in folds) {
  train <- iris[-f,]
  test <- iris[f,]
  i <- 1
  }

