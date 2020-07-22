yelp_data <- read.csv(file="CMTH642 Lab 7 yelp_dataset.csv", header=TRUE, sep=",")
summary(yelp_data)
train_index <- sample(1:nrow(yelp_data), 0.7 * nrow(yelp_data))
train.set <- yelp_data[train_index,]
test.set <- yelp_data[-train_index,]
train.set_new <- train.set[-2]
test.set_new <- test.set[-2]
glm_model <- glm(class~.,train.set, family = "binomial")
summary(glm_model)
summary(train.set)
predict(glm_model, newdata=data.frame(city="Toronto", review_count=200,
                                      categories="Coffee or Sandwiches"), type="response")
predicted <- predict(glm_model, test.set, type="response")
predicted_class <- ifelse(predicted>=0.5, 1, 0)
ConfusionMatrix <- table(actual = test.set$class,
                         predicted = predicted_class)
sum(diag(ConfusionMatrix))/nrow(test.set)
