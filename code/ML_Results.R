ML_Results <- function(cleaned){


  library(caret)
  library(tidyverse)


  # create a list of 70% of the rows in the original dataset we can use for training
  validation_index <- createDataPartition(cleaned$race, p=0.70, list=FALSE)
  # select 30% of the data for validation
  validation <- cleaned[-validation_index,]
  # use the remaining 80% of data to training and testing the models
  cleaned <- cleaned[validation_index,]

  # Run algorithms using 10-fold cross validation
  control <- trainControl(method="cv", number=10)
  metric <- "Accuracy"

  # linear algorithms
  set.seed(7)
  fit.lda <- train(race~., data=cleaned, method="lda", metric=metric, trControl=control)

  # nonlinear algorithms

  # CART
  set.seed(7)
  fit.cart <- train(race~., data=cleaned, method="rpart", metric=metric, trControl=control)

  # kNN
  set.seed(7)
  fit.knn <- train(race~., data=cleaned, method="knn", metric=metric, trControl=control)

  # advanced algorithms
  # SVM
  set.seed(7)
  fit.svm <- train(race~., data=cleaned, method="svmRadial", metric=metric, trControl=control)
  # Random Forest
  set.seed(7)
  fit.rf <- train(race~., data=cleaned, method="rf", metric=metric, trControl=control)

  results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
  #results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn))
  #r <- summary(results)
  #r



}