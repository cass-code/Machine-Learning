# compare models RMSE

comp_RMSE <- function(Adult){

  # libraries
  library(caret)
  library(ggplot2)
  library(tidyverse)
  library(rsample)
  library(glmnet)
  library(dplyr)
  library(reshape2)
  library(gridExtra)
  library(grid)
  library(ggplot2)
  library(lattice)

  # cleaning data
  clean <- Adult %>% mutate(age = 2021 - w5_a_dob_y) %>%
    rename (gender = w5_a_gen, race = w5_a_popgrp, income = w5_a_em1pay, married = w5_a_mar, school = w5_a_edschgrd, tertiary = w5_a_edter) %>%
    select(age, gender, race, income, married, school, tertiary) # The NIDS data set has many variables so just selecting the ones I want
  clean$race <- as.numeric(clean$race)
  cleaned <- clean %>% filter(race > 0, income >0, gender >0, married >0, school >=0, tertiary >=0) %>%
    mutate(race = replace(race, race == 1, "African")) %>% mutate(race = replace(race, race == 2, "Coloured")) %>%
    mutate(race = replace(race, race == 3, "Asian/Indian")) %>% mutate(race = replace(race, race == 4, "White")) %>%
    mutate(race = replace(race, race == 5, "Other")) %>%
    filter(race == "African" |race == "Coloured" |race == "Asian/Indian" |race == "White") %>%
    mutate(age2 = age * age) %>%
    mutate(income = replace(income, income > 0, log(income))) %>%
    mutate(gender = replace(gender, gender == 2, 0)) %>%
    mutate(tertiary = replace(tertiary, tertiary == 2, 0)) %>%
    mutate(married = replace(married, married >= 2, 0)) %>%
    rename (male = gender) %>%
    drop_na()


  cleaned$income <- as.numeric(cleaned$income) # realised these should be numeric
  cleaned$gender <- as.numeric(cleaned$male)
  cleaned$married <- as.numeric(cleaned$married)
  cleaned$school <- as.numeric(cleaned$school)
  cleaned$tertiary <- as.numeric(cleaned$tertiary)
  cleaned$race <- as.factor(cleaned$race)


  # construct training data

  set.seed(123)
  split <- initial_split(cleaned, prop = 0.7, strata = "income")
  income_train <- training(split)
  income_test <- testing(split)

  X.train <- model.matrix(income ~., income_train)[,-1]
  Y.train <- income_train$income

  X.test <- model.matrix(income ~ ., income_test)[,-1]
  Y.test <- income_test$income


  # y.train <- as.numeric(income_train$income)  # response vector training set
  # y.test <- as.numeric(income_test$income) # response vector test set
  #
  # # model matrices for training and test set
  # # select all features except the response variable
  # x.train <- as.matrix(subset(income_train,
  #                             select = -c(income)))
  # x.test <- as.matrix(subset(income_test,
  #                            select = -c(income)))


  # calculates RMSE given a linear model object
  rmse = function(model){
    sqrt(sum((model$residual)^2)/nrow(model$model))
  }

  rmse2 = function(obs, preds){
    sqrt(sum((obs-preds)^2)/length(obs))
  }

  model.outcome = list()

  # linear regressions

  # calculate rmse on training set
  lin_model <- lm(formula = income ~ age + age2 + male + race + married + school + tertiary,
                  data = income_train)
  print(paste('RMSE on training set:', rmse(lin_model)))

  #store model object and results of rmse in the `list` object named `model.outcome`
  model.outcome[['baseline']] = list('model' = lin_model,
                                     'rmse' =  data.frame('name'= 'linear model',
                                                          'train.RMSE' = rmse(lin_model),
                                                          'test.RMSE' = NA))
  # predict
  lin.model.pred <- predict(lin_model, newdata = income_test)
  print(paste('RMSE on test set:', rmse2(Y.test, lin.model.pred))) # calculate RMSE for the test data set
  # store rmse for the test data set in the proper slot of the `list` object named `model.outcome`
  model.outcome[['baseline']]$rmse$test.RMSE <- rmse2(Y.test, lin.model.pred)


  # regularised regression

  X.train <- model.matrix(income ~., income_train)[,-1]
  Y.train <- income_train$income

  X.test <- model.matrix(income ~ ., income_test)[,-1]
  Y.test <- income_test$income

  # ridge

  ridge <- cv.glmnet(
    x = X.train,
    y = Y.train,
    alpha = 0)


  # prediction for the training set
  ridge.pred.train = predict(ridge, X.train, s = "lambda.min")
  # calculate rmse on training set
  print(paste('RMSE on training set:', rmse2(ridge.pred.train, Y.train)))
  # prediction for the test set
  ridge.pred.test = predict(ridge, X.test, s = "lambda.min")
  # calculate RMSE for the test data set
  print(paste('RMSE on test set:', rmse2(ridge.pred.test, Y.test)))

  model.outcome[['ridge']] = list('model' = ridge,
                                  'rmse' =  data.frame('name'= 'ridge regression',
                                                       'train.RMSE' = rmse2(ridge.pred.train, Y.train),
                                                       'test.RMSE' = rmse2(ridge.pred.test, Y.test)))

  # Lasso regression

  lasso <- cv.glmnet(
    x = X.train,
    y = Y.train,
    alpha = 1)

  # predict
  lasso.pred.train = predict(lasso, X.train, s = "lambda.min")

  # calculate rmse on training set
  print(paste('RMSE on training set:', rmse2(lasso.pred.train, Y.train)))

  # prediction for the test set
  lasso.pred.test = predict(lasso, X.test, s = "lambda.min")

  # calculate RMSE for the test data set
  print(paste('RMSE on test set:', rmse2(lasso.pred.test, Y.test)))


  # store model object and results of rmse in the `list` object named `model.outcome`
  model.outcome[['lasso']] = list('model' = lasso,
                                  'rmse' =  data.frame('name'= 'Lasso regression',
                                                       'train.RMSE' = rmse2(lasso.pred.train, Y.train),
                                                       'test.RMSE' = rmse2(lasso.pred.test, Y.test)))


  res.matrix = do.call(rbind, model.outcome)
  # extract data frame object
  res.df = do.call(rbind, res.matrix[,2])


  # melt to tidy data frame
  df = melt(res.df, id.vars = "name", measure.vars = c("train.RMSE", "test.RMSE"))
  df


}