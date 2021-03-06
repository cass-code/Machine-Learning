# regularised regression

plot_lasso <- function(Adult){


  # libraries
  library(caret)
  library(ggplot2)
  library(tidyverse)
  library(rsample)
  library(glmnet)


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


  X <- model.matrix(income ~ age + age2 + male + race + married + school + tertiary, income_train)[,-1]
  Y <- income_train$income

  # Lasso regression


  lasso <- cv.glmnet(
    x = X,
    y = Y,
    alpha = 1)

  l <- plot(lasso)
  l

  # peaking at the minimum mean squared error and the relevant parameters(lambdas)
  #min(lasso$cvm)
  #lasso$lambda.min




}