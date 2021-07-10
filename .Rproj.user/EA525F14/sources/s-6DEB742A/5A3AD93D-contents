# machine learning with predicting income

linreg_pred <- function(Adult){

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

    set.seed(7)
    split <- initial_split(cleaned, prop = 0.7, strata = "income")
    income_train <- training(split)
    income_test <- testing(split)

    # multiple regression
    set.seed(123)
    (model1 <- train(form = income ~ age + school +tertiary,
                     data = income_train,
                     method= "lm",
                     trControl = trainControl(method="cv", number = 10)
    ))

    set.seed(123)
    (model2 <- train(form = income ~ age + age2 + school  + tertiary + race,
                     data = income_train,
                     method= "lm",
                     trControl = trainControl(method="cv", number = 10)
    ))

    set.seed(123)
    (model3 <- train(form = income ~ age + age2 + married + school +tertiary + male + race,
                     data = income_train,
                     method= "lm",
                     trControl = trainControl(method="cv", number = 10)
    ))

    # Make predictions and compute the RMSE
    predictions1 <- model1 %>% predict(income_test)
    pred1 <- data.frame("Reg" = RMSE(predictions1, income_test$income))

    predictions2 <- model2 %>% predict(income_test)
    pred2 <- data.frame("Reg" = RMSE(predictions2, income_test$income))

    predictions3 <- model3 %>% predict(income_test)
    pred3 <-data.frame("Reg" = RMSE(predictions3, income_test$income))

    g <- bind_rows(pred1, pred2, pred3)
    g

}
