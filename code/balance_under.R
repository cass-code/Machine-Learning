balance_under <- function(Adult){

  library(dplyr)
  library(tidyverse)
  library(ROSE)

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


  africa <- cleaned %>% filter(race == "African" | race == "White") # First splitting the dataset
  coloured <- cleaned %>% filter(race == "Coloured" | race == "White")


  # balanced data set with under-sampling
  balanced.under <- ovun.sample(race~., data=africa,
                                p=0.5, seed=1,
                                method="under")$data

  # Balancing white and coloured
  balanced.under1 <- ovun.sample(race~., data=coloured,
                                 p=0.5, seed=1,
                                 method="under")$data



  colour <- balanced.under1 %>% filter(race == "Coloured")
  asia <- cleaned %>% filter(race == "Asian/Indian")

  bal_under <- bind_rows(balanced.under, colour, asia)
  bal_under

}