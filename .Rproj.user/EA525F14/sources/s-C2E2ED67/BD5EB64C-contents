balance_both <- function(Adult){

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
  coloured <- cleaned %>% filter(race == "Coloured" | race == "African")
  asian <- cleaned %>% filter(race == "Asian/Indian" | race == "African")

  library(ROSE)

  # balanced data set with both-sampling
  balanced.both <- ovun.sample(race~., data=africa,
                               p=0.5, seed=1,
                               method="both")$data

  white <- balanced.both %>% filter(race == "White")

  # Balancing African and coloured
  balanced.both1 <- ovun.sample(race~., data=coloured,
                                p=0.5, seed=1,
                                method="both")$data



  colour <- balanced.both1 %>% filter(race == "Coloured")

  # Balancing African and Asian
  balanced.both2 <- ovun.sample(race~., data=asian,
                                p=0.5, seed=1,
                                method="both")$data

  asia <- balanced.both2 %>% filter(race == "Asian/Indian")

  bal_both <- bind_rows(balanced.both1, white, asia)
  bal_both

}