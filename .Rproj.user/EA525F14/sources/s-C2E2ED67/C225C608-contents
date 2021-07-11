# SQL data processing

#install.packages("dbplyr", "RSQLite") #install packages for SQL

# libraries used
library(dplyr)
library(dbplyr)
library(tidyverse)
library(RSQLite)
library(haven)

#rm(list=ls())

##dir.create("data", showWarnings = FALSE)
#download.file(url = "https://www.datafirst.uct.ac.za/dataportal/index.php/catalog/712/download/9901",
# destfile = "data_raw/nids.zip", mode = "wb")
#nzip("data_raw/nids.zip", files = NULL, list = FALSE, overwrite = TRUE, junkpaths = FALSE, exdir = ".", unzip = "internal", setTimes = FALSE)

# Read in NIDS data
#(I tried downloading this from Data first using R but it gave too many problems - I think it was the passwords and API but I spent too long troubleshooting and decided to download and read it in manually)
Wave5 <- read_dta("data/Adult_W5_Anon_V1.0.0.dta")
Wave4 <- read_dta("data/Adult_W4_Anon_V2.0.0.dta")
Wave3 <- read_dta("data/Adult_W3_Anon_V3.0.0.dta")
Wave2 <- read_dta("data/Adult_W2_Anon_V4.0.0.dta")

# Starting with SQL
nids <- DBI::dbConnect(RSQLite::SQLite(), "data/nids-db~output.sqlite") #opening a new connection
# Writing the data into tables in the NIDS database
dbWriteTable(nids, "Wave2", Wave2)
dbWriteTable(nids, "Wave3", Wave3)
#dbWriteTable(nids, "Wave4", Wave4, overwrite = TRUE) I accidentally added the wrong data for wave 4 and had to overwrite it
dbWriteTable(nids, "Wave4", Wave4)
dbWriteTable(nids, "Wave5", Wave5)

# checking what tables are in the nids database (surprise! it's all the tables I just wrote to the database)
dbListTables(nids)
src_dbi(nids) # checking the source

dbListFields(nids, "wave5") # looking at fields in wave 5

### Query work ###

# Investigating the data

w5 <- tbl(nids, "wave5")
dim(w5) # looking at the dimensions shows that there are 1144 columns but because dplyr is lazy it doesn't count the number of rows
head(w5, n = 5) # looking at the first 5 entries
colnames(w5) # looking at the column names to help identify interesting variables
#names <- as.data.frame(colnames(w5)) %>% collect() #- could use this to create a table with the list of column names

# Cleaning the data and visualising it

raceprop <- tbl(nids, "wave5") %>%
    select (w5_a_dob_y, w5_a_gen, w5_a_popgrp, w5_a_em1pay, w5_a_mar, w5_a_edschgrd, w5_a_edter) %>%
    rename (gender = w5_a_gen, race = w5_a_popgrp, income = w5_a_em1pay, married = w5_a_mar, school = w5_a_edschgrd,
            tertiary = w5_a_edter) %>%
    filter(race > 0 & race<5) %>%
    group_by(race) %>% tally()

show_query(raceprop) # getting the SQL code for r chunk

school_dist <- tbl(nids, "wave5") %>% mutate(age = 2021 - w5_a_dob_y) %>%
    select (age, w5_a_gen, w5_a_popgrp, w5_a_em1pay, w5_a_mar, w5_a_edschgrd, w5_a_edter) %>%
    rename (gender = w5_a_gen, race = w5_a_popgrp, income = w5_a_em1pay, married = w5_a_mar, school = w5_a_edschgrd,
            tertiary = w5_a_edter) %>%
    filter(race > 0 & race<5) %>%  filter(school < 16 & school >=0 )

show_query(school_dist) # getting the SQL code for r chunk

married <- tbl(nids, "wave5") %>% mutate(age = 2021 - w5_a_dob_y) %>%
    select (age, w5_a_gen, w5_a_popgrp, w5_a_em1pay, w5_a_mar, w5_a_edschgrd, w5_a_edter) %>%
    rename (gender = w5_a_gen, race = w5_a_popgrp, income = w5_a_em1pay, married = w5_a_mar, school = w5_a_edschgrd,
            tertiary = w5_a_edter) %>%
    filter(race > 0 & race<5) %>%  filter(school < 16 & school >=0 ) %>% group_by(married) %>% tally()

show_query(married) # getting the SQL code for r chunk

# joining two tables

cleanw5 <- tbl(nids, "wave5") %>%
    mutate(age = 2021 - w5_a_dob_y) %>%
    rename (gender = w5_a_gen, race = w5_a_popgrp, income = w5_a_em1pay, married = w5_a_mar, school = w5_a_edschgrd, tertiary = w5_a_edter) %>%
    select(age, gender, race, income, married, school, tertiary) # basic cleaning of wave 5

cleanw4 <- tbl(nids, "wave4") %>%
    mutate(age = 2021 - w4_a_dob_y) %>%
    rename (gender = w4_a_gen, race = w4_a_popgrp, income = w4_a_em1pay, married = w4_a_mar, school = w4_a_edschgrd, tertiary = w4_a_edter) %>%
    select(age, gender, race, income, married, school, tertiary) # basic cleaning of wave 4

joined <- union_all(cleanw5, cleanw4) # joining the two data sets

headjoin <- head(joined, n=5) %>% collect() # looking at the first 5 rows of the joined data set and then saving this to the global environment

show_query(joined)

# disconnect from the database
dbDisconnect(nids)

# disconnect from the database
dbDisconnect(nids)
