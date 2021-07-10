open_file <- function(url){

#install.packages("dbplyr", "RSQLite") #install packages for SQL

library(dplyr)
library(dbplyr)
library(tidyverse)

file.location()

dir.create("data_raw", showWarnings = FALSE)
download.file(url = "https://www.datafirst.uct.ac.za/dataportal/index.php/catalog/712/download/9901") #NIDS wave 5 dataset
destfile = "data_raw/portal_nids.sqlite", mode = "wb")


con <- dbConnect(RSQLite::SQLite(), dbname = ":memory:") #NIDS is reasonably small so I am using memory here
nids <-

}