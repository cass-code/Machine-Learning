RMSE_data <- function(Adult){ 

source("code/comp_RMSE.R")

df <- comp_RMSE(Adult)
df$variable <- as.character(df$variable)
df <- df %>% mutate(variable = replace(variable, variable == "train.RMSE", "RMSE Train")) %>%
  mutate(variable = replace(variable, variable == "test.RMSE", "RMSE Test"))

source("code/linreg.R")
#str(lm1)
models<- linreg(Adult)
source("code/linreg_pred.R")
data1 <- linreg_pred(Adult) %>% tibble::as_tibble() 
r<- models[[4]]
#n<- models[[5]]
names<- data.frame("name" = c("Reg 1", "Reg 2", "Reg 3"))

data <- bind_cols(names, r, data1) %>% rename("RMSE Train" = x, "RMSE Test" = Reg) %>% tibble::as_tibble()
dff <- data %>% tidyr::gather(variable, value, -name)
final <- bind_rows(df, dff)

library(dplyr)

df1 <- comp_table(Adult)
data <- final %>% tibble::as_tibble() %>% tidyr::spread(key = name,value = value) %>% as.data.frame() %>% select(-variable)
names<- data.frame("Dataset" = c("Test", "Train"))
data1 <- bind_cols(names, data)
data1
}