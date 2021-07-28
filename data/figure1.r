library(tidyverse)
data <- read.csv("data/Nichols_et_al_data.csv")

data %>% 
  filter(include == 0) 

figure1 <- data %>% 
  filter(con, claim)
