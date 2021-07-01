library(tidyverse)
library(janitor)
library(dplyr)

data1 <- read.csv("data/Nichols_et_al_data.csv")

data1 <- data1 %>% 
  filter(include == 0)

view(data1)

data1 <- rename(claimpercent = claim,
                claimmoney = moneyclaim,
                CT_practice = completion.time..practice.included.,
                CT_payments = completion.time..payments.only.,
                religiosity = relig,
                religion = Religion) %>% 
  select(-CT_practice, -CT_payments, -CT, -CT_cheat, -Religion.Text, -affil_cong)

