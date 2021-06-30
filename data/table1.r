# loading in libraries, including dplyr

library(tidyverse)
library(janitor)
library(dplyr)

#loading the data

data1 <- read.csv("data/Nichols_et_al_data.csv")

# cleaning the data for use:
# filtering out participants that weren't included in the final data
# renaming variables to make more sense

data1 <- data1 %>% 
  filter(include == 0) %>% 
  rename(cond = con,
         claimpercent = claim,
         claimmoney = moneyclaim,
         CT_practice = completion.time..practice.included.,
         CT_payments = completion.time..payments.only.,
         religiosity = relig,
         religion = Religion) %>% 
  select(-CT_practice, -CT_payments, -CT, -CT_cheat, -Religion.Text, -affil_cong)

# creating new variable using the mutate function
# these are the variables seen in table 1 and as seen in the Rmd file in OSF

data1 <- data1 %>% 
  mutate(
    negativity = (distressing + irritating + boring + sad)/4,
    positivity = (interesting + pleasant + exciting + relaxing + happy)/5,
    impact = (deep + powerful)/2,
    tempo = (fast + abs(slow-7))/2
  ) 




