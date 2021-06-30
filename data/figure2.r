library(tidyverse)
library(janitor)

data1 <- read.csv("data/Nichols_et_al_data.csv")


#Cleaning up the data for use

data1 <- data1 %>%                                                          
  filter(include == 0) %>% 
  rename(cond = con,
         claimpercent = claim,
         claimmoney = moneyclaim,
         CT_practice = completion.time..practice.included.,
         CT_payments = completion.time..payments.only.,
         religiosity = relig,
         religion = Religion) %>% 
  select(cond:ritual, -claimmoney, -sex, -age, -Religion.Text, -religion, -starts_with("CT")) %>% # selecting only the columns required
  as_data_frame() 

#Cleaning up data for plotA
dataA <- data1 %>% 
  select(cond:religiosity) 



# Creating plot A.


plotA <- ggplot(dataA, aes(religiosity, claimpercent)) +
  geom_smooth(method="lm")

plot(plotA)
  