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

# Re-order conditions to: religous, secular, noise, and control

dataA$cond[dataA$cond==4] <- 0 # make religious prime the reference category
dataA$cond[dataA$cond==1] <- 3
dataA$cond[dataA$cond==3] <- 2
dataA$cond[dataA$cond==2] <- 1


# treatment variable
dataA$cond <- factor(dataA$cond,levels= c(0,1,2,3),
                labels = c("Religious", "Secular", "Noise","Control"))



