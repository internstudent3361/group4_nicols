# loading in libraries, including dplyr

library(tidyverse)
library(janitor)
library(dplyr)
library(gt)

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

# now we have the new variables (as above), we must find the means, SDs,
# CIs, and Cohen's Ds for each, grouped by condition.
# use group_by and summarise functions

data1 %>% 
  group_by(cond) %>%
  summarise(mean_claim = mean(claimpercent, na.rm = TRUE),
            mean_sac = mean(sacred, na.rm = TRUE),
            mean_neg = mean(negativity, na.rm = TRUE),
            mean_pos = mean(positivity, na.rm = TRUE),
            mean_imp = mean(impact, na.rm = TRUE),
            mean_temp = mean(tempo, na.rm = TRUE),
            SD_claim = sd(claimpercent, na.rm = TRUE),
            SD_sac = sd(sacred, na.rm = TRUE),
            SD_neg = sd(negativity, na.rm = TRUE),
            SD_pos = sd(positivity, na.rm = TRUE),
            SD_imp = sd(impact, na.rm = TRUE),
            SD_temp = sd(tempo, na.rm = TRUE)
            )

# Percent claimed variable

# Control Group (1): Means, SDs, number of observations, standard error and
# confidence interval lower and upper limits

mean1 <- mean(data1$claimpercent[data1$cond=="1"], na.rm = TRUE) # mean
sd1 <- sd(data1$claimpercent[data1$cond=="1"], na.rm = TRUE) # SD
n1 <- length(data1$cond[data1$cond=="1" & !is.na(data1$cond)]) # number of participants
se1 <- sd1/sqrt(n1) # standard error
lCI1 <- mean1 - (1.96*se1)
uCI1 <- mean1 + (1.96*se1)









