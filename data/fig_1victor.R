library(tidyverse)
library(janitor)
library(dplyr)
library(ggplot2)
library(ggdist) #used for first raincloud plot
library(data.table)

data1 <- read_csv("data/Nichols_et_al_data.csv")

data1 <- data1 %>%  
  filter(include == 0) %>% 
  rename(cond = con,
         claimpercent = claim,
         claimmoney = moneyclaim,
         CT_practice = `completion time (practice included)`,
         CT_payments = `completion time (payments only)`,
         religiosity = relig,
         religion = Religion) %>% 
  select(site, claimpercent, cond, id) %>% 
  mutate(claimpercent = claimpercent * 100) %>% 
  as_tibble()
  


data1$cond[data1$cond==4] <- 0 # Make religious prime the reference category
data1$cond[data1$cond==1] <- 4 # This is in a weird order as R reads the code line by line, so if we go from top to bottom, 
data1$cond[data1$cond==3] <- 1 # we're changing the number twice which screws up our dataframe
data1$cond[data1$cond==4] <- 3


data1 <- data1 %>% 
  mutate(numberOf = (cond == 0) * 100 + (cond == 1 | cond == 2) * 103 + (cond == 3) * 102) %>% 
  mutate(claimpercent2 = claimpercent / numberOf)

fig1 <- ggplot(data1, aes(x = cond)) +
  geom_col(
    aes(y = claimpercent2),
    width = .3
  ) +
  ggdist::stat_halfeye(
    aes(y = claimpercent),
    adjust = .5,
    width = .4,
    .width = 0,
    point_colour = NA,
    position = position_nudge(x = 0.17, y = 0)
  ) +
  coord_flip() +
  expand_limits(x = 0, y = 0) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))


  


plot(fig1)