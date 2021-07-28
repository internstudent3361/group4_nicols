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

# nice work with this plot!! it looks great so please don't feel like you have to make these changes, but here are a few ideas if you want to give them a go

## 1. create a new tibble data1_summary that just has the summary stats 
  # for example, aim to have 5 variables: cond, mean_claim, sd_claim, n (you can use n = n() ), and se (sd/sqrt(n))... (hint, you'll need to use group_by and summarise)
  # this should give you a tibble with 5 columns and 4 rows (1 per condition)
## 2. each layer of ggplot can have a unique dataset, so now for geom_col you can set data=data1_summary (I don't think this will be any different compared to what you have)
  # I think you'll still have to convert cond to numeric and can adjust it's placement like this: geom_col(...aes(x=as.numeric(cond)-.25, y=mean)...)
## 3. but now you can add in error bars because you have calculated the se

## just as a side note, I found a function geom_flat_violin() PupillometryR package which does exactly what you have with stat_halfeye but potentially in 1 line of code 
  # if you want to try geom_flat_violin(position = position_nudge(x=.1, y=0), adjust=2)
