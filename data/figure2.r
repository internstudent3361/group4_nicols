library(tidyverse)
library(janitor)

data1 <- read.csv("data/Nichols_et_al_data.csv")


#Cleaning up the data for use

dataA <- data1 %>%                                                          
  filter(include == 0) %>% 
  
  rename(cond = con,
         claimpercent = claim,
         claimmoney = moneyclaim,
         CT_practice = completion.time..practice.included.,
         CT_payments = completion.time..payments.only.,
         religiosity = relig,
         religion = Religion) %>% 
  
  mutate(religiosity = abs(religiosity - 5),
         ritual = abs(ritual - 7),              # reverse coding
         claimpercent = claimpercent * 100,     #turning this into a percentage value
         affil = as.factor(affil_cong)) %>% 
  
  select(cond:ritual, -claimmoney, -sex, -age, -Religion.Text, -religion, -starts_with("CT")) %>% # selecting only the columns required
  
  as_data_frame() 

# Re-order conditions to: religous, secular, noise, and control

dataA$cond[dataA$cond==4] <- 0 # Make religious prime the reference category
dataA$cond[dataA$cond==1] <- 4 # This is in a weird order as R reads the code line by line, so if we go from top to bottom, 
dataA$cond[dataA$cond==3] <- 1 # we're changing the number twice which screws up our dataframe
dataA$cond[dataA$cond==4] <- 3


# treatment variable
dataA$cond <- factor(dataA$cond, levels= c(0,1,2,3),
                labels = c("Religious", "Secular", "Noise","Control"))

# creating variables so we can filter out specific values for each plot
dataB <- dataA
dataC <- dataA


# filtering out NA values for each plot 

dataA <- dataA %>% 
  filter(!is.na(religiosity))

dataB <- dataB %>% 
  filter(!is.na(ritual))

dataC <- dataC %>% 
  filter(!is.na(affil))

# plotting the figure

figA <- ggplot(dataA, aes(religiosity, claimpercent, color = cond)) +
  geom_smooth(method = "lm", se = FALSE) + #method = "lm" creates a straight line of best fit
  theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  coord_cartesian(ylim = c(0, 50))


figB <- ggplot(dataB, aes(ritual, claimpercent, color = cond)) +
  geom_smooth(method = "lm") +
  theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  coord_cartesian(ylim = c(0, 50))

figC <- ggplot(dataC, aes(affil, claimpercent, color = cond)) +
  geom_smooth(method = "lm") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  coord_cartesian(ylim = c(0, 50))

plot(figA)
plot(figB)
plot(figC)
