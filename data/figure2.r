library(tidyverse)
library(janitor)
library(gridExtra)
library(gmodels)


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

  
#Make a new dataframe that contains CI limits for all conditions based on whether participants are religiously affiliated or not

groupA <- dataA %>% 
  group_by(cond, affil) %>% 
  summarise(mean = mean(claimpercent, na.rm = TRUE),
            sd = sd(claimpercent, na.rm = TRUE),
            n = n()) %>% 
  drop_na() %>% 
  mutate(se = sd / sqrt(n),
         lowerCI = mean - qt(1 - (0.05/2), n - 1) * se,
         upperCI = mean + qt(1 - (0.05/2), n - 1) * se) %>% 
  ungroup()
  



# plotting the figure

figA <- ggplot(dataA, aes(religiosity, claimpercent, color = cond)) +
  geom_smooth(method = "lm", se = FALSE) + #method = "lm" creates a straight line of best fit
  theme_light() + #Gives white background to plot
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + #Removes gridlines from plot
  coord_cartesian(ylim = c(0, 50)) + #Sets y limit to 50
  labs(x = "Religiosity", y = "Percentage claimed", title = "Condition*Religiosity") + #axis labels and title
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")   #centres title text and removes legend


figB <- ggplot(dataA, aes(ritual, claimpercent, color = cond)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  coord_cartesian(ylim = c(0, 50)) +
  labs(x = "Ritual frequency", y = "Percentage claimed", title = "Condition*Ritual frequency") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

figC <- ggplot(groupA, aes(affil, mean, color = cond)) +
  geom_line(aes(cond = cond)) +
  geom_errorbar(data = groupA, aes(ymin = lowerCI, ymax = upperCI)) +
  theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Religious affiliation", y = "Percentage claimed", title = "Condition*Religious affiliation") +
  theme(plot.title = element_text(hjust = 0.5)) 

grid.arrange(figA, figB, figC, ncol = 3)

# nice work so far!! A couple of ideas for figure 2c:
# you're absolutely right, it looks like the authors did use 95% CI for the figure

# option 1: there are functions you can use to compute the CI yourself. I found an example here: https://stackoverflow.com/questions/35953394/calculating-length-of-95-ci-using-dplyr
  # library(gmodels) has a function ci() that will help you get the mean, low CI, high CI, and sd (see example above)
  # you want to make sure to use group_by before using the ci()
  # pretty much you want to end up with a dataframe that has the following variables (condition, affiliation, mean, lowCI, and highCI, and sd can't hurt)
  # then you should be able to use geom_line() and geom_errorbar()

# option 2: recreate the plot using se instead of CI
  # the error bars will be smaller, but I think this is the second best option as you will still be able to plot some measure of error
  # se = sd / sqrt(n)

