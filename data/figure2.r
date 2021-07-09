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
         affil = as.factor(affil_cong))     
         
  
  select(cond:ritual, -claimmoney, -sex, -age, -Religion.Text, -religion, -starts_with("CT")) %>% # selecting only the columns required
  
  as_data_frame() 

# Re-order conditions to: religous, secular, noise, and control

dataA$cond[dataA$cond==4] <- 0 # Make religious prime the reference category
dataA$cond[dataA$cond==1] <- 4 # This is in a weird order as R reads the code line by line, so if we go from top to bottom, 
dataA$cond[dataA$cond==3] <- 1 # we're changing the number twice which screws up our dataframe
dataA$cond[dataA$cond==4] <- 3


# labelling the levels of the treatment variable

dataA$cond <- factor(dataA$cond, levels= c(0,1,2,3),
                labels = c("Religious", "Secular", "Noise","Control"))

dataB <- dataA %>% 
  filter(cond == "Religious")
  
  
  
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
  
# labelling the levels of religious affiliation 

groupA$affil <- factor(groupA$affil, levels = c(0, 1), 
                       labels = c("Non-affiliated", "Affiliated"))


# plotting the figure

figA <- ggplot(dataA, aes(religiosity, claimpercent, color = cond)) +
  geom_smooth(method = "lm", se = FALSE, size = 1.5) + #method = "lm" creates a straight line of best fit
  geom_smooth(data = dataB, size = 0, se = TRUE, method = "lm", fill = "#fbf1d8") + #this creates the SE for the religious condition
  theme_light() + #Gives white background to plot
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + #Removes gridlines from plot
  coord_cartesian(ylim = c(0, 50)) + #Sets y limit to 50
  labs(x = "Religiosity", y = "Percentage claimed") + #axis labels 
  theme(legend.position = "none") +  #removes legend
  facet_grid(. ~ "Condition*Religiosity") + # title, we use facet_grid to put it in a grey box
  scale_color_manual(values = c("#dea520", "#5ab3e4", "#094689", "#a4a4a4")) #change the colours of the lines to fit source

figB <- ggplot(dataA, aes(ritual, claimpercent, color = cond)) +
  geom_smooth(method = "lm", se = FALSE, size = 1.5) +
  geom_smooth(data = dataB, size = 0, se = TRUE, method = "lm", fill = "#fbf1d8") +
  theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  coord_cartesian(ylim = c(0, 50)) +
  labs(x = "Ritual frequency", y = "Percentage claimed") +
  theme(legend.position = "none") +
  facet_grid(. ~ "Condition*Ritual frequency") +
  scale_x_continuous(breaks = seq(0, 6, 1)) +  #adjusting the tick marks so 7 marks appear
  scale_color_manual(values = c("#dea520", "#5ab3e4", "#094689", "#a4a4a4"))

figC <- ggplot(groupA, aes(affil, mean, color = cond)) +
  geom_line(aes(group = cond), position = position_dodge(width = 0.5), size = 1.5) + #group our lines by condition, we use position dodge so nothing overlaps
  geom_errorbar(data = groupA, aes(ymin = lowerCI, ymax = upperCI), width = 0.2, position = position_dodge(width = 0.5), size = 1.3) +
  theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ylim(0, 50) +
  scale_x_discrete() +
  labs(x = "Religious affiliation", y = "Percentage claimed") +
  facet_grid(. ~ "Condition*Religious affiliation") +
  scale_color_manual(values = c("#dea520", "#5ab3e4", "#094689", "#a4a4a4"))


grid.arrange(figA, figB, figC, ncol = 3)

