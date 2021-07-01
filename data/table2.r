library(tidyverse)
library(janitor)

#install packages written on the rmd
libs <- c("yarrr","dplyr","psych", "glmmADMB", "lsmeans", "nlme", "stargazer")
lapply(libs, require, character.only = TRUE)
install.packages(libs)

data1 <- read.csv("data/Nichols_et_al_data.csv")

#age is mutated to this TO CENTRE the variables so a 60 yr old wouldnt chuck the data off. 
#its just how far they are from the mean. 
data2 <- data1 %>%
  mutate(age.c = age - mean(age, na.rm=T))

# Reorder conditions to religious, secular, noise and control
data2$con[data2$con==4] <- 0 
data2$con[data2$con==1] <- 4
data2$con[data2$con==3] <- 1
data2$con[data2$con==4] <- 3

# treatment variable
data2$con<-factor(data2$con,levels= c(0,1,2,3),
                  labels = c("Religious", "Secular", "Noise","Control"))


# label each of the sites
data2$sit <- data2$site
data2$site<-factor(data2$site,levels= c(1,2,3),
                   labels = c("USA", "CZ","JP"))

# Create site subsamples
USA <- data2[data2$site=="USA",]
CZ <- data2[data2$site=="CZ",]
JP <- data2[data2$site=="JP",]
sites <- c("USA", "CZ", "JP")

#claim as a function of condition, sex, age and site. This is not significant.
summary(lm1 <- lm(claim ~ con + sex + age.c + site, data = data2))

# claim as a function of the interaction with religiosity 
summary(lm2 <- lm(claim ~ con*relig + sex + age.c + site, data=data2))
