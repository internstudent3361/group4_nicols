# loading libraries that nichols et al. used so 
# i can identify the functions they used

library(tidyverse)
library(dplyr)
library(yarrr)
library(psych)
library(lsmeans)

# could not install "glmmADMB" for this version of R
# lsmeans is being phased out so i have to use emmeans instead

# reading the csv

nichols <- read_csv("data/nichols_et_al_data.csv")

# test running the r script from r markdown

# create site
USA <- nichols[nichols$site=="USA",]
CZ <- nichols[nichols$site=="CZ",]
JP <- nichols[nichols$site=="JP",]
sites <- c("USA", "CZ", "JP")

# histogram of percentage of money claimed dishonestly
par(mfrow=c(1,1)) # creates rows?
hist(nichols$claim, breaks = seq(0,100,5), freq = F,xlab = 'Claim', ylab = 'Probability',
     main = 'Histogram with Normal PDF',col="grey78", border="deepskyblue1", lty=1,
     ylim=c(0,0.05),xlim=c(0,100)) # xlim and ylim specifies limits
# histogram not completed - figuring out functions still
# need to figure out the "sit" tibble

# histograms of claims per site
par(mfrow=c(1,3)) # creates rows?
for (i in 1:3){ # not sure what i means
  sit = eval(parse(text = sites[i])) # eval(parse) is returning an unevaluated function call?
  hist(sit$claim, breaks = seq(0,100,5), freq = F, # creating histogram
       main = parse(text = sites[i]), col="grey78", border="deepskyblue1",
       lty=1, ylim=c(0,0.1))} # ylim specifies limit
# still figuring out what each function means
# - plot does not look completed either