# loading in libraries, including dplyr

library(tidyverse)
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

# Control Group (1)

mean1c <- mean(data1$claimpercent[data1$cond=="1"], na.rm = TRUE)*100 # mean
sd1c <- sd(data1$claimpercent[data1$cond=="1"], na.rm = TRUE)*100 # SD
n1c <- length(data1$cond[data1$cond=="1" & !is.na(data1$cond)]) # number of participants
se1c <- sd1c/sqrt(n1c) # standard error
lCI1c <- mean1c - (1.96*se1c) # lower 95% CI
uCI1c <- mean1c + (1.96*se1c) # upper 95% CI

# White Noise Group (2)

mean2c <- mean(data1$claimpercent[data1$cond=="2"], na.rm = TRUE)*100 # mean
sd2c <- sd(data1$claimpercent[data1$cond=="2"], na.rm = TRUE)*100 # SD
n2c <- length(data1$cond[data1$cond=="2" & !is.na(data1$cond)]) # number of participants
se2c <- sd2c/sqrt(n2c) # standard error
lCI2c <- mean2c - (1.96*se2c) # lower 95% CI
uCI2c <- mean2c + (1.96*se2c) #upper 95% CI

# Secular Group (3)

mean3c <- mean(data1$claimpercent[data1$cond=="3"], na.rm = TRUE)*100 # mean
sd3c <- sd(data1$claimpercent[data1$cond=="3"], na.rm = TRUE)*100 # SD
n3c <- length(data1$cond[data1$cond=="3" & !is.na(data1$cond)]) # number of participants
se3c <- sd3c/sqrt(n3c) # standard error
lCI3c <- mean3c - (1.96*se3c) # lower 95% CI
uCI3c <- mean3c + (1.96*se3c) #upper 95% CI

# Religious Group (4)

mean4c <- mean(data1$claimpercent[data1$cond=="4"], na.rm = TRUE)*100 # mean
sd4c <- sd(data1$claimpercent[data1$cond=="4"], na.rm = TRUE)*100 # SD
n4c <- length(data1$cond[data1$cond=="4" & !is.na(data1$cond)]) # number of participants
se4c <- sd4c/sqrt(n4c) # standard error
lCI4c <- mean4c - (1.96*se4c) # lower 95% CI
uCI4c <- mean4c + (1.96*se4c) #upper 95% CI


# Sacredness variable

# White Noise Group (2)

mean2s <- mean(data1$sacred[data1$cond=="2"], na.rm = TRUE) # mean
sd2s <- sd(data1$sacred[data1$cond=="2"], na.rm = TRUE) # SD
n2s <- length(data1$cond[data1$cond=="2" & !is.na(data1$cond)]) # number of participants
se2s <- sd2s/sqrt(n2s) # standard error
lCI2s <- mean2s - (1.96*se2s) # lower 95% CI
uCI2s <- mean2s + (1.96*se2s) #upper 95% CI

# Secular Group (3)

mean3s <- mean(data1$sacred[data1$cond=="3"], na.rm = TRUE) # mean
sd3s <- sd(data1$sacred[data1$cond=="3"], na.rm = TRUE) # SD
n3s <- length(data1$cond[data1$cond=="3" & !is.na(data1$cond)]) # number of participants
se3s <- sd3s/sqrt(n3s) # standard error
lCI3s <- mean3s - (1.96*se3s) # lower 95% CI
uCI3s <- mean3s + (1.96*se3s) #upper 95% CI

# Religious Group (4)

mean4s <- mean(data1$sacred[data1$cond=="4"], na.rm = TRUE) # mean
sd4s <- sd(data1$sacred[data1$cond=="4"], na.rm = TRUE) # SD
n4s <- length(data1$cond[data1$cond=="4" & !is.na(data1$cond)]) # number of participants
se4s <- sd4s/sqrt(n4s) # standard error
lCI4s <- mean4s - (1.96*se4s) # lower 95% CI
uCI4s <- mean4s + (1.96*se4s) #upper 95% CI

# Negativity variable

# White Noise Group (2)

mean2n <- mean(data1$negativity[data1$cond=="2"], na.rm = TRUE) # mean
sd2n <- sd(data1$negativity[data1$cond=="2"], na.rm = TRUE) # SD
n2n <- length(data1$cond[data1$cond=="2" & !is.na(data1$cond)]) # number of participants
se2n <- sd2n/sqrt(n2n) # standard error
lCI2n <- mean2n - (1.96*se2n) # lower 95% CI
uCI2n <- mean2n + (1.96*se2n) #upper 95% CI

# Secular Group (3)

mean3n <- mean(data1$negativity[data1$cond=="3"], na.rm = TRUE) # mean
sd3n <- sd(data1$negativity[data1$cond=="3"], na.rm = TRUE) # SD
n3n <- length(data1$cond[data1$cond=="3" & !is.na(data1$cond)]) # number of participants
se3n <- sd3n/sqrt(n3n) # standard error
lCI3n <- mean3n - (1.96*se3n) # lower 95% CI
uCI3n <- mean3n + (1.96*se3n) #upper 95% CI

# Religious Group (4)

mean4n <- mean(data1$negativity[data1$cond=="4"], na.rm = TRUE) # mean
sd4n <- sd(data1$negativity[data1$cond=="4"], na.rm = TRUE) # SD
n4n <- length(data1$cond[data1$cond=="4" & !is.na(data1$cond)]) # number of participants
se4n <- sd4n/sqrt(n4n) # standard error
lCI4n <- mean4n - (1.96*se4n) # lower 95% CI
uCI4n <- mean4n + (1.96*se4n) #upper 95% CI

# Positivity variable

# White Noise Group (2)

mean2p <- mean(data1$positivity[data1$cond=="2"], na.rm = TRUE) # mean
sd2p <- sd(data1$positivity[data1$cond=="2"], na.rm = TRUE) # SD
n2p <- length(data1$cond[data1$cond=="2" & !is.na(data1$cond)]) # number of participants
se2p <- sd2p/sqrt(n2p) # standard error
lCI2p <- mean2p - (1.96*se2p) # lower 95% CI
uCI2p <- mean2p + (1.96*se2p) #upper 95% CI

# Secular Group (3)

mean3p <- mean(data1$positivity[data1$cond=="3"], na.rm = TRUE) # mean
sd3p <- sd(data1$positivity[data1$cond=="3"], na.rm = TRUE) # SD
n3p <- length(data1$cond[data1$cond=="3" & !is.na(data1$cond)]) # number of participants
se3p <- sd3p/sqrt(n3p) # standard error
lCI3p <- mean3p - (1.96*se3p) # lower 95% CI
uCI3p <- mean3p + (1.96*se3p) #upper 95% CI

# Religious Group (4)

mean4p <- mean(data1$positivity[data1$cond=="4"], na.rm = TRUE) # mean
sd4p <- sd(data1$positivity[data1$cond=="4"], na.rm = TRUE) # SD
n4p <- length(data1$cond[data1$cond=="4" & !is.na(data1$cond)]) # number of participants
se4p <- sd4p/sqrt(n4p) # standard error
lCI4p <- mean4p - (1.96*se4p) # lower 95% CI
uCI4p <- mean4p + (1.96*se4p) #upper 95% CI

# Tempo variable

# White Noise Group (2)

mean2t <- mean(data1$tempo[data1$cond=="2"], na.rm = TRUE) # mean
sd2t <- sd(data1$tempo[data1$cond=="2"], na.rm = TRUE) # SD
n2t <- length(data1$cond[data1$cond=="2" & !is.na(data1$cond)]) # number of participants
se2t <- sd2t/sqrt(n2t) # standard error
lCI2t <- mean2t - (1.96*se2t) # lower 95% CI
uCI2t <- mean2t + (1.96*se2t) #upper 95% CI

# Secular Group (3)

mean3t <- mean(data1$tempo[data1$cond=="3"], na.rm = TRUE) # mean
sd3t <- sd(data1$tempo[data1$cond=="3"], na.rm = TRUE) # SD
n3t <- length(data1$cond[data1$cond=="3" & !is.na(data1$cond)]) # number of participants
se3t <- sd3t/sqrt(n3t) # standard error
lCI3t <- mean3t - (1.96*se3t) # lower 95% CI
uCI3t <- mean3t + (1.96*se3t) #upper 95% CI

# Religious Group (4)

mean4t <- mean(data1$tempo[data1$cond=="4"], na.rm = TRUE) # mean
sd4t <- sd(data1$tempo[data1$cond=="4"], na.rm = TRUE) # SD
n4t <- length(data1$cond[data1$cond=="4" & !is.na(data1$cond)]) # number of participants
se4t <- sd4t/sqrt(n4t) # standard error
lCI4t <- mean4t - (1.96*se4t) # lower 95% CI
uCI4t <- mean4t + (1.96*se4t) #upper 95% CI

# Impact variable

# White Noise Group (2)

mean2i <- mean(data1$impact[data1$cond=="2"], na.rm = TRUE) # mean
sd2i <- sd(data1$impact[data1$cond=="2"], na.rm = TRUE) # SD
n2i <- length(data1$cond[data1$cond=="2" & !is.na(data1$cond)]) # number of participants
se2i <- sd2i/sqrt(n2i) # standard error
lCI2i <- mean2i - (1.96*se2i) # lower 95% CI
uCI2i <- mean2i + (1.96*se2i) #upper 95% CI

# Secular Group (3)

mean3i <- mean(data1$impact[data1$cond=="3"], na.rm = TRUE) # mean
sd3i <- sd(data1$impact[data1$cond=="3"], na.rm = TRUE) # SD
n3i <- length(data1$cond[data1$cond=="3" & !is.na(data1$cond)]) # number of participants
se3i <- sd3i/sqrt(n3i) # standard error
lCI3i <- mean3i - (1.96*se3i) # lower 95% CI
uCI3i <- mean3i + (1.96*se3i) #upper 95% CI

# Religious Group (4)

mean4i <- mean(data1$impact[data1$cond=="4"], na.rm = TRUE) # mean
sd4i <- sd(data1$impact[data1$cond=="4"], na.rm = TRUE) # SD
n4i <- length(data1$cond[data1$cond=="4" & !is.na(data1$cond)]) # number of participants
se4i <- sd4i/sqrt(n4i) # standard error
lCI4i <- mean4i - (1.96*se4i) # lower 95% CI
uCI4i <- mean4i + (1.96*se4i) #upper 95% CI

# computing Cohen's d

# % claimed

#relig vs sec
d1c <- abs((mean4c-mean3c)/sqrt((sd4c^2+sd3c^2)/2))
#relig vs noise
d2c <- abs((mean4c-mean2c)/sqrt((sd4c^2+sd2c^2)/2))
#relig vs control
d3c <- abs((mean4c-mean1c)/sqrt((sd4c^2+sd1c^2)/2))

# sacredness

#relig vs sec
d1s <- abs((mean4s-mean3s)/sqrt((sd4s^2+sd3s^2)/2))
#relig vs noise
d2s <- abs((mean4s-mean2s)/sqrt((sd4s^2+sd2s^2)/2))

# negativity

#relig vs sec
d1n <- abs((mean4n-mean3n)/sqrt((sd4n^2+sd3n^2)/2))
#relig vs noise
d2n <- abs((mean4n-mean2n)/sqrt((sd4n^2+sd2n^2)/2))

# positivity

#relig vs sec
d1p <- abs((mean4p-mean3p)/sqrt((sd4p^2+sd3p^2)/2))
#relig vs noise
d2p <- abs((mean4p-mean2p)/sqrt((sd4p^2+sd2p^2)/2))

# tempo

#relig vs sec
d1t <- abs((mean4t-mean3t)/sqrt((sd4t^2+sd3t^2)/2))
#relig vs noise
d2t <- abs((mean4t-mean2t)/sqrt((sd4t^2+sd2t^2)/2))

# impact

#relig vs sec
d1i <- abs((mean4i-mean3i)/sqrt((sd4i^2+sd3i^2)/2))
#relig vs noise
d2i <- abs((mean4i-mean2i)/sqrt((sd4i^2+sd2i^2)/2))


# Making the tables

# Make religious table
table1 <- tibble(
  characteristics = c("% claimed", "Sacredness", "Negativity", "Positivity", "Tempo", "Impact"),
  M = c(mean4c, mean4s, mean4n, mean4p, mean4t, mean4i),
  SD = c(sd4c, sd4s, sd4n, sd4p, sd4t, sd4i),
  lCI = c(lCI4c, lCI4s, lCI4n, lCI4p, lCI4t, lCI4i),
  uCI = c(uCI4c, uCI4s, uCI4n, uCI4p, uCI4t, uCI4i),
  d = c("-", "-", "-", "-", "-", "-")
) 

table1 %>% mutate_if(is.numeric, ~round(., 2)) %>%
  gt() %>%
  cols_label(characteristics = "Religious") 

# Make secular table
table2 <- tibble(
  characteristics = c("% claimed", "Sacredness", "Negativity", "Positivity", "Tempo", "Impact"),
  M = c(mean3c, mean3s, mean3n, mean3p, mean3t, mean3i),
  SD = c(sd3c, sd3s, sd3n, sd3p, sd3t, sd3i),
  lCI = c(lCI3c, lCI3s, lCI3n, lCI3p, lCI3t, lCI3i),
  uCI = c(uCI3c, uCI3s, uCI3n, uCI3p, uCI3t, uCI3i),
  d = c(d1c, d1s, d1n, d1p, d1t, d1i)
)

table2 %>% mutate_if(is.numeric, ~round(., 2)) %>%
  gt() %>%
  cols_label(characteristics = "Secular")

# Make white noise table
table3 <- tibble(
  characteristics = c("% claimed", "Sacredness", "Negativity", "Positivity", "Tempo", "Impact"),
  M = c(mean2c, mean2s, mean2n, mean2p, mean2t, mean2i),
  SD = c(sd2c, sd2s, sd2n, sd2p, sd2t, sd2i),
  lCI = c(lCI2c, lCI2s, lCI2n, lCI2p, lCI2t, lCI2i),
  uCI = c(uCI2c, uCI2s, uCI2n, uCI2p, uCI2t, uCI2i),
  d = c(d2c, d2s, d2n, d2p, d2t, d2i)
)

table3 %>% mutate_if(is.numeric, ~round(., 2)) %>%
  gt() %>%
  cols_label(characteristics = "White Noise")

# Make control group table
table4 <- tibble(
  characteristics = c("% claimed", "Sacredness", "Negativity", "Positivity", "Tempo", "Impact"),
  M = c(mean1c, NA, NA, NA, NA, NA),
  SD = c(sd1c, NA, NA, NA, NA, NA),
  lCI = c(lCI1c, NA, NA, NA, NA, NA),
  uCI = c(uCI1c, NA, NA, NA, NA, NA),
  d = c(d3c, NA, NA, NA, NA, NA)
)

table4 %>% mutate_if(is.numeric, ~round(., 2)) %>%
  gt() %>%
  cols_label(characteristics = "Control Group")


data_tables <- data.frame(Religious = table1, 
                          Secular = table2,
                          WhiteNoise = table3,
                          Control = table4)

gt_tbl <- gt(data_tables) %>% 
  tab_source_note(
    source_note = "M = Mean; SD = Standard Deviation; CI = 95% Confidence Intervals. Cohen's d represents the effect size of comparisons between musical conditions.") %>%
  tab_spanner(
    label = "Religious (n = 102)",
    columns = c(Religious.M, Religious.SD, Religious.lCI, Religious.uCI, Religious.d)
  ) %>%
  tab_spanner(
    label = "Secular (n = 103)",
    columns = c(Secular.M, Secular.SD, Secular.lCI, Secular.uCI, Secular.d)
  ) %>%
  tab_spanner(
    label = "White Noise (n = 103)",
    columns = c(WhiteNoise.M, WhiteNoise.SD, WhiteNoise.lCI, WhiteNoise.uCI, WhiteNoise.d)
  ) %>%
  tab_spanner(
    label = "Control (n = 100)",
    columns = c(Control.M, Control.SD, Control.lCI, Control.uCI, Control.d)
  ) %>%
  cols_label(Religious.characteristics = "", 
             Religious.M = "M",
             Religious.SD = "SD",
             Religious.lCI = "lCI",
             Religious.uCI = "uCI",
             Religious.d = "d",
             Secular.characteristics = "Secular",
             Secular.M = "M",
             Secular.SD = "SD",
             Secular.lCI = "lCI",
             Secular.uCI = "uCI",
             Secular.d = "d",
             WhiteNoise.characteristics = "White Noise",
             WhiteNoise.M = "M",
             WhiteNoise.SD = "SD",
             WhiteNoise.lCI = "lCI",
             WhiteNoise.uCI = "uCI",
             WhiteNoise.d = "d",
             Control.characteristics = "Control",
             Control.M = "M",
             Control.SD = "SD",
             Control.lCI = "lCI",
             Control.uCI = "uCI",
             Control.d = "d") %>%
  
  fmt_number(
    columns = c(Religious.M, Religious.SD, Religious.lCI, Religious.uCI, Secular.M, Secular.SD, Secular.lCI, Secular.uCI, Secular.d, WhiteNoise.M, WhiteNoise.SD, WhiteNoise.lCI, WhiteNoise.uCI, WhiteNoise.d, Control.M, Control.SD, Control.lCI, Control.uCI, Control.d),
    decimals = 2,
    use_seps = FALSE
  ) %>% 
  
  cols_hide(
    columns = c(Secular.characteristics, WhiteNoise.characteristics, Control.characteristics))

gt_tbl %>% fmt_missing(
  columns = everything(),
  missing_text = "-"
)
