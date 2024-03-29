#K.Palof ADF&G                updated for 2018 goals 2-1-18 / 2-1-2021 updates for 2021
# 1/29/24
#objective: determine how many trips to sample for lenghts in the crab fisheries (golden, tanner, 
#         and dungeness - seperately) to be able to tell if the % of recruits changes from year to year.
#statistical objective: 
#power analysis to determine the number of trips to sample from each management area
#         to detect

# Data - request to Zane Chapman

#rm(list = ls()) # clear workspace since data frames have same names
#####Load Packages ---------------------------------
library(tidyverse)
library(pwr)
library(broom)

cur_yr <- 2024 # update with new analysis

#####Load Data ---------------------------------------------------
# change input file for each
dat1 <- read.csv("./data/GKC Port Sampling 2020-2023_2.csv")
#dat2 <- read.csv("./data/gkcdockside_17.csv")

##### Data manipulation ---------------
# add 2017 data - specific to 2018 goals 
#dat1 %>% bind_rows(dat2) -> dat3
dat1 -> dat3 # for simplicity due to additions that were made in the past.

### Summarise data by trip for the fishery, keep season, location (I_FISHERY), and trip # 
#unique(dat$SEASON) use season NOT year
# all recruit classes by trip
dat3 %>%
  #group_by(YEAR, SEASON, I_FISHERY, TRIP_NO, RECRUIT_STATUS) %>% 
  group_by(Year, Season, Invertebrate.Fishery, Trip.Number, Recruit.Status) %>% 
  summarise(N = n()) -> by_trip
# total crab by trip
by_trip %>%
  #group_by(YEAR, SEASON, I_FISHERY, TRIP_NO)%>%
  group_by(Year, Season, Invertebrate.Fishery, Trip.Number) %>% 
  summarise(tot_crab = sum(N)) -> total_by_trip
# % sampled of each recruit class by trip
by_trip %>%
  left_join(total_by_trip) %>%
  mutate(percent = N/tot_crab) -> by_trip2
# just recruits by trip
by_trip2 %>%
  filter(Recruit.Status == "Recruit") ->recruit_by_trip2
# summarize by season and fishery area
#       these match those calculated in .JMP files in previous years
recruit_by_trip2 %>%
  #group_by(Year, SEASON, I_FISHERY) %>%
  group_by(Year, Season, Invertebrate.Fishery) %>% 
  summarise(ntrip = n(), mean_percentR = mean(percent), sd_percentR = sd(percent)) -> recruits

#recruits %>%
#  filter(SEASON == "Oct2016 - Sep17")

## Power analysis ----
# t-test - one sample
tidy(pwr.t.test( d= (0.10/0.164196), sig.level = 0.05, power = 0.85, type = "one.sample", alternative = "two.sided"))

# d is difference to detect which is the difference / S.D.

# using dplyr and broom
recruits %>% na.omit() %>% rowwise %>% 
  mutate(x0.10 = crab_power(0.10, sd_percentR), x0.08 = crab_power(0.08, sd_percentR), 
         x0.06=crab_power(0.06, sd_percentR))-> recruits_pow
# writing a function
crab_power <- function(a, b){
  step1 <- pwr.t.test(d=(a/b), sig.level = 0.05, power = 0.85, type = "one.sample", alternative = "two.sided")
  step1$n
}
crab_power(0.10, recruits$sd_percentR[1])



  
# power using average of last 3 years
#years3 <- c("Oct2014 - Sep15", "Oct2015 - Sep16", "Oct2016 - Sep17")
#years3 <- c("Oct2017 - Sep18", "Oct2018 - Sep19", "Oct2019 - Sep20")
recruits_pow %>%
  #filter(SEASON %in% years3) %>%
  group_by(Invertebrate.Fishery) %>%
  summarise (mean(x0.10), mean(x0.08), mean (x0.06))->last3_recuits_pow

#save 
write.csv(recruits_pow, file = paste0("./output/GKCrecruit_power_", cur_yr, ".csv"))
write.csv(last3_recuits_pow, file = paste0("./output/GKC_last3_power_", cur_yr, ".csv"))
## crabs sampled per trip review ---------------------
# currently set at 50, is there a minimium where the trip is not useful?
ggplot(recruit_by_trip2, aes(tot_crab, percent)) +geom_point() +geom_smooth(method = "lm")

# --------------------- side analyses ----------------
# remove sample sizes less than 25 and see if results are different
recruit_by_trip2 %>%
  filter(tot_crab > 20) %>% 
  group_by(SEASON, I_FISHERY) %>%
  summarise(ntrip = n(), mean_percentR = mean(percent), sd_percentR = sd(percent)) -> recruits.20

diff <- recruits$mean_percentR - recruits.20$mean_percentR


## percent recruit with SD for graphing -------
head(recruits) # see line 45 for code to create this 

#only East Central
recruits %>% 
  filter(I_FISHERY == "East Central GKC") -> recruits_EC
ggplot(recruits_EC, aes(YEAR, mean_percentR)) + 
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_percentR - sd_percentR, ymax = mean_percentR + sd_percentR), 
                width = 0.2, position = position_dodge(0.05))
