#K.Palof ADF&G
#objective: determin how many trips to sample for lenghts in the crab fisheries (golden, tanner, 
#         and dungeness - seperately) to be able to tell if the % of recruits changes from year to year.
#statistical objective: 
#power analysis to determine the number of trips to sample from each management area
#         to detect

rm(list = ls()) # clear workspace since data frames have same names
#####Load Packages ---------------------------------
library(tidyverse)
library(pwr)
library(broom)

#####Load Data ---------------------------------------------------
# change input file for each
dat <- read.csv("./data/gkc_dockside_13_16.csv")

##### Data manipulation ---------------
### Summarise data by trip for the fishery, keep season, location (I_FISHERY), and trip # 
#unique(dat$SEASON) use season NOT year
# all recruit classes by trip
dat %>%
  group_by(SEASON, I_FISHERY, TRIP_NO, RECRUIT_STATUS) %>%
  summarise(N = n()) -> by_trip
# total crab by trip
by_trip %>%
  group_by(SEASON, I_FISHERY, TRIP_NO)%>%
  summarise(tot_crab = sum(N)) -> total_by_trip
# % sampled of each recruit class by trip
by_trip %>%
  left_join(total_by_trip) %>%
  mutate(percent = N/tot_crab) -> by_trip2
# just recruits by trip
by_trip2 %>%
  filter(RECRUIT_STATUS == "Recruit") ->recruit_by_trip2
# summarize by season and fishery area
#       these match those calculated in .JMP files in previous years
recruit_by_trip2 %>%
  group_by(SEASON, I_FISHERY) %>%
  summarise(ntrip = n(), mean_percentR = mean(percent), sd_percentR = sd(percent)) -> recruits

recruits %>%
  filter(SEASON == "Oct2015 - Sep16")

# Power analysis 
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
years3 <- c("Oct2013 - Sep14", "Oct2014 - Sep15", "Oct2015 - Sep16")
recruits_pow %>%
  filter(SEASON %in% years3) %>%
  group_by(I_FISHERY) %>%
  summarise (mean(x0.10), mean(x0.08), mean (x0.06))->last3_recuits_pow

#save 
write.csv(recruits_pow, file = "./output/GKCrecruit_power.csv")

## crabs sampled per trip review ---------------------
# currently set at 50, is there a minimium where the trip is not useful?
ggplot(recruit_by_trip2, aes(tot_crab, percent)) +geom_point() +geom_smooth(method = "lm")

# remove sample sizes less than 25 and see if results are different
recruit_by_trip2 %>%
  filter(tot_crab > 20) %>% 
  group_by(SEASON, I_FISHERY) %>%
  summarise(ntrip = n(), mean_percentR = mean(percent), sd_percentR = sd(percent)) -> recruits.20

diff <- recruits$mean_percentR - recruits.20$mean_percentR
