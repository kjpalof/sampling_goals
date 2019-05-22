# K.Palof ADF&G            udpated 2-1-18/ 5-21-19
# objective: determine how many trips to sample for lengths in the crab fisheries (golden, tanner, 
#         and dungeness - seperately) to be able to tell if the % of recruits changes from year to year.
# statistical objective: 
# power analysis to determine the number of trips to sample from each management area
#         to detect change in % recruits from port sampling/dockside data


### Dungeness Crab --------------------

#####Load Packages ---------------------------------
library(tidyverse)
library(pwr)
library(broom)
library(readxl)

#####Load Data ---------------------------------------------------
# change input file for each
# pull dockside data from ALEX 
dat <- read_excel("./data/dunge_14_18_dockside.xlsx", sheet = "AlexData")


##### Data clean  up ---------------



#### Data manipulation ---------------

### Summarise data by trip for the fishery, keep season, location (I_FISHERY), and trip # 
#unique(dat$SEASON) use season NOT year
# all recruit classes by trip
dat %>%
  group_by(SEASON, I_FISHERY, DISTRICT, TRIP_NO, RECRUIT_STATUS) %>%
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
  filter(SEASON == "Apr2018 - Mar19")

# Power analysis --------------
# t-test - one sample - example
tidy(pwr.t.test( d= (0.10/0.164196), sig.level = 0.05, power = 0.85, type = "one.sample", alternative = "two.sided"))

# d is difference to detect which is the difference / S.D.


# using dplyr and broom 
# !! load function below first !!!
recruits %>% na.omit() %>% 
  rowwise %>% 
  mutate(x0.10 = crab_power(0.10, sd_percentR), x0.08 = crab_power(0.08, sd_percentR), 
         x0.06=crab_power(0.06, sd_percentR)) %>% 
  mutate(year = as.factor(SEASON) )-> recruits_pow
# writing a function
crab_power <- function(a, b){
  step1 <- pwr.t.test(d=(a/b), sig.level = 0.05, power = 0.85, type = "one.sample", alternative = "two.sided")
  step1$n
}
crab_power(0.10, recruits$sd_percentR[2])


# power using average of 5 years 
# this data set only has the last 5 years so no need for filtering.
unique(recruits_pow$SEASON)
#years3 <- c("Sep2014 - Aug15", "Sep2015 - Aug16", "Sep2016 - Aug17")

recruits_pow %>%
  as.data.frame() %>% 
  group_by(I_FISHERY) %>%
  summarise (mean(x0.10), mean(x0.08), mean (x0.06))->mean_recuits_pow

# use the number of trips/landings to sample using a difference to detect of 0.10

#save 
write.csv(recruits_pow, file = "./output/DC_recruit_power_19.csv")
write.csv(mean_recuits_pow, file = "./output/DC_last5years_sample_size_19.csv")
