# sampling_goals
power analysis to determine sampling goals for crab fisheries using % recruit per landing.


# Tanner crab results
(updated 2024)
Two files are produced:
1) "TC_last3_sample_size_power_2024.csv"
-- has the mean of the last 3 years for estimated sample size depending on the difference we would like to detect for changes in recruits from port sampling data 

2) "TC_recruit_power_2024.csv"
-- has the output by year, season, and fishery area - includes:
column names:
ntrip - number of trips observed NOT total number of trips but the number sampled by port sampling
mean_percentR - mean percent recruits by trip
sd_percentR - standard deviation of this mean
x0.15 - the "difference to detect" for changes in % recruit by sampling area. If this is "0.15" this means we can detect a 15% change by year in each sampling area with statistical significance of alpha 0.05 
x0.10, etc.

-- the other values represent increased statistical power. Typically we had been using 10% but I've provided 15% for reference. 