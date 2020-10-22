# All contents come from
# R0 packages - cRAN and github
# earlyR

# two excellent packages 

# Getting ready

library(tidyverse)

# Reference
# http://www.repidemicsconsortium.org/earlyR/

# Early R Package
library(earlyR)
library(incidence)

# 1. make a fake dataset
# confirmed cases with the following symptom onset dates

onset <- as.Date(c("2017-02-04", "2017-02-12", "2017-02-15",
                   "2017-02-23", "2017-03-01", "2017-03-01",
                   "2017-03-02", "2017-03-03", "2017-03-03"))    

# 2. set the date with NO cases
# generate incidence

today <- as.Date("2017-03-21")
i <- incidence(onset, last_date = today)

# 3. To estimate R
# need estimates of the mean and standard deviation of the serial interval,

mu <- 15.3 # mean in days days
sigma <- 9.3 # standard deviation in days

# 4. Estimate R

res <- get_R(i, si_mean = mu, si_sd = sigma)
res

# 5. distribution of likely values of R, 
# and the Maximum-Likelihood (ML) estimation
plot(res)


# R0 package

# 5 methods to estimate epid parameters
# the incubation period, i.e. time between infection and symptoms;
# the serial interval, i.e. time between symptoms onset in primary and secondary cases; 
# and the initial reproduction ratio, i.e. the average number of secondary cases per primary case.

# loads library
library(R0) 

# epidemic curve can be input as a list of dates
epid <- c("2012-01-01", "2012-01-02", "2012-01-02", "2012-01-03")
# or as incidence counts
epid.count <- c(1,2,4,8)

# create generation time : gamma distribution, 
# with mean 2.6 time units and standard deviation 1 time unit
GT.flu <- generation.time("gamma", c(2.6, 1))

# loads example dataset
data(Germany.1918)
res.R <- estimate.R(Germany.1918, GT = GT.flu, 
                    methods = c("EG","ML","SB","TD"))
# applies methods EG, ML, SB, TD to the dataset

plot(res.R) 
# diplays results

plotfit(res.R)
# displays fit to the epidemic curve
# sensitivity analysis according to choice of time window for exponential growth

# EpiEstim package



