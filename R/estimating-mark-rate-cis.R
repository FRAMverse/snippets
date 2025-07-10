## Frequently we need to estimate the mark rates or legal marked rates from test fishing data.
##   and we often want confidenece intervals on those estimates. 
## When we pool the data together so that we just have counts of caught fish that
##   were or were not in our category of interest, we can use the following function
##   to estimate those CIs. This uses likelihood profiling, which is approximately the 
##   same approach that is used by default when estimating CIs from a fitted logistic 
##   regression model, except that likelihood profiling correctly handles the edge cases
##   when 0 individuals fell into the category of interest (or 0 individuals did not
##   fall into the category of interest)

library(tidyverse)
library(binom)


## Function ---------------------------------------
estimate_rate_cis = function(sampled_trues, #number of fish sampled that fall in the category of interest (e.g. UM, or legal UM)
                    sampled_falses){ #number of fish sampled that don't fall in the category of interest
  res = binom::binom.confint(sampled_trues, sampled_trues + sampled_falses) |> 
    filter(method == "profile") |> 
    select(estimated_rate = mean, ci_95_lower = lower, ci_95_upper = upper, confidence_method = method)
  return(res)
}

## example: --------------------------------
library(pssp)

## pull test fishing data
tf <- pssp::tf_pull('05', '2025-07-01', '2025-07-09')
## collapse into count of fish in each class
dat = pssp::tf_lm_summary(tf)
dat

## what is our estimated legal marked rate?
estimate_rate_cis(dat$Legal_AD, # number of legal marked fish caught
         sum(dat)-dat$Legal_AD) #number of fish caught that were not legal marked
