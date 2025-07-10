## We frequently want to put confidence intervals on test fishing data
## When we're looking at binomial data (e.g. legal mark rate), the best approach is logistic regression
## But when we have more than 2 classes (e.g., we want the proportion of fish that are legal marked,
## legal unmarked, sublegal marked, sublegal unmarked), the best way to get that confidence intervals is
## to treat the data as multinomial. (We *could* also treat that as 4 separate binomial models,
## but that does assume the four rates are independent of each other. See `estimating-mark-rate-cis.R` for that.)
## 
## Here are two ways to estimate confidence intervals for simple pooled data 
##   (e.g. when we're not trying to model structured data)
##   a) bootstrapping / resampling
##   b) the MultinomCI function from the `DescTools` function
##   


## getting some example test fishing data: ---------------------------------
library(tidymodels)
library(tidyverse)
# pssp::pssp_refresh_data()
tf <- pssp::tf_pull('05', '2025-07-01', '2025-07-09')



## for now, focus on a 2-class situation: mark rate
dat = pssp::tf_lm_summary(tf)


###########################################################################
## Bootstrapping approach:
###########################################################################

bootstrap_multinomial = function(dat, ## named vector or named tibble, with values equal to the # of observations in each class 
                                 Nsim = 100000 ## number of simulations. Function is very fast, so large is fine.
){
  
  sim_mat = matrix(sample(names(dat), 
                          size = sum(dat)*Nsim,
                          replace = TRUE,
                          prob = as.numeric(dat)),
                   nrow = Nsim)
  probs_mat = matrix(-99, nrow = nrow(sim_mat), ncol = length(dat))
  colnames(probs_mat) = names(dat)
  for(i in 1:length(dat)){
    probs_mat[, i] = rowMeans(sim_mat == names(dat)[[i]])
  }
  
  summary_mat = apply(probs_mat, 2, quantile, prob = c(0.025, 0.975))
  
  return(list(simulation_matrix = probs_mat, confidence_interval = summary_mat))
}

## example: 
out = pssp::tf_lm_summary(tf) |> 
  bootstrap_multinomial()
out$confidence_interval

######################################################################
## DescTools
######################################################################
dat = pssp::tf_lm_summary(tf)
dat.vec = as.numeric(dat)
names(dat.vec) = names(dat)
DescTools::MultinomCI(dat.vec)
