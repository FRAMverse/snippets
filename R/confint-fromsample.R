## calculate confidence intervals of binomial data from the proportion of successes and the total number of trials.
## Common use case: Our test fishing found a mark rate of 0.7 from 26 total fish. What's our confidence interval for that?
## Can be used for other classifications of fish as long as there are only 2 classes (e.g. "is legal size" vs "not legal size" or
## "is unmarked natural" vs "is not unmarked natural")
## 
## Primarily uses the assymptotic method to calculate confidence intervals in logit space and then transforms them back.
## Transformation of the standard error is based on the delta method.
## In cases in which p = 0 or p = 1 (e.g., all fish were marked or all fish were unmarked), assymptotic methods fail, and this
## uses likelihood profiling method instead. It was not obvious how to extend likelihood profiling to handle cases in which the level was 
## not 0.95, so in those cases the function gives a warning and returns the (meaningless) assymptotic estimate.
## 
## The function is vectorized, and can be used to calculate the CIs for an entire dataframe of fish data at once.
## 
confint_fromsample = function(p, #sample probability of marked (or other status of interest)
                              n, #total number of samples
                              level = 0.95 #confidence level for the confidence intervals. Default is 95% confidence intervals.
){
  ## input error checking
  p.clean = p[!is.na(p)]
  if(any(p.clean<0 | p.clean>1)){
    cli::cli_abort("`p` is a probability and must be between 0 and 1")
  }
  if(any(n %% 1 !=0 | n < 0)){
    cli::cli_abort("`n` must be a positive integer")
  }
  if(length(p) != length (n)){
    cli::cli_abort("`p` and `n` must have the same length.")
  }
  
  ## standard error for P
  se = sqrt((p)*(1-p)/(n-1))
  ## transform p and se into logit space
  ## The transformation of the se relies on the delta method:
  ## https://www.proteus.co.nz/news-tips-and-tricks/calculating-standard-errors-for-logistic-regressionlogit-link-using-the-delta-method
  
  beta = log(p/(1-p))
  se.beta = se/(p * (1-p))
  
  ## 95% confidence interval = mean +/- 1.96 * standard error
  multiplier = qnorm(1-(1-level)/2)
  conf = cbind(beta - multiplier * se.beta, beta + multiplier* se.beta)
  res = exp(conf)/(1+exp(conf))
  
  ## using likelihood profiling to handle cases when p = 0 or 1. 
  if(any( p == 0 | p == 1)){
    if(level != 0.95){
      cli::cli_alert_warning("Extreme values detected (p = 0 or p = 1). Current methods only calculate CIs correctly in these cases when level = 0.95")
    }else{
      ## NOT vectorized yet
      ind.do = which(p == 0 | p == 1)
      phats = seq(0.001, 0.999, by = 0.001)
      for(ind in ind.do){
        log.liks = log(dbinom(p[ind] * n[ind], prob = phats, size = n[ind]))
        conf = range(phats[log.liks - max(log.liks) > -2])
        res[ind,] = c(conf[1], conf[2])
      }
    }
  }
  ## transform confidence interval back from logit space into normal space
  
  colnames(res) = c("ci.low", "ci.high")
  return(res)
}


### Examples -----------

## works for individual cases
confint_fromsample(p = .7, n = 26)

## Can give vectors of p and n to provide multiple confidence intervals.
confint_fromsample(p = c(0, 0.7), n = c(26, 26))

library(tidyverse)
## using some of our catch data as an example
raw = read_csv("C:/Users/ME/Downloads/ocean.csv")
dat = raw |> 
  mutate(n = marked_encounters + unmarked_encounters,
         p = marked_encounters / n
  ) |> 
  mutate(conf.limits = confint_fromsample(p, n))
