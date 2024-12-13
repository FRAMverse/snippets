## Minimal working example for common use case for me: inside of a parallelized loop, generate some data, then calculate summary statistics for that data. We want to save both, and this allows that to happen rapidly.

library(foreach)
library(doParallel)
library(data.table)

threads = 5
c1 = makeCluster(threads)
registerDoParallel(c1)

Nsim = 10
res = foreach(i = 1:Nsim) %dopar% {
  res.cur = data.frame(x = rnorm(1), y  = rnorm(1))
  res.dat = matrix(rnorm(30), ncol = 3)
  return(list(res = res.cur, data = res.dat))
}
stopCluster(c1)
res.df = as.dataframe(data.table::rbindlist(lapply(res, "[[", 1), idcol = "simid"))
data.sim = data.table::rbindlist(lapply(res, "[[", 2), idcol = "simid")
