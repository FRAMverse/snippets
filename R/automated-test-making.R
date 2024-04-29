## preliminary functions
## For use in populating test_that when working with a function that outputs 
## a dataframe. These functions take a dataframe with "correct" aspects (dimensions, etc)
## and output R code for test_that that tests equality of these aspects, presuming
## the object `res` is in the testthat equivalence chunk thing, and that's the object
## we want to check
## 
## Note that these may produce tests we DON'T want (e.g., maybe the rownumber shouldn't be the same)
## 
## Wishlist: 
##    Individual columns contain no NAs
##    Individual columns contain all the same unique values as the provided df (or no values outside of the values )
##          in the provided df)
##    Individual column values are within certain ranges
## 
## It was suggested this could be provided in the testthat setup files:
##   https://testthat.r-lib.org/articles/special-files.html

testmaker_df_class = function(x){
  res = lapply(x, class)
  cat(paste0('expect_equal(class(res$', names(res), ', "', unlist(res), '"))'), sep = "\n")
}
testmaker_df_class(temp)


testmaker_df_dim = function(x){
  res = dim(x)
  cat(paste0('expect_equal(', c("nrow", "ncol"), '(res), ', res, '))'), sep = "\n")
}
testmaker_df_dim(temp)

testmaker_df_names = function(x){
  cat(paste0('expect_equal(names(res), c(', paste0(paste0('"', names(x), '"'), collapse = ", "), '))'), sep = "\n")
}

temp = lib_summary()
testmaker_df_dim(temp)
testmaker_df_names(temp)
testmaker_df_class(temp)
