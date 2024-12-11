library(pdist)

## function to shift points to avoid overlap when that is easy.
## Not optimized for speed, may be slow for large datasets. 
adjust_points = function(mat.tot, # matrix with two columns, one for each variable
                         thresh = 0.01 # threshold distance (in scaled space. thresh = 0.01 is 1% of window)
                         ){
  if(any(is.na(mat.tot))){
    cli::cli_abort("Function does not currently support NAs. Clean before use.")
  }
  mat.old = mat.tot
  
  #record NAs
  # ind.nas = which(apply(mat.tot, 1, function(x){any(is.na(x))}))
  #remove NAs
  mat.tot = na.omit(mat.tot)
  #record ranges
  mat.key = apply(mat.tot, 2, range)
  
  ## scale matrix
  mat.scaled = mat.tot
  mat.scaled[,1] = mat.scaled[,1]-mat.key[1,1]
  mat.scaled[,1] = mat.scaled[,1]/max(mat.scaled[,1])
  
  mat.scaled[,2] = mat.scaled[,2]-mat.key[1,2]
  mat.scaled[,2] = mat.scaled[,2]/max(mat.scaled[,2])
  
  ##generate additions for testpoints:
  theta = seq(0, 2*pi, length = 20)
  add.x = c(cos(theta)*1.01*thresh, cos(theta)*1.3*thresh)
  add.y = c(sin(theta)*1.01*thresh, sin(theta)*1.3*thresh)
  
  flag = FALSE
  
  prob.tally = 0
  for(i in (nrow(mat.scaled)-1):1){
    cur.prob = mat.scaled[i,]
    comp = as.matrix(pdist(mat.scaled[i,],
                           mat.scaled[(i+1):nrow(mat.scaled),]))
    if(any(comp<thresh)){
      prob.tally = prob.tally+1
      ## Note that the i+1 needs to turn to i when not using while loop
      exist.prob = mat.scaled[c(rep(FALSE, i), comp<thresh),]
      
      ## quick check on points "threshold + 10%" away to see if any work better
      cur.test = cbind(add.x+cur.prob[1],
                       add.y+cur.prob[2])
      
      ind.good = which(apply(as.matrix(pdist(cur.test, mat.scaled[(i+1):nrow(mat.scaled),])),
                             1, function(x){all(x>thresh)}))
      if(length(ind.good)>0){
        mat.scaled[i,] = cur.test[sample(ind.good,1),]
      }
    }
  }
  
  ## Unscale
  mat.new = mat.scaled
  mat.new[,1] = mat.new[,1]*apply(mat.key, 2, diff)[1]
  mat.new[,1] = mat.new[,1] + mat.key[1,1]
  
  mat.new[,2] = mat.new[,2]*apply(mat.key, 2, diff)[2]
  mat.new[,2] = mat.new[,2] + mat.key[1,2]
  return(mat.new)
}

adjust_points(mat.tot = rbind(dat.trends$lgabund.est, dat.trends$const.est))
