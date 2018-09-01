# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #



# solution evaluator -------------------------------------- #

newton.evaluator <- function(cnst.m, dt.coef.m, dt.conc.in, part.eq, thr.type, threshold, max.it = 1000) {
  
  dt.conc.out <- copy(dt.conc.in)
  
  for (iter in 1:max.it) {
    
    if (length(part.eq) > 0)
      dt.conc.out[part.eq] <- dt.conc.in[part.eq]
    
    # base concentrations equation
    conc.base.res <- t(dt.coef.m) %*% exp(cnst.m + dt.coef.m %*% log(dt.conc.out))
    
    # product concentrations equation
    conc.prod.res <- exp(cnst.m + dt.coef.m %*% log(dt.conc.out))
    
    # jacobian matrix
    jc <- t(dt.coef.m) %*% (dt.coef.m * as.vector(conc.prod.res))
    
    # error vector
    err.v <- t(dt.coef.m) %*% conc.prod.res - dt.conc.in
    
    if (length(part.eq) > 0)
      err.v[part.eq] <- as.numeric(0)
    
    # step
    tmp <- exp(log(dt.conc.out) - 1 * ginv(jc, tol = 0) %*% err.v)
    
    # check accuracy
    if (thr.type[1] == "rel") {
      
      accr <- mean(abs(1 - dt.conc.out / tmp))
      
    } else if (thr.type[1] == "abs") {
      
      accr <- mean(abs(log(dt.conc.out) - log(tmp)))
      
    }
    
    dt.conc.out <- tmp
    
    if (accr < threshold) {
      
      break
      
    }
    
    # print steps for longer evaluation
    if (iter %% 100 == 0) print(iter)
    
    # iterator
    iter <- iter + 1
    
  }
  
  list(out = conc.prod.res, iter = iter)
  
}


# loop for every solution --------------------------- #

newton.wrapper <- function(cnst.m, dt.coef.m, dt.conc.m, part.eq, reac.nm, thr.type, threshold) {
  
  dt.res.m <- matrix(ncol = reac.nm, nrow = 0)
  
  for (i in 1:nrow(dt.conc.m)) {
    
    dt.conc.in <- copy(dt.conc.m[i, ])
    out <- newton.evaluator(cnst.m, dt.coef.m, dt.conc.in, part.eq, thr.type, threshold)
    
    dt.res.m <- rbind(dt.res.m, as.numeric(out[[1]]))
    
  }
  
  dt.res.m
  
}












