# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #



# solution evaluator -------------------------------------- #

newton.evaluator <- function(cnst.m, dt.coef.m, dt.conc.in, dt.conc.out, part.eq, thr.type, threshold, max.it = 1000) {
  
  accr <- c()
  conv <- 0
  
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
    
    # correct for input equality concentrations
    if (length(part.eq) > 0)
      err.v[part.eq] <- as.numeric(0)
    
    # if does not converge
    if (length(err.v[is.na(err.v) | is.infinite(err.v)]) > 0) {
      
      conv <- -1
      break
      
    }
    
    # step
    tmp <- exp(log(dt.conc.out) - 1 * ginv(jc, tol = 0) %*% err.v)
    
    # check accuracy
    if (thr.type[1] == "rel") {
      
      accr.tmp <- dt.conc.out / tmp
      accr.tmp[is.infinite(accr.tmp)] <- 0
      
      accr <- c(accr, mean(abs(1 - accr.tmp)))
      
    } else if (thr.type[1] == "abs") {
      
      accr.tmp <- tmp
      accr.tmp[accr.tmp == 0] <- 1e-100
      
      accr <- c(accr, mean(abs(log(dt.conc.out) - log(accr.tmp))))
      
    }
    
    # prepare result
    dt.conc.out <- tmp

    if (iter > 1) {
      
      if (accr[iter] >= accr[iter - 1] | is.infinite(accr[iter]) | is.infinite(accr[iter - 1])) {
        
        conv <- conv + 1
        
      } else {
        
        conv <- 0
      }
      
    }
    
    if (accr[iter] < threshold | (conv >= 5 & iter > 100))
      break

    # print steps for longer evaluation
    if (iter %% 1000 == 0) print(iter)
    
    # iterator
    iter <- iter + 1
    
  }
  
  list(out = conc.prod.res, iter = iter, conv.code = conv)
  
}


# loop for every solution --------------------------- #

newton.wrapper <- function(cnst.m, dt.coef.m, dt.conc.m, part.eq, reac.nm, thr.type, threshold) {
  
  dt.res.m <- matrix(ncol = reac.nm, nrow = 0)
  
  max.it <- 1000
  tr.nm <- max.it * .1
  
  for (i in 1:nrow(dt.conc.m)) {
    
    dt.conc.in <- copy(dt.conc.m[i, ])
    
    dt.conc.out.init <- copy(dt.conc.in)
    
    # N tries to converge
    for (j in 1:tr.nm) {
      
      if (j  > 1)
        dt.conc.out.init <- copy(dt.conc.in) * runif(1, 1e-3, .999)
      
      out <- newton.evaluator(cnst.m, dt.coef.m, dt.conc.in, dt.conc.out.init, part.eq, thr.type, threshold, max.it)
      
      # browser()
      if (out[["conv.code"]] == 0)
        break

    }
    
    rtrn <- out[["out"]]
    
    if (out[["conv.code"]] != 0)
      rtrn[, 1] <- NA
      
    dt.res.m <- rbind(dt.res.m, as.numeric(rtrn))

  }
  
  dt.res.m
  
}












