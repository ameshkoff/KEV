# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #


# covariation matrix ------------------------------ #

ab.cov <- function(ab.err
                   , cnst.m
                   , cnst.tune
                   , dt.ab.err.m, dt.coef, dt.coef.m, dt.conc.m, part.eq, reac.nm
                   , eq.thr.type, eq.threshold
                   , method = c("lm", "basic wls")
                   , ab.threshold) {

  cnst.tune.nm <- which(colnames(dt.res.m) %in% cnst.tune)
  
  fr.degr <- nrow(dt.res.m) - length(cnst.tune.nm)
  wght <- 1 / (as.vector(dt.ab.err.m) ^ 2)
  
  cnst.grid <- cnst.m
  
  for (i in 1:length(cnst.tune.nm)) {
    
    cnst.grid <- cbind(cnst.grid, cnst.m, cnst.m)

    cnst.grid[cnst.tune.nm[i], i*2-1] <- cnst.grid[cnst.tune.nm[i], i*2-1] - ab.threshold
    cnst.grid[cnst.tune.nm[i], i*2] <- cnst.grid[cnst.tune.nm[i], i*2] + ab.threshold
    
  }
  
  cnst.grid <- cnst.grid[, 1:(ncol(cnst.grid) - 1)]
  
  dt.ab.diff <- data.table()
  
  for (i in 1:ncol(cnst.grid)) {
    
    rtrn <- molar.ext.wrapper(cnst.grid[, i]
                              , cnst.tune.nm
                              , dt.coef, dt.coef.m, dt.conc.m, part.eq, reac.nm
                              , eq.thr.type, eq.threshold
                              , method)
    
    rtrn <- as.vector(as.matrix(rtrn$dt.ab.calc))
    dt.ab.diff <- rbind(dt.ab.diff, as.data.table(as.list(rtrn)))
    
  }
  
  dt.ab.diff <- data.table(t(dt.ab.diff))
  
  i <- 1:ncol(cnst.grid)
  
  dt.ab.diff <- as.matrix(dt.ab.diff)[, which(i %% 2 == 0)] - as.matrix(dt.ab.diff)[, which(i %% 2 == 1)]
  dt.ab.diff <- dt.ab.diff / (2 * log(exp(ab.threshold), 10))
  
  cov.m <- (ab.err / fr.degr) * ginv(t(dt.ab.diff) %*% diag(wght) %*% dt.ab.diff, tol = 0)

  cov.m

}






