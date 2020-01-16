# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #


# covariation matrix ----------------------------------------- #

emf.cov <- function(emf.err
                   , cnst.m
                   , cnst.tune.nm
                   , dt.coef, dt.coef.m, dt.conc.m, part.eq, reac.nm
                   , dt.emf.m, dt.emf.err.m, dt.params.m
                   , eq.thr.type, eq.threshold
                   , method = c("lm", "basic wls")
                   , emf.threshold) {
  
  fr.degr <- nrow(dt.emf.m) * ncol(dt.emf.err.m) - length(cnst.tune.nm)
  wght <- sum(as.vector(dt.emf.err.m) ^ 2) / ((as.vector(dt.emf.err.m) ^ 2) * length(as.vector(dt.emf.err.m)))
  
  cnst.grid <- cnst.m
  
  for (i in 1:length(cnst.tune.nm)) {
    
    cnst.grid <- cbind(cnst.grid, cnst.m, cnst.m)
    
    cnst.grid[cnst.tune.nm[i], i*2-1] <- cnst.grid[cnst.tune.nm[i], i*2-1] - emf.threshold
    cnst.grid[cnst.tune.nm[i], i*2] <- cnst.grid[cnst.tune.nm[i], i*2] + emf.threshold
    
  }
  
  cnst.grid <- cnst.grid[, 1:(ncol(cnst.grid) - 1)]
  
  dt.emf.diff <- data.table()
  err <- numeric()
  
  for (i in 1:ncol(cnst.grid)) {
    
    rtrn <- emf.params.wrapper(cnst.grid[, i]
                              , cnst.tune.nm
                              , dt.coef, dt.coef.m, dt.conc.m, part.eq, reac.nm
                              , dt.emf.m, dt.emf.err.m, dt.params.m
                              , eq.thr.type, eq.threshold
                              , method)
    
    err <- c(err, rtrn$err)
    rtrn <- as.vector(as.matrix(rtrn$dt.emf.calc))
    dt.emf.diff <- rbind(dt.emf.diff, as.data.table(as.list(rtrn)))
    
  }
  
  dt.emf.diff <- data.table(t(dt.emf.diff))
  
  i <- 1:ncol(cnst.grid)
  
  dt.emf.diff <- as.matrix(dt.emf.diff)[, which(i %% 2 == 0)] - as.matrix(dt.emf.diff)[, which(i %% 2 == 1)]
  dt.emf.diff <- dt.emf.diff / (2 * log(exp(emf.threshold), 10))
  
  err.diff <- err[which(i %% 2 == 0)] - err[which(i %% 2 == 1)]
  
  cov.m <- (emf.err / fr.degr) * ginv(t(dt.emf.diff) %*% diag(wght) %*% dt.emf.diff, tol = 0)
  
  list(cov.m = cov.m, cor.m = cov.m / ((diag(cov.m) ^ 0.5) %*% t(diag(cov.m) ^ 0.5)), err.diff = err.diff)
  
}


# calculated constants validity ------------------------------ #

emf.cnst.validation <- function(dt.coef, cnst.m, cnst.tune
                              , dt.emf.m, dt.emf.err.m, dt.params.m
                              , dt.coef.m, dt.conc.m, part.eq, reac.nm
                              , lrate.fin
                              , emf.threshold = 5e-5
                              , eq.threshold = 1e-08
                              , eq.thr.type = c("rel", "abs")
                              , method = c("lm", "basic wls")) {
    
    # get iterations number
    
    hardstop <- 2 * length(cnst.tune)
    
    lrate.fin <- lrate.fin * 2
    
    # update cnst (1 step minus)
    
    cnst.tune.nm <- which(unlist(dt.coef[, name]) %in% cnst.tune)
    cnst.m[cnst.tune.nm] <- cnst.m[cnst.tune.nm] - cnst.m[cnst.tune.nm] * lrate.fin
    
    # calculate
    
    grid.opt <- emf.constant.optimizer(dt.coef, cnst.m, cnst.tune
                                       , dt.emf.m, dt.emf.err.m, dt.params.m
                                       , dt.coef.m, dt.conc.m, part.eq, reac.nm
                                       , hardstop
                                       , lrate.init = lrate.fin
                                       , search.density = 1
                                       , emf.threshold * .5
                                       , eq.threshold
                                       , eq.thr.type
                                       , mode = "grid"
                                       , method
                                       , algorithm = "basic search")$grid.opt
    
    # remove garbage
    
    cln <- colnames(grid.opt)
    cln <- cln[cln %like% "^(step\\.id|err|[0-9]+$)"]
    
    grid.opt <- grid.opt[, cln, with = FALSE]
    
    # get optimized constants
    
    cln <- colnames(grid.opt)
    cln <- cln[cln %like% "^[0-9]+$"]
    
    for (cl in cln) {
      
      grid.opt[1:(length(cnst.tune) + 1), f := shift(eval(as.name(cl)), type = "lead") - eval(as.name(cl))]
      grid.opt[(length(cnst.tune) + 1):nrow(grid.opt), f := eval(as.name(cl)) - shift(eval(as.name(cl)))]
      grid.opt[f != 0, cnst.nm := cl]
      
      grid.opt[, f := NULL]
      
    }
    
    # get deltas
    
    grid.opt[, dlt := err - grid.opt[(length(cnst.tune) + 1), err]]
    
    # define validity
    
    cnst.vld <- grid.opt[-(length(cnst.tune) + 1), .(cnst.nm, dlt)]
    
    cnst.vld[, dir := "v.left"]
    cnst.vld[(length(cnst.tune) + 1):nrow(cnst.vld), dir := "v.right"]
    
    cnst.vld <- dcast.data.table(cnst.vld, cnst.nm ~ dir, value.var = "dlt", fun.aggregate = sum, na.rm = TRUE, fill = 0)
    
    cnst.vld[, validity := "OK"]
    cnst.vld[abs(v.left) <= 1e-20 | abs(v.right) <= 1e-20, validity := "Non-Sensitive"]
    cnst.vld[(v.left < 0 & v.right >= 0), validity := "-Inf"]
    cnst.vld[(v.left >= 0 & v.right < 0), validity := "Inf"]
    
    # return
    
    cnst.vld
    
  }


# constants with standard deviations -------------------------- #

emf.constant.deviations <- function(cnst.m, cov.m, cnst.tune.nm, cnst.valid) {
  
  cnst.dev <- cbind(cnst = as.vector(log(exp(cnst.m), 10)), dev = rep(0, length(cnst.m)), validity = rep("OK", length(cnst.m)))
  
  cnst.dev[cnst.tune.nm, "dev"] <- cov.m[row(cov.m) == col(cov.m)] ^ .5
  
  cnst.dev[cnst.tune.nm, "validity"] <- cnst.valid[order(cnst.nm)][, validity]
  
  cnst.dev <- as.data.table(cnst.dev)
  
  cnst.dev[validity == "OK" & as.numeric(dev) / abs(as.numeric(cnst)) > .1, validity := "Insignificant"]
  
  cnst.dev
  
}


# molar coefficients standard deviations ---------------------- #

emf.params.deviations <- function(cnst.m
                                  , cnst.tune.nm
                                  , dt.coef, dt.coef.m, dt.conc.m, part.eq, reac.nm
                                  , dt.emf.m, dt.emf.err.m, dt.params.m
                                  , eq.thr.type, eq.threshold
                                  , method = c("lm", "basic wls")
                                  , emf.threshold) {
  
  rtrn <- emf.params.wrapper(cnst.m
                            , cnst.tune.nm
                            , dt.coef, dt.coef.m, dt.conc.m, part.eq, reac.nm
                            , dt.emf.m, dt.emf.err.m, dt.params.m
                            , eq.thr.type, eq.threshold
                            , method)
  
  list(dt.params.dev = rtrn$dt.params.dev, dt.params = rtrn$dt.params, dt.emf.calc = rtrn$dt.emf.calc)
  
}


# emf residuals ---------------------------------------- #

emf.residuals <- function(dt.emf.m, dt.emf.calc, reac.nm) {
  
  emf.res.abs <- as.matrix(dt.emf.calc) - dt.emf.m
  emf.res.rel <- emf.res.abs / dt.emf.m
  
  obs <- as.vector(dt.emf.m)
  pred <- as.vector(as.matrix(dt.emf.calc))
  
  adj.r.squared <- 1 - (sum((obs - pred) ^ 2) / (length(obs) - reac.nm)) / var(obs)
  
  list(emf.res.abs = emf.res.abs, emf.res.rel = emf.res.rel, adj.r.squared = adj.r.squared)
  
}





