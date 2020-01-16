# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #


# covariation matrix ----------------------------------------- #

nm.cov <- function(nm.err
                   , cnst.m
                   , cnst.tune.nm
                   , dt.coef, dt.coef.m, dt.conc.m, part.eq, reac.nm
                   , dt.nm, dt.ind
                   , coef.b, conc.b
                   , eq.thr.type, eq.threshold
                   , method = c("lm", "basic wls")
                   , nm.threshold) {
  
  fr.degr <- nrow(dt.nm) - length(cnst.tune.nm)
  wght <- dt.nm[, wght]
  
  cnst.grid <- cnst.m
  
  for (i in 1:length(cnst.tune.nm)) {
    
    cnst.grid <- cbind(cnst.grid, cnst.m, cnst.m)
    
    cnst.grid[cnst.tune.nm[i], i * 2 - 1] <- cnst.grid[cnst.tune.nm[i], i * 2 - 1] - nm.threshold
    cnst.grid[cnst.tune.nm[i], i * 2] <- cnst.grid[cnst.tune.nm[i], i * 2] + nm.threshold
    
  }
  
  cnst.grid <- cnst.grid[, 1:(ncol(cnst.grid) - 1)]
  
  dt.nm.diff <- data.table()
  err <- numeric()
  
  for (i in 1:ncol(cnst.grid)) {
    
    rtrn <- nm.shift.evaluator.wrapper(cnst.grid[, i]
                              , cnst.tune.nm
                              , dt.coef, dt.coef.m, dt.conc.m, part.eq, reac.nm
                              , dt.nm, dt.ind
                              , coef.b, conc.b
                              , eq.thr.type, eq.threshold
                              , method)
    
    err <- c(err, rtrn$err)
    rtrn <- as.vector(as.matrix(rtrn$dt.nm.calc[, calculated]))
    dt.nm.diff <- rbind(dt.nm.diff, as.data.table(as.list(rtrn)))
    
  }
  
  dt.nm.diff <- data.table(t(dt.nm.diff))
  
  i <- 1:ncol(cnst.grid)
  
  dt.nm.diff <- as.matrix(dt.nm.diff)[, which(i %% 2 == 0)] - as.matrix(dt.nm.diff)[, which(i %% 2 == 1)]
  dt.nm.diff <- dt.nm.diff / (2 * log(exp(nm.threshold), 10))
  
  err.diff <- err[which(i %% 2 == 0)] - err[which(i %% 2 == 1)]
  
  cov.m <- (nm.err / fr.degr) * ginv(t(dt.nm.diff) %*% diag(wght) %*% dt.nm.diff, tol = 0)
  
  list(cov.m = cov.m, cor.m = cov.m / ((diag(cov.m) ^ 0.5) %*% t(diag(cov.m) ^ 0.5)), err.diff = err.diff)
  
}


# calculated constants validity ------------------------------ #

nm.cnst.validation <- function(dt.coef, cnst.m, cnst.tune
                            , dt.nm, dt.ind
                            , dt.coef.m, dt.conc.m, part.eq, reac.nm
                            , coef.b, conc.b
                            , lrate.fin
                            , nm.threshold = 5e-5
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
  
  grid.opt <- nm.constant.optimizer(dt.coef, cnst.m, cnst.tune
                                 , dt.nm, dt.ind
                                 , dt.coef.m, dt.conc.m, part.eq, reac.nm
                                 , coef.b, conc.b
                                 , hardstop
                                 , lrate.init = lrate.fin
                                 , search.density = 1
                                 , nm.threshold * .5
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
  cnst.vld[v.left <= 1e-20 | v.right <= 1e-20, validity := "Non-Sensitive"]
  cnst.vld[(v.left < 0 & v.right >= 0), validity := "-Inf"]
  cnst.vld[(v.left >= 0 & v.right < 0), validity := "Inf"]
  
  # return
  
  cnst.vld
  
}


# constants with standard deviations -------------------------- #

nm.constant.deviations <- function(cnst.m, cov.m, cnst.tune.nm, cnst.valid) {
  
  cnst.dev <- cbind(cnst = as.vector(log(exp(cnst.m), 10)), dev = rep(0, length(cnst.m)), validity = rep("OK", length(cnst.m)))
  
  cnst.dev[cnst.tune.nm, "dev"] <- cov.m[row(cov.m) == col(cov.m)] ^ .5
  
  cnst.dev[cnst.tune.nm, "validity"] <- cnst.valid[order(cnst.nm)][, validity]
  
  cnst.dev <- as.data.table(cnst.dev)
  
  cnst.dev[validity == "OK" & as.numeric(dev) / abs(as.numeric(cnst)) > .1, validity := "Insignificant"]
  
  cnst.dev
  
}


# molar coefficients standard deviations ---------------------- #

nm.ind.shift.deviations <- function(cnst.m
                                  , cnst.tune.nm
                                  , dt.coef, dt.coef.m, dt.conc.m, part.eq, reac.nm
                                  , dt.nm, dt.ind
                                  , coef.b, conc.b
                                  , eq.thr.type, eq.threshold
                                  , method = c("lm", "basic wls")
                                  , nm.threshold) {
  
  rtrn <- nm.shift.evaluator.wrapper(cnst.m
                            , cnst.tune.nm
                            , dt.coef, dt.coef.m, dt.conc.m, part.eq, reac.nm
                            , dt.nm, dt.ind
                            , coef.b, conc.b
                            , eq.thr.type, eq.threshold
                            , method)
  
  list(ind.shift.dev = rtrn$ind.shift.dev, ind.shift = rtrn$ind.shift, dt.nm.calc = rtrn$dt.nm.calc)
  
}


# chemical shifts residuals ---------------------------------------- #

nm.shift.residuals <- function(dt.nm.m, dt.nm.calc, reac.nm) {
  
  nm.res.abs <- as.matrix(dt.nm.calc) - dt.nm.m
  nm.res.rel <- nm.res.abs / dt.nm.m
  
  obs <- as.vector(dt.nm.m)
  pred <- as.vector(as.matrix(dt.nm.calc))
  
  adj.r.squared <- 1 - (sum((obs - pred) ^ 2) / (length(obs) - reac.nm)) / var(obs)
  
  list(nm.res.abs = nm.res.abs, nm.res.rel = nm.res.rel, adj.r.squared = adj.r.squared)
  
}





