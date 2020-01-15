# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2020                                                 #
#                                                            #
# ########################################################## #


# covariation matrix ----------------------------------------- #

ht.cov <- function(ht.err
                   , cnst.m
                   , cnst.tune.ind
                   , dt.heat.calc
                   , objective.fn = function(){1}
                   , algorithm.options
                   , method = c("lm", "basic wls")
                   , ht.threshold) {
  
  fr.degr <- nrow(dt.heat.calc) - length(cnst.tune.ind)
  wght <- sum(dt.heat.calc[, error] ^ 2) / ((dt.heat.calc[, error] ^ 2) * nrow(dt.heat.calc))
  
  cnst.grid <- cnst.m
  
  for (i in 1:length(cnst.tune.ind)) {
    
    cnst.grid <- cbind(cnst.grid, cnst.m, cnst.m)
    
    cnst.grid[cnst.tune.ind[i], i*2-1] <- cnst.grid[cnst.tune.ind[i], i*2-1] - ht.threshold
    cnst.grid[cnst.tune.ind[i], i*2] <- cnst.grid[cnst.tune.ind[i], i*2] + ht.threshold
    
  }
  
  cnst.grid <- cnst.grid[, 1:(ncol(cnst.grid) - 1)]
  
  dt.heat.diff <- data.table(tmp = rep(0, nrow(dt.heat.calc)))
  err <- numeric()
  
  for (i in 1:ncol(cnst.grid)) {
    
    algorithm.options$cnst.m <- cnst.grid[, i]
    
    rtrn <- objective.fn(algorithm.options$cnst.m[cnst.tune.ind], method = "basic wls", algorithm.options)
    
    err <- c(err, rtrn$err)
    dt.heat.diff <- cbind(dt.heat.diff, rtrn$dt.heat.calc[, error])

  }
  
  dt.heat.diff[, tmp := NULL]

  i <- 1:ncol(cnst.grid)
  
  dt.heat.diff <- as.matrix(dt.heat.diff)[, which(i %% 2 == 0)] - as.matrix(dt.heat.diff)[, which(i %% 2 == 1)]
  dt.heat.diff <- dt.heat.diff / (2 * log(exp(ht.threshold), 10))
  
  err.diff <- err[which(i %% 2 == 0)] - err[which(i %% 2 == 1)]
  
  cov.m <- (ht.err / fr.degr) * ginv(t(dt.heat.diff) %*% diag(wght) %*% dt.heat.diff, tol = 0)
  
  list(cov.m = cov.m, cor.m = cov.m / ((diag(cov.m) ^ 0.5) %*% t(diag(cov.m) ^ 0.5)), err.diff = err.diff)
  
}


# constants validity and standard deviations ------------------ #

constant.validation <- function(dt.coef, cnst.m, cnst.tune
                            , dt.ab.m, dt.ab.err.m, dt.mol.m
                            , dt.coef.m, dt.conc.m, part.eq, reac.nm
                            , lrate.fin
                            , ht.threshold = 5e-5
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
  
  grid.opt <- constant.optimizer(dt.coef, cnst.m, cnst.tune
                                 , dt.ab.m, dt.ab.err.m, dt.mol.m
                                 , dt.coef.m, dt.conc.m, part.eq, reac.nm
                                 , hardstop
                                 , lrate.init = lrate.fin
                                 , search.density = 1
                                 , ab.threshold * .5
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
  cnst.vld[v.left <= 1e-20 || v.right <= 1e-20, validity := "Non-Sensitive"]
  cnst.vld[(v.left < 0 && v.right >= 0), validity := "-Inf"]
  cnst.vld[(v.left >= 0 && v.right < 0), validity := "Inf"]
  
  #
  
  cnst.dev <- cbind(cnst = as.vector(log(exp(cnst.m), 10)), dev = rep(0, length(cnst.m)), validity = rep("OK", length(cnst.m)))
  
  cnst.dev[cnst.tune.nm, "dev"] <- cov.m[row(cov.m) == col(cov.m)] ^ .5
  
  cnst.dev[cnst.tune.nm, "validity"] <- cnst.valid[order(cnst.nm)][, validity]
  
  cnst.dev <- as.data.table(cnst.dev)
  
  cnst.dev[validity == "OK" & as.numeric(dev) / abs(as.numeric(cnst)) > .1, validity := "Insignificant"]
  
  cnst.dev
  
}

