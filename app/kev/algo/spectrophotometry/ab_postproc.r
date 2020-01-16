# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #


# covariation matrix ----------------------------------------- #

ab.cov <- function(ab.err
                   , cnst.m
                   , cnst.tune.nm
                   , dt.coef, dt.coef.m, dt.conc.m, part.eq, reac.nm
                   , dt.ab.m, dt.ab.err.m, dt.mol.m
                   , eq.thr.type, eq.threshold
                   , method = c("lm", "basic wls")
                   , ab.threshold) {

  fr.degr <- nrow(dt.ab.m) * ncol(dt.ab.err.m) - length(cnst.tune.nm)
  wght <- sum(as.vector(dt.ab.err.m) ^ 2) / ((as.vector(dt.ab.err.m) ^ 2) * length(as.vector(dt.ab.err.m)))
  
  cnst.grid <- cnst.m
  
  for (i in 1:length(cnst.tune.nm)) {
    
    cnst.grid <- cbind(cnst.grid, cnst.m, cnst.m)

    cnst.grid[cnst.tune.nm[i], i*2-1] <- cnst.grid[cnst.tune.nm[i], i*2-1] - ab.threshold
    cnst.grid[cnst.tune.nm[i], i*2] <- cnst.grid[cnst.tune.nm[i], i*2] + ab.threshold
    
  }
  
  cnst.grid <- cnst.grid[, 1:(ncol(cnst.grid) - 1)]
  
  dt.ab.diff <- data.table()
  err <- numeric()
  
  for (i in 1:ncol(cnst.grid)) {
    
    rtrn <- molar.ext.wrapper(cnst.grid[, i]
                              , cnst.tune.nm
                              , dt.coef, dt.coef.m, dt.conc.m, part.eq, reac.nm
                              , dt.ab.m, dt.ab.err.m, dt.mol.m
                              , eq.thr.type, eq.threshold
                              , method)

    err <- c(err, rtrn$err)
    rtrn <- as.vector(as.matrix(rtrn$dt.ab.calc))
    dt.ab.diff <- rbind(dt.ab.diff, as.data.table(as.list(rtrn)))
    
  }
  
  dt.ab.diff <- data.table(t(dt.ab.diff))
  
  i <- 1:ncol(cnst.grid)
  
  dt.ab.diff <- as.matrix(dt.ab.diff)[, which(i %% 2 == 0)] - as.matrix(dt.ab.diff)[, which(i %% 2 == 1)]
  dt.ab.diff <- dt.ab.diff / (2 * log(exp(ab.threshold), 10))
  
  err.diff <- err[which(i %% 2 == 0)] - err[which(i %% 2 == 1)]
  
  cov.m <- (ab.err / fr.degr) * ginv(t(dt.ab.diff) %*% diag(wght) %*% dt.ab.diff, tol = 0)

  list(cov.m = cov.m, cor.m = cov.m / ((diag(cov.m) ^ 0.5) %*% t(diag(cov.m) ^ 0.5)), err.diff = err.diff)

}


# calculated constants validity ------------------------------ #

cnst.validation <- function(dt.coef, cnst.m, cnst.tune
                           , dt.ab.m, dt.ab.err.m, dt.mol.m
                           , dt.coef.m, dt.conc.m, part.eq, reac.nm
                           , lrate.fin
                           , ab.threshold = 5e-5
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
  cnst.vld[abs(v.left) <= 1e-20 | abs(v.right) <= 1e-20, validity := "Non-Sensitive"]
  cnst.vld[(v.left < 0 & v.right >= 0), validity := "-Inf"]
  cnst.vld[(v.left >= 0 & v.right < 0), validity := "Inf"]
  
  # return
  
  cnst.vld
  
}


# constants with standard deviations -------------------------- #

constant.deviations <- function(cnst.m, cov.m, cnst.tune.nm, cnst.valid) {
  
  cnst.dev <- cbind(cnst = as.vector(log(exp(cnst.m), 10)), dev = rep(0, length(cnst.m)), validity = rep("OK", length(cnst.m)))
  
  cnst.dev[cnst.tune.nm, "dev"] <- cov.m[row(cov.m) == col(cov.m)] ^ .5
  
  cnst.dev[cnst.tune.nm, "validity"] <- cnst.valid[order(cnst.nm)][, validity]
  
  cnst.dev <- as.data.table(cnst.dev)
  
  cnst.dev[validity == "OK" & as.numeric(dev) / abs(as.numeric(cnst)) > .1, validity := "Insignificant"]
  
  cnst.dev
  
}


# molar coefficients standard deviations ---------------------- #

molar.coef.deviations <- function(cnst.m
                                  , cnst.tune.nm
                                  , dt.coef, dt.coef.m, dt.conc.m, part.eq, reac.nm
                                  , dt.ab.m, dt.ab.err.m, dt.mol.m
                                  , eq.thr.type, eq.threshold
                                  , method = c("lm", "basic wls")
                                  , ab.threshold) {
  
  rtrn <- molar.ext.wrapper(cnst.m
                            , cnst.tune.nm
                            , dt.coef, dt.coef.m, dt.conc.m, part.eq, reac.nm
                            , dt.ab.m, dt.ab.err.m, dt.mol.m
                            , eq.thr.type, eq.threshold
                            , method)
  
  list(mol.coef.dev = rtrn$mol.coef.dev, mol.coef = rtrn$mol.coef, dt.ab.calc = rtrn$dt.ab.calc)
  
}


# absorbance residuals ---------------------------------------- #

absorbance.residuals <- function(dt.ab.m, dt.ab.calc, reac.nm) {
  
  ab.res.abs <- as.matrix(dt.ab.calc) - dt.ab.m
  ab.res.rel <- ab.res.abs / dt.ab.m
  
  obs <- as.vector(dt.ab.m)
  pred <- as.vector(as.matrix(dt.ab.calc))
  
  adj.r.squared <- 1 - (sum((obs - pred) ^ 2) / (length(obs) - reac.nm)) / var(obs)
  
  list(ab.res.abs = ab.res.abs, ab.res.rel = ab.res.rel, adj.r.squared = adj.r.squared)
  
}





