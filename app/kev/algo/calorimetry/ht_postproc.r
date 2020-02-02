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
                   , cnst.tune
                   , cnst.tune.ind
                   , dt.heat.calc
                   , objective.fn = function(){1}
                   , algorithm.options
                   , method
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
    
    rtrn <- objective.fn(algorithm.options$cnst.m[cnst.tune.ind], method = method, algorithm.options)
    
    err <- c(err, rtrn$err)
    dt.heat.diff <- cbind(dt.heat.diff, rtrn$dt.heat.calc[, error])

  }
  
  dt.heat.diff[, tmp := NULL]

  i <- 1:ncol(cnst.grid)
  
  dt.heat.diff <- as.matrix(dt.heat.diff)[, which(i %% 2 == 0)] - as.matrix(dt.heat.diff)[, which(i %% 2 == 1)]
  dt.heat.diff <- dt.heat.diff / (2 * log(exp(ht.threshold), 10))
  
  err.diff <- err[which(i %% 2 == 0)] - err[which(i %% 2 == 1)]
  
  cov.m <- (ht.err / fr.degr) * ginv(t(dt.heat.diff) %*% diag(wght) %*% dt.heat.diff, tol = 0)
  
  # correlation matrix
  
  cor.m <- cov.m / ((diag(cov.m) ^ 0.5) %*% t(diag(cov.m) ^ 0.5))
  
  cor.m <- as.data.table(cor.m)
  setnames(cor.m, cnst.tune)
  
  cor.m <- as.data.frame(cor.m)
  rownames(cor.m) <- cnst.tune
  
  # return
  
  list(cov.m = cov.m, cor.m = cor.m, err.diff = err.diff)
  
}


# constants validity and standard deviations ------------------ #

constant.validation <- function(cnst.m
                                , cnst.tune
                                , objective.fn = function(){1}
                                , evaluation.fn = function(){1}
                                , algorithm.options
                                , metrics
                                , dt.list
                                , cov.m
                                , ht.threshold
                                , lrate.fin
                                , method) {
  
  # get iterations number
  
  algorithm.options$hardstop <- 2 * length(cnst.tune)
  algorithm.options$lrate.init <- lrate.fin * 2
  algorithm.options$algorithm <- "basic search"
  algorithm.options$value.threshold <- algorithm.options$value.threshold * .5
  
  # update cnst (1 step minus)
  
  cnst.m.iter <- cnst.m
  
  cnst.tune.ind <- which(unlist(dt.list$dt.coef[, name]) %in% cnst.tune)
  cnst.m.iter[cnst.tune.ind] <- cnst.m[cnst.tune.ind] - cnst.m[cnst.tune.ind] * lrate.fin
  
  # calculate
  
  grid.opt <- kev.constant.optimizer(objective.fn = objective.fn
                                     , evaluation.fn = evaluation.fn
                                     , values.init = cnst.m.iter[cnst.tune.ind]
                                     , lower.bound = -Inf
                                     , upper.bound = Inf
                                     , dt.list = dt.list
                                     , algorithm.options = algorithm.options
                                     , metrics = metrics
                                     , mode = "grid"
                                     , verbose = TRUE)$grid.opt
  
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
  cnst.vld[v.left < 0 & v.right >= 0, validity := "-Inf"]
  cnst.vld[v.left >= 0 & v.right < 0, validity := "Inf"]
  
  #
  
  cnst.dev <- cbind(cnst = as.vector(log(exp(cnst.m), 10)), dev = rep(0, length(cnst.m)), validity = rep("OK", length(cnst.m)))
  
  cnst.dev[cnst.tune.ind, "dev"] <- cov.m[row(cov.m) == col(cov.m)] ^ .5
  
  cnst.dev[cnst.tune.ind, "validity"] <- cnst.vld[order(cnst.nm)][, validity]
  
  cnst.dev <- as.data.table(cnst.dev)
  
  cnst.dev[validity == "OK" & as.numeric(dev) / abs(as.numeric(cnst)) > .1, validity := "Insignificant"]
  
  setnames(cnst.dev, c("Constant", "St.Deviation", "Validity"))
  
  # return
  
  cnst.dev
  
}


# heats residuals --------------------------------------- #

heat.residuals <- function(dt.heat, dt.heat.calc, dt.enth.calc, dt.enth) {
  
  tmp <- copy(dt.heat)
  
  tmp[, `:=`(heats.calculated = dt.heat.calc[, heats], res.abs = dt.heat.calc[, error])]
  
  x.length <- nrow(dt.enth.calc) - nrow(dt.enth)
  
  tmp[, res.rel := res.abs / heats]

  adj.r.squared <- 1 - (sum((dt.heat[, heats] - dt.heat.calc[, heats]) ^ 2) / (nrow(dt.heat) - x.length)) / var(dt.heat[, heats])
  
  list(dt.heat.calc = tmp, adj.r.squared = adj.r.squared)
  
}




