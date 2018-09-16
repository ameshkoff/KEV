# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #



# evaluators ---------------------------------------------- #

molar.ext.evaluator <- function(x.known, y.raw, dt.res.m, wght) {
  
  # molar coefficients already known
  
  cln.known <- names(x.known)

  # subtract already known from y
  
  x.known.v <- dt.res.m[, cln.known, drop = FALSE] %*% x.known
  y <- as.vector(y.raw - x.known.v)
  
  # prepare data set from udated y and still unknown molar ext. coeff-s
  
  cln.unknown <- colnames(dt.res.m)
  cln.unknown <- setdiff(cln.unknown, cln.known)
  
  cln.unknown <- colSums(dt.res.m[, cln.unknown])
  cln.unknown <- cln.unknown[cln.unknown > 0]
  cln.unknown <- names(cln.unknown)
  
  dt <- data.table(y = y, dt.res.m[, cln.unknown, drop = FALSE])
  
  # formula
  
  frm <- paste("y ~ 0 +", paste(paste0("`", cln.unknown, "`"), collapse = "+"))
  frm <- as.formula(frm)
  
  # run linear model and get coefficients from it
  
  # mol.coef.new <- ginv((t(dt.res.m[, cln.unknown, drop = FALSE]) %*% diag(wght)) %*% dt.res.m[, cln.unknown, drop = FALSE], tol = 0) %*%
  #   ((t(dt.res.m[, cln.unknown, drop = FALSE]) %*% diag(wght)) %*% y)
  # 
  # mol.coef.new <- as.vector(mol.coef.new)
  # names(mol.coef.new) <- cln.unknown
  
  md <- lm(frm, dt, weights = wght)
  # browser()

  mol.coef.new <- md$coefficients
  names(mol.coef.new) <- str_replace_all(names(mol.coef.new), "`", "")
  
  y.calc <- predict(md) + x.known.v
  
  # y.calc <- as.vector(dt.res.m[, cln.unknown, drop = FALSE] %*% mol.coef.new + x.known.v)
  
  # fill NAs (0 actually)
  
  mol.coef.new[is.na(mol.coef.new)] <- 0
  
  # now the trick to place them all in the original order (too dirty maybe)
  
  mol.coef <- rep(1, ncol(dt.res.m))
  names(mol.coef) <- colnames(dt.res.m)
  
  for (j in names(x.known))
    mol.coef[names(mol.coef) == j] <- x.known[names(x.known) == j]
  
  for (j in names(mol.coef.new))
    mol.coef[names(mol.coef) == j] <- mol.coef.new[names(mol.coef.new) == j]
  
  list(mol.coef = mol.coef, y.calc = y.calc)
  
}


# step function ------------------------------------------- #

worker <- function(cnst.m) {
  
  
  # run equilibrium evaluator
  
  dt.res.m <- newton.wrapper(cnst.m, dt.coef.m, dt.conc.m, part.eq, reac.nm, "abs", 1e-08)
  colnames(dt.res.m) <- dt.coef[, name]
  
  cnst.tune.nm <- which(colnames(dt.res.m) %in% cnst.tune)
  
  #  run molar extinction evaluator
  
  mol.coef <- data.table()
  dt.ab.calc <- data.table()
  
  for (i in 1:ncol(dt.ab.m)) {
    
    x.known <- dt.mol.m[i, ]
    y.raw <- dt.ab.m[, i, drop = FALSE]
    
    # weights for linear model
    
    wght <- 1 / (dt.ab.err.m[, i] ^ 2)
    
    rtrn <- molar.ext.evaluator(x.known, y.raw, dt.res.m, wght)
    
    mol.coef <- rbind(mol.coef, as.data.table(as.list(rtrn$mol.coef)))
    dt.ab.calc <- rbind(dt.ab.calc, as.data.table(as.list(rtrn$y.calc)))
    
  }
  
  dt.ab.calc <- data.table(t(dt.ab.calc))
  
  
  # evaluate cost function
  
  observed <- as.vector(dt.ab.m)
  predicted <- as.vector(as.matrix(dt.ab.calc))
  wght <- 1 / (as.vector(dt.ab.err.m) ^ 2)
  
  err <- sum(((observed - predicted) ^ 2) * wght)
  # browser()
  list(err = err, mol.coef = mol.coef)
  
}


worker.wrapper <- function(grid.opt, cnst.m, cnst.iter, step.iter, hardstop = 100, debug = FALSE) {
  
  cnst.back <- cnst.m[cnst.iter] 
  step.success <- grid.opt[closed == length(cnst.tune.nm), max(step.id)]
  step.last <- grid.opt[, max(step.id)]
  err.base <- grid.opt[step.success, err]
  
  # add current row
  
  if (step.iter != step.last) {
    
    grid.opt <- rbind(grid.opt, list(step.id = step.iter, step.type = "xpl", closed = 0), use.names = TRUE, fill = TRUE)
    
    lrate <- grid.opt[step.success, eval(as.name(paste0(cnst.iter, "__lrate")))]
    grid.opt[step.iter, eval(paste0(cnst.iter, "__lrate")) := lrate]
    
  }

  # exploratory move
  
  if (step.success == 1 | grid.opt[step.success, step.type] == "xpl") {
    
    lrate <- grid.opt[step.iter, eval(as.name(paste0(cnst.iter, "__lrate")))]
    
    dt.step <- data.table(cnst = cnst.back, err = err.base)
    
    for (i in 1:(search.density * 2)) {
      
      sgn <- i %% 2
      if (sgn == 0) sgn <- -1

      cnst.curr <- cnst.back + lrate * ((i + 1) %/% 2) * sgn
      cnst.m[cnst.iter] <- cnst.curr
      
      dt.step <- rbind(dt.step, list(cnst.curr, worker(cnst.m)$err), use.names = FALSE)
      
    }
    
    cnst.curr <- dt.step[err == min(err), cnst][1]
    err.curr <- dt.step[, min(err)]
    
    # print(paste(cnst.back, cnst.right, cnst.left, err.base, err.right, err.left))
    
    if (debug) {
      
      grid.opt[step.iter, `:=`(err = dt.step[2, err], closed = closed + 1)]
      grid.opt[step.iter, eval(as.character(cnst.iter)) := dt.step[2, cnst]]
      cnst.m[cnst.iter] <- dt.step[2, cnst]
      
    } else {
      
      if (err.curr < err.base) {
        
        grid.opt[step.iter, `:=`(err = err.curr, closed = closed + 1)]
        grid.opt[step.iter, eval(as.character(cnst.iter)) := cnst.curr]
        cnst.m[cnst.iter] <- cnst.curr
        
      } else {
        
        grid.opt[step.iter, eval(paste0(cnst.iter, "__lrate")) := lrate * .5]
        cnst.m[cnst.iter] <- cnst.back
        
      }
      
    }
    
    tmp <- grid.opt[step.iter, c("step.id", paste0(cnst.tune.nm, "__lrate")), with = FALSE]
    tmp <- melt(tmp, id.vars = "step.id", measure.vars = paste0(cnst.tune.nm, "__lrate"))
    
    cnst.tune.wrk <- as.integer(str_extract(unlist(tmp[value > ab.threshold, variable]), "^[0-9]+"))

    if (length(cnst.tune.wrk) > 0) {
      
      cnst.iter <- which(cnst.tune.wrk == cnst.iter) + 1
      
      if (cnst.iter > length(cnst.tune.wrk)) {
        
        cnst.iter <- cnst.tune.wrk[1]
        
      } else {
        
        cnst.iter <- cnst.tune.wrk[cnst.iter]
        
      }
      # browser()
      if (grid.opt[step.iter, closed] == length(cnst.tune.wrk)) {
        step.iter <- step.iter + 1
      }
      
    }
    
  }
  
  mol.coef <- worker(cnst.m)$mol.coef
  
  if (step.iter < hardstop & length(cnst.tune.wrk) != 0) {
    
    rtrn <- worker.wrapper(grid.opt, cnst.m, cnst.iter, step.iter, hardstop, debug)
    
    grid.opt <- rtrn$grid.opt
    cnst.m <- rtrn$cnst.m

  }

  list(grid.opt = grid.opt, cnst.m = cnst.m)
  
}


# create grid --------------------------------------------- #

dt.res.m <- newton.wrapper(cnst.m, dt.coef.m, dt.conc.m, part.eq, reac.nm, "rel", 1e-08)
colnames(dt.res.m) <- dt.coef[, name]
cnst.tune.nm <- which(unlist(dt.coef[, name]) %in% cnst.tune)

grid.opt <- data.table(step.id = integer(), step.type = character(), err = numeric(), closed = integer())

for (i in cnst.tune.nm) {
  
  grid.opt[, eval(as.character(i)) := numeric()]
  grid.opt[, paste0(eval(i), "__lrate") := numeric()]
  
}

# add row

grid.opt <- rbind(grid.opt, list(step.id = 1, step.type = "xpl", closed = length(cnst.tune.nm)), use.names = TRUE, fill = TRUE)

for (i in cnst.tune.nm) {
  
  grid.opt[step.id == 1, eval(as.character(i)) := cnst.m[i]]
  grid.opt[, eval(paste0(i, "__lrate")) := lrate.init * eval(as.name(i))]
  
}

err.v <- worker(cnst.m)$err
grid.opt[, err := err.v]




res <- worker.wrapper(grid.opt, cnst.m, 16, 2, hardstop = 400, debug = FALSE)
res
worker(res$cnst.m)$mol.coef


# dbg2 <- worker.wrapper(grid.opt, cnst.m, 16, 2, hardstop = 400, debug = TRUE)$grid.opt


# pattern move ------------------------------------------ #








