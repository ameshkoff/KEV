# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #



# evaluators ---------------------------------------------- #

molar.ext.evaluator <- function(x.known = NULL, y.raw, dt.res.m, wght, method = c("lm", "basic wls")) {
  
  
  # if some molar coefficients already known
  
  if (!is.null(x.known)) {
    
    cln.known <- names(x.known)
    
    # subtract already known from y
    
    x.known.v <- dt.res.m[, cln.known, drop = FALSE] %*% x.known
    y <- as.vector(y.raw - x.known.v)
    
  } else {
    
    cln.known <- ""
    y <- as.vector(y.raw)
    x.known.v <- rep(0, length(y))
    
  }
  
  
  # prepare data set from updated y and still unknown molar ext. coeff-s
  
  cln.unknown <- colnames(dt.res.m)
  cln.unknown <- setdiff(cln.unknown, cln.known)
  
  cln.unknown <- colSums(dt.res.m[, cln.unknown, drop = FALSE])
  cln.unknown <- cln.unknown[cln.unknown > 0]
  cln.unknown <- names(cln.unknown)
  
  # run linear model and get coefficients from it
  
  if (method[1] == "lm") {
    
    # classic R (weighted) lm linear model
    
    dt <- data.table(y = y, dt.res.m[, cln.unknown, drop = FALSE])
    
    frm <- paste("y ~ 0 +", paste(paste0("`", cln.unknown, "`"), collapse = "+"))
    frm <- as.formula(frm)
    
    md <- lm(frm, dt, weights = wght)
    
    mol.coef.new <- md$coefficients
    names(mol.coef.new) <- str_replace_all(names(mol.coef.new), "`", "")
    
    y.calc <- predict(md) + x.known.v
    
  } else if (method[1] == "basic wls") {
    
    # basic (weighted) least squares
    
    dt.m <- dt.res.m[, cln.unknown, drop = FALSE]
    
    mol.coef.new <- ginv((t(dt.m) %*% diag(wght)) %*% dt.m, tol = 0) %*% ((t(dt.m) %*% diag(wght)) %*% y)

    mol.coef.new <- as.vector(mol.coef.new)
    names(mol.coef.new) <- cln.unknown
    
    y.calc <- as.vector(dt.res.m[, cln.unknown, drop = FALSE] %*% mol.coef.new + x.known.v)
    
  }
  
  
  # fill NAs (0 actually)
  
  mol.coef.new[is.na(mol.coef.new)] <- 0
  
  # now the trick to place them all in the original order (too dirty maybe)
  
  mol.coef <- rep(1, ncol(dt.res.m))
  names(mol.coef) <- colnames(dt.res.m)
  
  if (!is.null(x.known)) {
    
    for (j in names(x.known))
      mol.coef[names(mol.coef) == j] <- x.known[names(x.known) == j]
    
  }
  
  for (j in names(mol.coef.new))
    mol.coef[names(mol.coef) == j] <- mol.coef.new[names(mol.coef.new) == j]
  
  list(mol.coef = mol.coef, y.calc = y.calc)
  
}


# step function ------------------------------------------- #

worker <- function(cnst.m, method = c("lm", "basic wls"), final = FALSE) {
  
  
  # run equilibrium evaluator
  
  dt.res.m <- newton.wrapper(cnst.m, dt.coef.m, dt.conc.m, part.eq, reac.nm, "abs", 1e-08)
  colnames(dt.res.m) <- dt.coef[, name]
  
  cnst.tune.nm <- which(colnames(dt.res.m) %in% cnst.tune)

  #  run molar extinction evaluator
  
  mol.coef <- data.table()
  dt.ab.calc <- data.table()
  
  for (i in 1:ncol(dt.ab.m)) {
    
    y.raw <- dt.ab.m[, i, drop = FALSE]
    
    # weights for linear model
    
    wght <- 1 / (dt.ab.err.m[, i] ^ 2)
    
    # if some molar coefficients are already known
    
    if (is.matrix(dt.mol.m)) {
      
      x.known <- dt.mol.m[i, ]
      rtrn <- molar.ext.evaluator(x.known, y.raw, dt.res.m, wght, method)
      
    } else {
      
      rtrn <- molar.ext.evaluator(NULL, y.raw, dt.res.m, wght, method)
      
    }

    mol.coef <- rbind(mol.coef, as.data.table(as.list(rtrn$mol.coef)))
    dt.ab.calc <- rbind(dt.ab.calc, as.data.table(as.list(rtrn$y.calc)))
    
  }

  dt.ab.calc <- data.table(t(dt.ab.calc))
  
  
  # evaluate cost function
  
  observed <- as.vector(dt.ab.m)
  predicted <- as.vector(as.matrix(dt.ab.calc))
  wght <- 1 / (as.vector(dt.ab.err.m) ^ 2)
  
  err <- sum(((observed - predicted) ^ 2) * wght)
  
  if (final) {
    
    list(err = err, mol.coef = mol.coef, err.v = (predicted - observed))
    
  } else {
    
    list(err = err)
    
  }
  
  
}


worker.wrapper <- function(grid.opt, cnst.m, cnst.iter, step.iter, hardstop = 100, debug = FALSE, method = c("lm", "basic wls")) {
  
  for (j in 1:(hardstop * 10)) {
    
    cnst.back <- cnst.m[cnst.iter]
    step.success <- grid.opt[closed == length(cnst.tune.nm), max(step.id)]
    step.last <- grid.opt[, max(step.id)]
    err.base <- grid.opt[step.success, err]
    
    # add current row
    
    if (step.iter != step.last) {
      
      grid.opt <- rbind(grid.opt, list(step.id = step.iter, step.type = "xpl", closed = 0), use.names = TRUE, fill = TRUE)
      
      # if (grid.opt[step.success == "xpl"] & step.iter > 2){
      #   
      #   grid.opt[step.iter, step.type := "ptrn"]
      #   
      # }
      
      for (k in cnst.tune.nm) {
        
        lrate.tmp <- grid.opt[step.success, eval(as.name(paste0(k, "__lrate")))]
        grid.opt[step.iter, eval(paste0(k, "__lrate")) := lrate.tmp]
        grid.opt[step.iter, eval(as.character(k)) := cnst.m[k]]
        
      }
      
    }
    
    if (grid.opt[step.iter, step.type] == "xpl") {
      
      # exploratory move
      
      lrate <- grid.opt[step.iter, eval(as.name(paste0(cnst.iter, "__lrate")))]
      
      # if (step.iter == 2) {
      #   
      #   mdelta <- cnst.back  
      #   
      # } else {
      #   
      #   mdelta <- (cnst.back - grid.opt[step.iter - 2, eval(as.name(as.character(cnst.iter)))]) * 2
      #   
      # }
      
      dt.step <- data.table(cnst = cnst.back, err = err.base)
      
      for (i in 1:(search.density * 2)) {
        
        sgn <- i %% 2
        if (sgn == 0) sgn <- -1
        
        cnst.curr <- cnst.back + lrate * ((i + 1) %/% 2) * sgn #* mdelta
        cnst.m[cnst.iter] <- cnst.curr
        
        dt.step <- rbind(dt.step, list(cnst.curr, worker(cnst.m, method)$err), use.names = FALSE)
        
      }
      
      cnst.curr <- dt.step[err == min(err), cnst][1]
      err.curr <- dt.step[, min(err)]
      
      print(paste(step.iter, cnst.iter, cnst.curr, cnst.back, lrate))
      
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
      tmp <- melt(tmp, id.vars = "step.id", measure.vars = paste0(cnst.tune.nm, "__lrate"), variable.factor = FALSE)
      
      cnst.tune.wrk <- as.integer(str_extract(unlist(tmp[value > ab.threshold | is.na(value), variable]), "^[0-9]+"))

      if (length(cnst.tune.wrk) > 0) {

        cnst.iter <- which(cnst.tune.wrk == cnst.iter) + 1
        
        if (length(cnst.iter) == 0)
          cnst.iter <- 1
        
        if (cnst.iter > length(cnst.tune.wrk)) {
          
          cnst.iter <- cnst.tune.wrk[1]

        } else {
          
          cnst.iter <- cnst.tune.wrk[cnst.iter]

        }
        
        if (grid.opt[step.iter, closed] == length(cnst.tune.wrk)) {
          step.iter <- step.iter + 1
        }
        
      }
      
    } else if (grid.opt[step.iter, step.type] == "ptrn") {
      
      # pattern move
      
      
      
    }
    
    # check conditions

    if (length(cnst.tune.wrk) == 0) {
      
      break

    }
    
  }
  
  rtrn <- worker(cnst.m, method, final = TRUE)
  
  list(grid.opt = grid.opt, cnst.m = cnst.m, mol.coef = rtrn$mol.coef, err.v = rtrn$err.v)
  
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
  grid.opt[, eval(paste0(i, "__lrate")) := lrate.init * eval(as.name(as.character(i)))]
  
}

err.v <- worker(cnst.m, method = "lm")$err
grid.opt[, err := err.v]


remove(res)
system.time(res <- worker.wrapper(grid.opt, cnst.m, 16, 2, hardstop = 400, debug = FALSE, method = "basic wls"))
res


# dbg2 <- worker.wrapper(grid.opt, cnst.m, 16, 2, hardstop = 400, debug = TRUE)$grid.opt


# pattern move ------------------------------------------ #








