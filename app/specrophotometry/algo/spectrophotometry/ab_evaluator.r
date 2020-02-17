# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #



# evaluators ---------------------------------------------- #

molar.ext.evaluator <- function(x.known = NULL, y.raw, dt.res.m, wght, method = c("lm", "basic wls"), mode = c("base", "posptroc")) {
  
  
  # if some molar coefficients already known
  
  if (!is.null(x.known)) {
    
    cln.known <- names(x.known)
    
    # subtract already known from y
    
    x.known.v <- dt.res.m[, cln.known, drop = FALSE] %*% x.known
    y <- as.vector(y.raw - x.known.v)
    
    if (length(cln.known) == ncol(dt.res.m)) {
      
      x.known.v <- as.vector(x.known.v)
      
      if (mode[1] == "postproc") {
        
        return(list(mol.coef = x.known, y.calc = x.known.v, mol.coef.dev = NULL))
        
      } else {
        
        return(list(mol.coef = x.known, y.calc = x.known.v))
        
      }

    }
    
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
    
    if (mode[1] == "postproc") {
      
      mol.coef.dev <- summary(md)$coef[, "Std. Error"]
      
    }
    
  } else if (method[1] == "basic wls") {
    
    # basic (weighted) least squares
    
    # if (length(colnames(dt.res.m)[colnames(dt.res.m) %in% cln.unknown]) == 0)
    #   browser()
    
    dt.m <- dt.res.m[, cln.unknown, drop = FALSE]
    
    mol.coef.new <- ginv((t(dt.m) %*% diag(wght)) %*% dt.m, tol = 0) %*% ((t(dt.m) %*% diag(wght)) %*% y)

    mol.coef.new <- as.vector(mol.coef.new)
    names(mol.coef.new) <- cln.unknown
    
    y.calc <- as.vector(dt.res.m[, cln.unknown, drop = FALSE] %*% mol.coef.new + x.known.v)
    
    if (mode[1] == "postproc") {
      
      mol.coef.dev <- (diag(sum((y.raw - y.calc) ^ 2)/(length(y) - length(cln.unknown)) * ginv((t(dt.m)) %*% diag(wght) %*% dt.m, tol = 0))) ^ .5

    }
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
  
  if (mode[1] == "postproc") {
    
    list(mol.coef = mol.coef, y.calc = y.calc, mol.coef.dev = mol.coef.dev)
    
  } else {
    
    list(mol.coef = mol.coef, y.calc = y.calc) 
    
  }
  
}

# for postprocessing only : lots of data passed as arguments, too slow for loop call

molar.ext.wrapper <- function(cnst.m
                              , cnst.tune.nm
                              , dt.coef, dt.coef.m, dt.conc.m, part.eq, reac.nm
                              , dt.ab.m, dt.ab.err.m, dt.mol.m
                              , eq.thr.type, eq.threshold
                              , method = c("lm", "basic wls")
                              , mode = "postproc") {
  
  
  # run equilibrium evaluator
  
  dt.res.m <- newton.wrapper(cnst.m, dt.coef.m, dt.conc.m, part.eq, reac.nm, eq.thr.type[1], eq.threshold)
  colnames(dt.res.m) <- dt.coef[, name]
  
  #  run molar extinction evaluator
  
  mol.coef <- data.table()
  dt.ab.calc <- data.table()
  mol.coef.dev <- data.table()
  
  for (i in 1:ncol(dt.ab.m)) {
    
    y.raw <- dt.ab.m[, i, drop = FALSE]
    
    # weights for linear model
    
    wght <- sum((dt.ab.err.m[, i] ^ 2)) / ((dt.ab.err.m[, i] ^ 2) * length(dt.ab.err.m[, i]))

    # if some molar coefficients are already known
    # browser()
    if (is.matrix(dt.mol.m)) {
      
      x.known <- dt.mol.m[i, ]
      
      rtrn <- molar.ext.evaluator(x.known, y.raw, dt.res.m, wght, method, mode = "postproc")
      
    } else {
      
      rtrn <- molar.ext.evaluator(NULL, y.raw, dt.res.m, wght, method, mode = "postproc")
      
    }
    
    mol.coef <- rbind(mol.coef, as.data.table(as.list(rtrn$mol.coef)))
    dt.ab.calc <- rbind(dt.ab.calc, as.data.table(as.list(rtrn$y.calc)))
    mol.coef.dev <- rbind(mol.coef.dev, as.data.table(as.list(rtrn$mol.coef.dev)))
    
  }

  
  dt.ab.calc <- data.table(t(dt.ab.calc))
  
  # evaluate cost function
  
  observed <- as.vector(dt.ab.m)
  predicted <- as.vector(as.matrix(dt.ab.calc))
  
  wght <- sum(as.vector(dt.ab.err.m) ^ 2) / ((as.vector(dt.ab.err.m) ^ 2) * length(as.vector(dt.ab.err.m)))

  err <- sum(((observed - predicted) ^ 2) * wght)
  
  list(dt.ab.calc = dt.ab.calc, mol.coef = mol.coef, mol.coef.dev = mol.coef.dev, err = err)

}



# optimizer wrapper --------------------------------------- #

constant.optimizer <- function(dt.coef, cnst.m, cnst.tune
                               , dt.ab.m, dt.ab.err.m, dt.mol.m
                               , dt.coef.m, dt.conc.m, part.eq, reac.nm
                               , hardstop = 100
                               , lrate.init = .5
                               , search.density = 1
                               , ab.threshold = 5e-5
                               , eq.threshold = 1e-08
                               , eq.thr.type = c("rel", "abs")
                               , mode = c("base", "grid", "debug")
                               , method = c("lm", "basic wls")
                               , algorithm = c("direct search", "basic search")) {
  
  
  # additional functions are defined inner the main optimizing function so not to pass lots of data as arguments
  # time saved: 2.56 -> 2.07 on dsl.3 dataset
  # sorry for monstruosity!
  
  # step function -------------------- #
  
  constant.error.evaluator <- function(cnst.m, method = c("lm", "basic wls"), mode = c("iterator", "return", "debug")) {
    
    
    # run equilibrium evaluator
    
    dt.res.m <- newton.wrapper(cnst.m, dt.coef.m, dt.conc.m, part.eq, reac.nm, eq.thr.type[1], eq.threshold)
    colnames(dt.res.m) <- dt.coef[, name]
    
    if (any(is.na(dt.res.m)))
      return(list(err = 1e+12))
    
    cnst.tune.nm <- which(colnames(dt.res.m) %in% cnst.tune)
    
    #  run molar extinction evaluator
    
    mol.coef <- data.table()
    dt.ab.calc <- data.table()
    
    for (i in 1:ncol(dt.ab.m)) {
      
      y.raw <- dt.ab.m[, i, drop = FALSE]
      
      # weights for linear model
      wght <- sum((dt.ab.err.m[, i] ^ 2)) / ((dt.ab.err.m[, i] ^ 2) * length(dt.ab.err.m[, i]))
      
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

    wght <- sum(as.vector(dt.ab.err.m) ^ 2) / ((as.vector(dt.ab.err.m) ^ 2) * length(as.vector(dt.ab.err.m)))

    err <- sum(((observed - predicted) ^ 2) * wght)
    
    if (mode[1] == "iterator") {
      
      list(err = err)
      
    } else if (mode[1] == "return") {
      
      list(err = err, mol.coef = mol.coef, dt.ab.calc = dt.ab.calc)
      
    } else if (mode[1] == "debug") {
      
      list(err = err, mol.coef = mol.coef, dt.ab.calc = dt.ab.calc, err.v = (predicted - observed))
      
    }
    
  }
  
  
  # optimizer ------------------------ #
  
  constant.optimizer.inner <- function(grid.opt, cnst.m, cnst.tune.nm
                                       , hardstop = 1000
                                       , mode = c("base", "grid", "debug")
                                       , method = c("lm", "basic wls")
                                       , algorithm = c("direct search", "basic search")
                                       , lrate.init) {

    # prepare values
    
    step.iter <- 2
    cnst.iter <- cnst.tune.nm[1]
    cnst.tune.wrk <- cnst.tune.nm
    
    # loop algorithms(s)
    
    for (j in 1:(hardstop)) {
      # browser()
      cnst.back <- cnst.m[cnst.iter]
      step.success <- grid.opt[closed >= length(cnst.tune.wrk), max(step.id)]
      step.last <- grid.opt[, max(step.id)]
      err.base <- min(grid.opt[step.success, err], grid.opt[nrow(grid.opt), err], na.rm = TRUE)
      
      # add current row
      
      if (step.iter != step.last) {
        
        grid.opt <- rbind(grid.opt, list(step.id = step.iter, step.type = "xpl", closed = 0, lrate = lrate.init), use.names = TRUE, fill = TRUE)
        
        # browser()
        if (grid.opt[step.success, step.type] != "ptrn" & step.iter > 2 & algorithm[1] == "direct search"){
          
          step.success.xpl.prev <- tail(which(grid.opt[!is.na(err), step.type] != "ptrn"), 2)[1]
          impr <- grid.opt[step.success.xpl.prev, err] - grid.opt[step.success, err]
          
          if (impr > 0)
            grid.opt[step.iter, step.type := "ptrn"]
          
        } else if (grid.opt[step.success, step.type] == "ptrn") {
          
          grid.opt[step.iter, step.type := "ptrn.adj"]
          
        }
        
        for (k in cnst.tune.nm) {
          
          step.tmp <- grid.opt[step.success, eval(as.name(paste0(k, "__step")))]
          grid.opt[step.iter, eval(paste0(k, "__step")) := step.tmp]
          grid.opt[step.iter, eval(as.character(k)) := cnst.m[k]]
          
        }
        
      }
      
      if (grid.opt[step.iter, step.type] == "xpl") {
        
        # exploratory move
        
        step <- grid.opt[step.iter, eval(as.name(paste0(cnst.iter, "__step")))]
        
        dt.step <- data.table(cnst = cnst.back, err = err.base)
        

        for (i in 1:(search.density * 2)) {
          
          sgn <- i %% 2
          if (sgn == 0) sgn <- -1
          
          cnst.curr <- cnst.back + step * ((i + 1) %/% 2) * sgn  * lrate.init
          cnst.m[cnst.iter] <- cnst.curr
          
          dt.step <- rbind(dt.step
                           , list(cnst.curr, constant.error.evaluator(cnst.m, method)$err)
                           , use.names = FALSE)
          
        }
        
        cnst.curr <- dt.step[err == min(err), cnst][1]
        err.curr <- dt.step[, min(err)]
          

        if (mode[1] == "debug")
          print(paste(step.iter, cnst.iter, cnst.curr, cnst.back, step))
        
        if (mode[1] == "grid") {
          
          grid.opt[step.iter, `:=`(err = dt.step[2, err], closed = length(cnst.tune.wrk))]
          grid.opt[step.iter, eval(as.character(cnst.iter)) := dt.step[2, cnst]]
          cnst.m[cnst.iter] <- dt.step[2, cnst]
          
        } else {
          
          if (err.curr < err.base) {
            
            grid.opt[step.iter, `:=`(err = err.curr, closed = closed + 1)]
            grid.opt[step.iter, eval(as.character(cnst.iter)) := cnst.curr]
            cnst.m[cnst.iter] <- cnst.curr
            
            # speed up if needed

            if (nrow(grid.opt[step.type == "xpl"]) > 3) {

              tmp <- grid.opt[step.type == "xpl", eval(as.name(paste0(cnst.iter, "__step")))]
              tmp <- tmp[(length(tmp)-3):length(tmp)]
              # browser()

              if (max(tmp) == min(tmp)) {

                lrate.init <- lrate.init * 2

              }

            }
            
          } else {
            
            if (cnst.iter == max(cnst.tune.wrk)) {
              
              lrate.init <- lrate.init * .5
              
            }

            cnst.m[cnst.iter] <- cnst.back
            
          }
          
        }
        
        tmp <- grid.opt[step.iter, c("step.id", paste0(cnst.tune.nm, "__step")), with = FALSE]
        tmp <- melt(tmp, id.vars = "step.id", measure.vars = paste0(cnst.tune.nm, "__step"), variable.factor = FALSE)
        
        cnst.tune.wrk <- as.integer(str_extract(unlist(tmp[, variable]), "^[0-9]+"))
        
        if (lrate.init < ab.threshold)
          cnst.tune.wrk <- numeric()
        
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
        
        cnst.back <- cnst.m
        
        step.success.xpl.prev <- tail(which(grid.opt[, step.type] != "ptrn"), 2)[1]
        
        cnst.m[cnst.tune.nm] <-
          unlist(
            grid.opt[step.iter, c(as.character(cnst.tune.nm)), with = FALSE] +
              grid.opt[step.success, c(as.character(cnst.tune.nm)), with = FALSE] -
              grid.opt[step.success.xpl.prev, c(as.character(cnst.tune.nm)), with = FALSE]
          )
        
        err.curr <- constant.error.evaluator(cnst.m, method)$err
        
        if (err.curr < err.base) {
          
          grid.opt[step.iter, c(as.character(cnst.tune.nm))] <- as.list(cnst.m[cnst.tune.nm])
          grid.opt[step.iter, err := err.curr]
          
        } else {
          
          grid.opt[step.iter, c(as.character(cnst.tune.nm))] <- as.list(cnst.back[cnst.tune.nm])
          grid.opt[step.iter, err := err.base]
          cnst.m <- cnst.back
          
        }
        
        grid.opt[step.iter, closed := length(cnst.tune.wrk)]
        
        step.iter <- step.iter + 1
        
      } else if (grid.opt[step.iter, step.type] == "ptrn.adj") {
        
        # after pattern move
        
        step <- grid.opt[step.iter, eval(as.name(paste0(cnst.iter, "__step")))]
        
        dt.step <- data.table(cnst = cnst.back, err = err.base)
        
        for (i in 1:2) {
          
          sgn <- i %% 2
          if (sgn == 0) sgn <- -1
          
          cnst.curr <- cnst.back + step * ((i + 1) %/% 2) * sgn * lrate.init
          cnst.m[cnst.iter] <- cnst.curr
          
          dt.step <- rbind(dt.step
                           , list(cnst.curr, constant.error.evaluator(cnst.m, method)$err)
                           , use.names = FALSE)
          
        }
        
        cnst.curr <- dt.step[err == min(err), cnst][1]
        err.curr <- dt.step[, min(err)]
        
        if (mode[1] == "debug")
          print(paste(step.iter, cnst.iter, cnst.curr, cnst.back, step))
        
        if (err.curr < err.base) {
          
          grid.opt[step.iter, `:=`(err = err.curr, closed = closed + 1)]
          grid.opt[step.iter, eval(as.character(cnst.iter)) := cnst.curr]
          cnst.m[cnst.iter] <- cnst.curr
          
        } else {
          
          grid.opt[step.iter, `:=`(err = err.base, closed = closed + 1)]
          cnst.m[cnst.iter] <- cnst.back
          
        }

        tmp <- grid.opt[step.iter, c("step.id", paste0(cnst.tune.nm, "__step")), with = FALSE]
        tmp <- melt(tmp, id.vars = "step.id", measure.vars = paste0(cnst.tune.nm, "__step"), variable.factor = FALSE)
        
        cnst.tune.wrk <- as.integer(str_extract(unlist(tmp[, variable]), "^[0-9]+"))
        
        if (lrate.init < ab.threshold)
          cnst.tune.wrk <- numeric()
        
        
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
        
      }
      
      # check conditions
      
      if (length(cnst.tune.wrk) == 0) {
        
        break
        
      }
      
      if (mode[1] == "grid" & step.iter > 100) {
        
        break
        
      }
      
    }
    
    
    if (mode == "debug") {
      
      rtrn <- constant.error.evaluator(cnst.m, method, mode = "debug")
      
      dt.res.m <- newton.wrapper(cnst.m, dt.coef.m, dt.conc.m, part.eq, reac.nm, eq.thr.type, eq.threshold)
      colnames(dt.res.m) <- dt.coef[, name]
      
      list(grid.opt = grid.opt, cnst.m = cnst.m, mol.coef = rtrn$mol.coef, dt.ab.calc = rtrn$dt.ab.calc, dt.res.m = dt.res.m
           , err.v = rtrn$err.v, lrate.fin = lrate.init)
      
    } else {
      
      rtrn <- constant.error.evaluator(cnst.m, method, mode = "return")
      
      dt.res.m <- newton.wrapper(cnst.m, dt.coef.m, dt.conc.m, part.eq, reac.nm, eq.thr.type, eq.threshold)
      colnames(dt.res.m) <- dt.coef[, name]
      
      list(grid.opt = grid.opt, cnst.m = cnst.m, mol.coef = rtrn$mol.coef, dt.ab.calc = rtrn$dt.ab.calc, dt.res.m = dt.res.m, lrate.fin = lrate.init)
      
    }
    
  }
  
  
  # get numbers of tuned constants
  
  cnst.tune.nm <- which(unlist(dt.coef[, name]) %in% cnst.tune)
  
  # create grid for optimizing
  
  grid.opt <- data.table(step.id = integer(), step.type = character(), err = numeric(), closed = integer())
  
  for (i in cnst.tune.nm) {
    
    grid.opt[, eval(as.character(i)) := numeric()]
    grid.opt[, paste0(eval(i), "__step") := numeric()]
    
  }
  
  # create first row
  
  grid.opt <- rbind(grid.opt, list(step.id = 1, step.type = "xpl", closed = length(cnst.tune.nm)), use.names = TRUE, fill = TRUE)
  
  for (i in cnst.tune.nm) {
    
    grid.opt[step.id == 1, eval(as.character(i)) := cnst.m[i]]
    grid.opt[, eval(paste0(i, "__step")) := eval(as.name(as.character(i)))
             ]
    
  }
  
  # get starting error
  
  err.v <- constant.error.evaluator(cnst.m, method, mode = "return")$err
  grid.opt[, err := err.v]
  
  # run optimizer
  
  rtrn <- constant.optimizer.inner(grid.opt, cnst.m, cnst.tune.nm
                                   , hardstop, mode, method, algorithm, lrate.init)
  
  rtrn
  
}








