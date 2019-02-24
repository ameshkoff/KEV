# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #



# evaluators ---------------------------------------------- #

nm.shift.evaluator <- function(dt, dt.ind, dt.res.m, coef.b, conc.b
                               , method = c("lm", "basic wls"), mode = c("base", "posptroc")) {
  
  obs.shift <- dt[, observation]
  wght <- dt[, wght]
  signal.p <- dt[, signal][1]
  
  # known forms
  
  if (is.data.table(dt.ind)) {
    
    x.known <- dt.ind[signal == signal.p]
    
  } else {
    
    x.known <- ""
    names(x.known) <- ""
    
  }
  
  # independent variables matrix
  
  dt.frac <- t(diag(coef.b, nrow = length(coef.b)) %*% t(dt.res.m[, names(coef.b)] * (1 / conc.b)))
  colnames(dt.frac) <- names(coef.b)
  
  cln <- colnames(dt.frac)
  cln <- cln[!(cln %in% colnames(x.known))]
  
  dt.frac.u <- dt.frac[, cln, drop = FALSE]
  
  # if some molar coefficients already known
  
  if (is.data.table(x.known)) {
    
    # subtract already known from y
    
    cln <- colnames(dt.frac)
    cln <- cln[(cln %in% colnames(x.known))]
    
    x.known.v <- rowSums(dt.frac[, cln, drop = FALSE] %*% unlist(x.known[, !"signal", with = FALSE]))
    y <- as.vector(obs.shift - x.known.v)
    
    if (length(x.known) - 1 == length(coef.b)) {
      
      x.known.v <- as.vector(x.known.v)
      
      if (mode[1] == "postproc") {
        
        return(list(ind.shift = x.known, y.calc = x.known.v, ind.shift.dev = NULL))
        
      } else {
        
        return(list(ind.shift = x.known, y.calc = x.known.v))
        
      }
      
    }
    
  } else {
    
    y <- as.vector(obs.shift)
    x.known.v <- rep(0, length(y))
    
  }
  
  # run linear model and get coefficients from it
  
  if (method[1] == "lm") {
    
    # classic R (weighted) lm linear model
    
    dt <- data.table(y = y, dt.frac.u)
    
    frm <- paste("y ~ 0 +", paste(paste0("`", colnames(dt.frac.u), "`"), collapse = "+"))
    frm <- as.formula(frm)
    
    md <- lm(frm, dt, weights = wght)
    
    ind.shift.new <- md$coefficients
    # ind.shift.new <- ind.shift.new[!(ind.shift.new %like% "^\\(Intercept\\)$")]
    
    names(ind.shift.new) <- str_replace_all(names(ind.shift.new), "`", "")
    
    y.calc <- predict(md) + x.known.v
    
    if (mode[1] == "postproc") {
      
      ind.shift.dev <- summary(md)$coef[, "Std. Error"]
      
    }
    
  } else if (method[1] == "basic wls") {
    
    # basic (weighted) least squares
    
    ind.shift.new <- ginv((t(dt.frac.u) %*% diag(wght)) %*% dt.frac.u, tol = 0) %*% ((t(dt.frac.u) %*% diag(wght)) %*% y)
    
    ind.shift.new <- as.vector(ind.shift.new)
    names(ind.shift.new) <- colnames(dt.frac.u)
    
    y.calc <- as.vector(dt.frac.u %*% ind.shift.new + x.known.v)

    if (mode[1] == "postproc") {
      
      ind.shift.dev <- (diag(sum((obs.shift - y.calc) ^ 2)/(length(y) - length(ncol(dt.frac.u))) *
                               ginv((t(dt.frac.u)) %*% diag(wght) %*% dt.frac.u, tol = 0))) ^ .5
      
    }
  }
  
  
  # fill NAs (0 actually)
  
  ind.shift.new[is.na(ind.shift.new)] <- 0
  
  # now the trick to place them all in the original order (too dirty maybe)
  
  ind.shift <- coef.b
  
  if (is.data.table(x.known)) {
    
    for (j in colnames(dt.frac)[colnames(dt.frac) %in% colnames(x.known)])
      ind.shift[names(ind.shift) == j] <- unlist(x.known[, eval(as.name(j))])
    
  }
  
  for (j in names(ind.shift.new))
    ind.shift[names(ind.shift) == j] <- ind.shift.new[names(ind.shift.new) == j]
  
  # to list
  
  ind.shift <- as.list(ind.shift)
  
  # return
  if (mode[1] == "postproc") {
    
    names(ind.shift.dev) <- colnames(dt.frac.u)
    ind.shift.dev <- as.list(ind.shift.dev)
    
    list(ind.shift = ind.shift, y.calc = y.calc, ind.shift.dev = ind.shift.dev)
    
  } else {
    
    list(ind.shift = ind.shift, y.calc = y.calc) 
    
  }
  
}

# for postprocessing only : lots of data passed as arguments, too slow for loop call

nm.shift.evaluator.wrapper <- function(cnst.m
                              , cnst.tune.nm
                              , dt.coef, dt.coef.m, dt.conc.m, part.eq, reac.nm
                              , dt.nm, dt.ind
                              , coef.b, conc.b
                              , eq.thr.type, eq.threshold
                              , method = c("lm", "basic wls")
                              , mode = "postproc") {
  
  
  # run equilibrium evaluator
  
  dt.res.m <- newton.wrapper(cnst.m, dt.coef.m, dt.conc.m, part.eq, reac.nm, eq.thr.type[1], eq.threshold)
  colnames(dt.res.m) <- dt.coef[, name]
  
  # run individual chemical shift evaluator
  
  splt <- split(dt.nm, dt.nm[, signal])
  splt <- lapply(splt, nm.shift.evaluator, dt.ind, dt.res.m, coef.b, conc.b, method, mode = "postproc")
  
  ind.shift <- lapply(splt, function(x) { x$ind.shift })
  ind.shift <- rbindlist(ind.shift)
  
  ind.shift.dev <- lapply(splt, function(x) { x$ind.shift.dev })
  ind.shift.dev <- rbindlist(ind.shift.dev)
  
  if (is.data.table(dt.ind)) {
    
    ind.shift <- cbind(dt.ind[, .(signal)], ind.shift)
    ind.shift.dev <- cbind(dt.ind[, .(signal)], ind.shift.dev)
    
  } else {
    
    ind.shift <- data.table(signal = dt.nm[, sort(unique(signal))], ind.shift)
    ind.shift.dev <- data.table(signal = dt.nm[, sort(unique(signal))], ind.shift.dev)
    
  }
  
  dt.nm.calc <- unlist(lapply(splt, function(x) { x$y.calc }))
  dt.nm.calc <- dt.nm[, .(signal, solution, calculated = dt.nm.calc)]
  
  # evaluate cost function
  
  observed <- dt.nm[, observation]
  predicted <- dt.nm.calc[, calculated]
  
  err <- sum(((observed - predicted) ^ 2) * dt.nm[, wght])
  
  list(dt.nm.calc = dt.nm.calc, ind.shift = ind.shift, ind.shift.dev = ind.shift.dev, err = err)
  
}



# optimizer wrapper --------------------------------------- #

nm.constant.optimizer <- function(dt.coef, cnst.m, cnst.tune
                               , dt.nm, dt.ind
                               , dt.coef.m, dt.conc.m, part.eq, reac.nm
                               , coef.b, conc.b
                               , hardstop = 100
                               , lrate.init = .5
                               , search.density = 1
                               , nm.threshold = 5e-5
                               , eq.threshold = 1e-08
                               , eq.thr.type = c("rel", "abs")
                               , mode = c("base", "grid", "debug")
                               , method = c("lm", "basic wls")
                               , algorithm = c("direct search", "basic search")) {
  
  
  # additional functions are defined inner the main optimizing function so not to pass lots of data as arguments
  # sorry for monstruosity!
  
  # step function -------------------- #
  
  constant.error.evaluator <- function(cnst.m, method = c("lm", "basic wls"), mode = c("iterator", "return", "debug")) {
    
    
    # run equilibrium evaluator
    
    dt.res.m <- newton.wrapper(cnst.m, dt.coef.m, dt.conc.m, part.eq, reac.nm, eq.thr.type[1], eq.threshold)
    colnames(dt.res.m) <- dt.coef[, name]
    
    if (any(is.na(dt.res.m)))
      return(list(err = 1e+12))
    
    cnst.tune.nm <- which(colnames(dt.res.m) %in% cnst.tune)
    
    # run individual chemical shift evaluator
    
    splt <- split(dt.nm, dt.nm[, signal])
    splt <- lapply(splt, nm.shift.evaluator, dt.ind, dt.res.m, coef.b, conc.b, method)
    
    ind.shift <- lapply(splt, function(x) { x$ind.shift })
    ind.shift <- rbindlist(ind.shift)
    
    if (is.data.table(dt.ind)) {
      
      ind.shift <- cbind(dt.ind[, .(signal)], ind.shift)
      
    } else {
      
      ind.shift <- data.table(signal = dt.nm[, sort(unique(signal))], ind.shift)
      
    }

    dt.nm.calc <- unlist(lapply(splt, function(x) { x$y.calc }))
    dt.nm.calc <- dt.nm[, .(signal, solution, calculated = dt.nm.calc)]

    # evaluate cost function
    
    observed <- dt.nm[, observation]
    predicted <- dt.nm.calc[, calculated]
    
    err <- sum(((observed - predicted) ^ 2) * dt.nm[, wght])
    
    if (mode[1] == "iterator") {
      
      list(err = err)
      
    } else if (mode[1] == "return") {
      
      list(err = err, ind.shift = ind.shift, dt.nm.calc = dt.nm.calc)
      
    } else if (mode[1] == "debug") {
      
      list(err = err, ind.shift = ind.shift, dt.nm.calc = dt.nm.calc, err.v = (predicted - observed))
      
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
      
      cnst.back <- cnst.m[cnst.iter]
      step.success <- grid.opt[closed >= length(cnst.tune.wrk), max(step.id)]
      step.last <- grid.opt[, max(step.id)]
      err.base <- min(grid.opt[step.success, err], grid.opt[nrow(grid.opt), err], na.rm = TRUE)
      
      # add current row
      
      if (step.iter != step.last) {
        
        grid.opt <- rbind(grid.opt, list(step.id = step.iter, step.type = "xpl", closed = 0, lrate = lrate.init), use.names = TRUE, fill = TRUE)
        
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
        
        if (lrate.init < nm.threshold)
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
        
        if (lrate.init < nm.threshold)
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
    
    
    if (mode[1] == "debug") {
      
      rtrn <- constant.error.evaluator(cnst.m, method, mode = "debug")
      
      dt.res.m <- newton.wrapper(cnst.m, dt.coef.m, dt.conc.m, part.eq, reac.nm, eq.thr.type, eq.threshold)
      colnames(dt.res.m) <- dt.coef[, name]
      
      list(grid.opt = grid.opt, cnst.m = cnst.m, ind.shift = rtrn$ind.shift, dt.nm.calc = rtrn$dt.nm.calc, dt.res.m = dt.res.m
           , err.v = rtrn$err.v, lrate.fin = lrate.init)
      
    } else {
      
      rtrn <- constant.error.evaluator(cnst.m, method, mode = "return")
      
      dt.res.m <- newton.wrapper(cnst.m, dt.coef.m, dt.conc.m, part.eq, reac.nm, eq.thr.type, eq.threshold)
      colnames(dt.res.m) <- dt.coef[, name]
      
      list(grid.opt = grid.opt, cnst.m = cnst.m, ind.shift = rtrn$ind.shift, dt.nm.calc = rtrn$dt.nm.calc, dt.res.m = dt.res.m, lrate.fin = lrate.init)
      
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








