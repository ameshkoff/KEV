# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2020                                                 #
#                                                            #
# ########################################################## #


# main optmizer -------------------------------------------- #

kev.constant.optimizer <- function(objective.fn = ht.objective.function
                                  , evaluation.fn = ht.enth.evaluator
                                  , values.init
                                  , lower.bound
                                  , upper.bound
                                  , dt.list # data tables wrapped in the list
                                  , algorithm.options = list(algorithm = "direct search"
                                                             , hardstop = 100
                                                             , lrate.init = .5
                                                             , search.density = 1
                                                             , value.threshold = 5e-5
                                                             , eq.threshold = 1e-08
                                                             , eq.thr.type = c("rel", "abs"))
                                  , metrics = "sse"
                                  , mode = c("base", "grid", "debug")
                                  , verbose = TRUE) {
  
  if (algorithm.options$algorithm %in% c("basic search", "direct search")) {
    
    algorithm.options$cnst.m <- dt.list$cnst.m
    algorithm.options$values.tuned.ind <- dt.list$values.tuned.ind
    
    if (!is.null(dt.list$conc.series)) algorithm.options[["conc.series"]] <- dt.list$conc.series
    
    objective.fn <- objective.fn(metrics = metrics, mode = "iterator", dt.list = dt.list)
    
    kev.optimizer <- kev.direct.search(values.init
                                       , lower.bound = -Inf
                                       , upper.bound = Inf
                                       , objective.fn = objective.fn
                                       , hardstop = algorithm.options$hardstop
                                       , mode = mode
                                       , method = "basic wls"
                                       , algorithm = algorithm.options$algorithm
                                       , lrate.init = algorithm.options$lrate.init
                                       , objective.fn.args = algorithm.options
    )
    
    rtrn <- kev.optimizer()
    
  }
  
  rtrn
  
}


# direct search optimizer ---------------------------------- #
# !!! closure in the the body of the main optimizer (wrapper) to reduce data transmission

kev.direct.search <- function(values.init
                              , lower.bound
                              , upper.bound
                              , objective.fn = function(){1}
                              , hardstop = 1000
                              , mode = c("base", "grid", "debug")
                              , method = c("lm", "basic wls")
                              , algorithm = c("direct search", "basic search")
                              , lrate.init
                              , objective.fn.args = list()
                              ) {
  
  work.fn <- function() {
    
    values.opt.ind <- seq_along(values.init)
    
    # first step ---------------------------- #
    
    grid.opt <- data.table(step.id = integer(), step.type = character(), err = numeric(), closed = integer())
    
    for (i in values.opt.ind) {
      
      grid.opt[, eval(as.character(i)) := numeric()]
      grid.opt[, paste0(i, "__step") := numeric()]
      
    }
    
    grid.opt <- rbind(grid.opt, list(step.id = 1, step.type = "xpl", closed = length(values.init)), use.names = TRUE, fill = TRUE)
    
    for (i in values.opt.ind) {
      
      grid.opt[step.id == 1, eval(as.character(i)) := values.init[i]]
      grid.opt[, paste0(i, "__step") := eval(as.name(as.character(i)))]
      
    }
    
    # get starting error
    
    err.v <- objective.fn(values.init, method, objective.fn.args)
    grid.opt[, err := err.v]

    # loop --------------------------------- #
    
    step.iter <- 2
    value.iter <- 1
    value.tune.wrk <- values.opt.ind
    
    for (j in 1:(hardstop)) {
      
      value.back <- values.init[value.iter]
      step.success <- grid.opt[closed >= length(value.tune.wrk), max(step.id)]
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
        
        for (k in values.opt.ind) {
          
          step.tmp <- grid.opt[step.success, eval(as.name(paste0(k, "__step")))]
          grid.opt[step.iter, eval(paste0(k, "__step")) := step.tmp]
          grid.opt[step.iter, eval(as.character(k)) := values.init[k]]
          
        }
        
      }
      
      if (grid.opt[step.iter, step.type] == "xpl") {
        
        # exploratory move
        
        step <- grid.opt[step.iter, eval(as.name(paste0(value.iter, "__step")))]
        
        dt.step <- data.table(value = value.back, err = err.base)
        
        
        for (i in 1:(objective.fn.args$search.density * 2)) {
          
          sgn <- i %% 2
          if (sgn == 0) sgn <- -1
          
          value.curr <- value.back + step * ((i + 1) %/% 2) * sgn  * lrate.init
          values.init[value.iter] <- value.curr
          
          dt.step <- rbind(dt.step
                           , list(value.curr, objective.fn(values.init, method, objective.fn.args))
                           , use.names = FALSE)
          
        }
        
        value.curr <- dt.step[err == min(err), value][1]
        err.curr <- dt.step[, min(err)]
        
        
        if (mode[1] == "debug")
          print(paste(step.iter, value.iter, value.curr, value.back, step))
        
        if (mode[1] == "grid") {
          
          grid.opt[step.iter, `:=`(err = dt.step[2, err], closed = length(value.tune.wrk))]
          grid.opt[step.iter, eval(as.character(value.iter)) := dt.step[2, value]]
          values.init[value.iter] <- dt.step[2, value]
          
        } else {
          
          if (err.curr < err.base) {
            
            grid.opt[step.iter, `:=`(err = err.curr, closed = closed + 1)]
            grid.opt[step.iter, eval(as.character(value.iter)) := value.curr]
            values.init[value.iter] <- value.curr
            
            # speed up if needed
            
            if (nrow(grid.opt[step.type == "xpl"]) > 3) {
              
              tmp <- grid.opt[step.type == "xpl", eval(as.name(paste0(value.iter, "__step")))]
              tmp <- tmp[(length(tmp)-3):length(tmp)]
              # browser()
              
              if (max(tmp) == min(tmp)) {
                
                lrate.init <- lrate.init * 2
                
              }
              
            }
            
          } else {
            
            if (value.iter == max(value.tune.wrk)) {
              
              lrate.init <- lrate.init * .5
              
            }
            
            values.init[value.iter] <- value.back
            
          }
          
        }
        
        tmp <- grid.opt[step.iter, c("step.id", paste0(values.opt.ind, "__step")), with = FALSE]
        tmp <- melt(tmp, id.vars = "step.id", measure.vars = paste0(values.opt.ind, "__step"), variable.factor = FALSE)
        
        value.tune.wrk <- as.integer(str_extract(unlist(tmp[, variable]), "^[0-9]+"))
        
        if (lrate.init < objective.fn.args$value.threshold)
          value.tune.wrk <- numeric()
        
        if (length(value.tune.wrk) > 0) {
          
          value.iter <- which(value.tune.wrk == value.iter) + 1
          
          if (length(value.iter) == 0)
            value.iter <- 1
          
          if (value.iter > length(value.tune.wrk)) {
            
            value.iter <- value.tune.wrk[1]
            
          } else {
            
            value.iter <- value.tune.wrk[value.iter]
            
          }
          
          if (grid.opt[step.iter, closed] == length(value.tune.wrk)) {
            
            step.iter <- step.iter + 1
            
          }
          
        }
        
      } else if (grid.opt[step.iter, step.type] == "ptrn") {
        
        # pattern move
        
        value.back <- values.init
        
        step.success.xpl.prev <- tail(which(grid.opt[, step.type] != "ptrn"), 2)[1]
        
        values.init[values.opt.ind] <-
          unlist(
            grid.opt[step.iter, c(as.character(values.opt.ind)), with = FALSE] +
              grid.opt[step.success, c(as.character(values.opt.ind)), with = FALSE] -
              grid.opt[step.success.xpl.prev, c(as.character(values.opt.ind)), with = FALSE]
          )
        
        err.curr <- objective.fn(values.init, method, objective.fn.args)
        
        if (err.curr < err.base) {
          
          grid.opt[step.iter, c(as.character(values.opt.ind))] <- as.list(values.init[values.opt.ind])
          grid.opt[step.iter, err := err.curr]
          
        } else {
          
          grid.opt[step.iter, c(as.character(values.opt.ind))] <- as.list(value.back[values.opt.ind])
          grid.opt[step.iter, err := err.base]
          values.init <- value.back
          
        }
        
        grid.opt[step.iter, closed := length(value.tune.wrk)]
        
        step.iter <- step.iter + 1
        
      } else if (grid.opt[step.iter, step.type] == "ptrn.adj") {
        
        # after pattern move
        
        step <- grid.opt[step.iter, eval(as.name(paste0(value.iter, "__step")))]
        
        dt.step <- data.table(value = value.back, err = err.base)
        
        for (i in 1:2) {
          
          sgn <- i %% 2
          if (sgn == 0) sgn <- -1
          
          value.curr <- value.back + step * ((i + 1) %/% 2) * sgn * lrate.init
          values.init[value.iter] <- value.curr
          
          dt.step <- rbind(dt.step
                           , list(value.curr, objective.fn(values.init, method, objective.fn.args))
                           , use.names = FALSE)
          
        }
        
        value.curr <- dt.step[err == min(err), value][1]
        err.curr <- dt.step[, min(err)]
        
        if (mode[1] == "debug")
          print(paste(step.iter, value.iter, value.curr, value.back, step))
        
        if (err.curr < err.base) {
          
          grid.opt[step.iter, `:=`(err = err.curr, closed = closed + 1)]
          grid.opt[step.iter, eval(as.character(value.iter)) := value.curr]
          values.init[value.iter] <- value.curr
          
        } else {
          
          grid.opt[step.iter, `:=`(err = err.base, closed = closed + 1)]
          values.init[value.iter] <- value.back
          
        }
        
        tmp <- grid.opt[step.iter, c("step.id", paste0(values.opt.ind, "__step")), with = FALSE]
        tmp <- melt(tmp, id.vars = "step.id", measure.vars = paste0(values.opt.ind, "__step"), variable.factor = FALSE)
        
        value.tune.wrk <- as.integer(str_extract(unlist(tmp[, variable]), "^[0-9]+"))
        
        if (lrate.init < objective.fn.args$value.threshold)
          value.tune.wrk <- numeric()
        
        
        if (length(value.tune.wrk) > 0) {
          
          value.iter <- which(value.tune.wrk == value.iter) + 1
          
          if (length(value.iter) == 0)
            value.iter <- 1
          
          if (value.iter > length(value.tune.wrk)) {
            
            value.iter <- value.tune.wrk[1]
            
          } else {
            
            value.iter <- value.tune.wrk[value.iter]
            
          }
          
          if (grid.opt[step.iter, closed] == length(value.tune.wrk)) {
            
            step.iter <- step.iter + 1
            
          }
          
        }
        
      }
      
      # check conditions
      
      if (length(value.tune.wrk) == 0) {
        
        break
        
      }
      
      if (mode[1] == "grid" & step.iter > 100) {
        
        break
        
      }
      
    }
    
    # return
    
    list(grid.opt = grid.opt, values.tuned = values.init, lrate.fin = lrate.init)

  }
  
}



# objective function --------------------------------------- #

ht.objective.function <- function(metrics = "sse", mode = c("iterator", "debug", "postproc"), dt.list = list()) {
  
  work.fn <- function(values.tuned, method, objective.fn.args) {
    
    # insert tuned values into the constant vector
    
    cnst.m <- objective.fn.args$cnst.m
    values.tuned.ind <- objective.fn.args$values.tuned.ind
    
    cnst.m[values.tuned.ind] <- values.tuned
    
    # run equilibrium evaluator ------------------------- #
    
    dt.res.m <- newton.wrapper(cnst.m, dt.list$dt.coef.m, dt.list$dt.conc.m, dt.list$part.eq, dt.list$reac.nm
                               , objective.fn.args$eq.thr.type[1], objective.fn.args$eq.threshold)
    colnames(dt.res.m) <- dt.list$dt.coef[, name]
    
    if (any(is.na(dt.res.m)))
      return(list(err = 1e+12))
    
    #  run partial molar properties evaluator ----------- #
    
    conc.series <- objective.fn.args$conc.series

    # diff concentrations
    
    volumes.exp <- dt.list$dt.heat[, volumes]
    if (dt.list$calorimeter.type %in% c("dsc", "ampoule")) volumes.exp <- c(dt.list$init.vol, volumes.exp)
    
    dt.res.diff <- dt.res.m
    
    for (i in seq_along(conc.series)) {
      
      if (i > 1 && conc.series[i] == conc.series[i - 1]) {

        ser.ind <- which(unique(conc.series) == conc.series[i])
        
        if (dt.list$calorimeter.type %in% "overfilled") {
          
          dt.res.diff[i, ] <- dt.res.m[i, ] * dt.list$init.vol - dt.res.m[i - 1, ] * (dt.list$init.vol - volumes.exp[i - ser.ind])
          # dt.res.diff[i, ] <- (dt.res.m[i, ] - dt.res.m[i - 1, ] * (1 - volumes.exp[i - ser.ind] / dt.list$init.vol)) * (dt.list$init.vol)

        } else if (dt.list$calorimeter.type %in% c("dsc", "ampoule")) {
          
          dt.res.diff[i, ] <- dt.res.m[i, ] - dt.res.m[i - 1, ] * volumes.exp[i - ser.ind] / volumes.exp[i - ser.ind + 1]
          
        }
        
      }
      
    }
    
    dt.res.diff <- dt.res.diff[which(conc.series == shift(conc.series)), ]
    
    dt.res.diff <- dt.res.diff * dt.list$calorimeter.type.coef
    if (dt.list$calorimeter.type %in% c("dsc", "ampoule")) dt.res.diff <- dt.res.diff * dt.list$dt.heat[, volumes]
    
    # input for lm evaluator
    
    y.raw <- dt.list$dt.heat[, heats]
    
    x.known <- dt.list$dt.enth[, value]
    names(x.known) <- dt.list$dt.enth[, reaction]
    
    wght <- sum((dt.list$dt.heat[, deviation] ^ 2)) / ((dt.list$dt.heat[, deviation] ^ 2) * length(dt.list$dt.heat[, deviation]))
    
    # run lm evaluator
    
    rtrn <- ht.enth.evaluator(x.known, y.raw, dt.res.diff, wght, method, mode)
    
    dt.enth.calc <- as.data.table(rtrn$enth)
    setnames(dt.enth.calc, "value")
    dt.enth.calc[, reaction := names(rtrn$enth)]
    
    dt.heat.calc <- as.data.table(rtrn$y.calc)
    setnames(dt.heat.calc, "heats")
    
    # evaluate cost function
    
    if (metrics == "sse") {
      
      err <- sum(((dt.list$dt.heat[, heats] - dt.heat.calc[, heats]) ^ 2) * wght)
      
    } else if (metrics == "mse") {
      
      err <- mean(((dt.list$dt.heat[, heats] - dt.heat.calc[, heats]) ^ 2) * wght)
      
    } else if (metrics == "mae") {
      
      err <- mean(abs(dt.list$dt.heat[, heats] - dt.heat.calc[, heats]) * wght)
      
    }
    
    # return
    
    if (mode[1] == "iterator") {
      
      err
      
    } else if (mode[1] == "debug") {
      
      list(err = err, dt.enth.calc = dt.enth.calc, dt.heat.calc = dt.heat.calc)
      
    } else if (mode[1] == "postproc") {
      
      if (is.null(rtrn$enth.dev)) rtrn$enth.dev <- 0 # if all enthalpies were known before and so NULL is returned
      dt.enth.calc[, dev := rtrn$enth.dev]
      
      dt.heat.calc[, error := heats - dt.list$dt.heat[, heats]]
      
      list(err = err, dt.enth.calc = dt.enth.calc, dt.heat.calc = dt.heat.calc)
      
    }
    
  }
  
}


# enthalpies evaluator ------------------------------------- #

ht.enth.evaluator <- function(x.known = NULL, y.raw, dt.x, wght, method = c("lm", "basic wls"), mode = c("base", "posptroc")) {
  
  # if some molar coefficients already known
  
  if (!is.null(x.known)) {
    
    cln.known <- names(x.known)
    
    # subtract already known from y
    
    x.known.v <- dt.x[, cln.known, drop = FALSE] %*% x.known
    y <- as.vector(y.raw - x.known.v)
    
    if (length(cln.known) == ncol(dt.x)) {
      
      x.known.v <- as.vector(x.known.v)
      
      if (mode[1] == "postproc") {
        
        return(list(enth = x.known, y.calc = x.known.v, enth.dev = NULL))
        
      } else {
        
        return(list(enth = x.known, y.calc = x.known.v))
        
      }
      
    }
    
  } else {
    
    cln.known <- ""
    y <- as.vector(y.raw)
    x.known.v <- rep(0, length(y))
    
  }

  
  # prepare data set from updated y and still unknown molar ext. coeff-s
  
  cln.unknown <- colnames(dt.x)
  cln.unknown <- setdiff(cln.unknown, cln.known)
  
  cln.unknown <- colSums(dt.x[, cln.unknown, drop = FALSE])
  cln.unknown <- cln.unknown[cln.unknown > 0]
  cln.unknown <- names(cln.unknown)
  
  # run linear model and get coefficients from it
  
  if (method[1] == "lm") {
    
    # classic R (weighted) lm linear model
    
    dt <- data.table(y = y, dt.x[, cln.unknown, drop = FALSE])
    
    frm <- paste("y ~ 0 +", paste(paste0("`", cln.unknown, "`"), collapse = "+"))
    frm <- as.formula(frm)
    
    md <- lm(frm, dt, weights = wght)
    
    enth.new <- md$coefficients
    names(enth.new) <- str_replace_all(names(enth.new), "`", "")
    
    y.calc <- predict(md) + x.known.v
    
    if (mode[1] == "postproc") {
      
      enth.dev <- summary(md)$coef[, "Std. Error"]
      
    }
    
  } else if (method[1] == "basic wls") {
    
    # basic (weighted) least squares
    
    dt.m <- dt.x[, cln.unknown, drop = FALSE]
    
    enth.new <- ginv((t(dt.m) %*% diag(wght)) %*% dt.m, tol = 0) %*% ((t(dt.m) %*% diag(wght)) %*% y)
    
    enth.new <- as.vector(enth.new)
    names(enth.new) <- cln.unknown
    
    y.calc <- as.vector(dt.x[, cln.unknown, drop = FALSE] %*% enth.new + x.known.v)
    
    if (mode[1] == "postproc") {
      
      enth.dev <- (diag(sum((y.raw - y.calc) ^ 2)/(length(y) - length(cln.unknown)) * ginv((t(dt.m)) %*% diag(wght) %*% dt.m, tol = 0))) ^ .5
      
    }
  }
  
  # fill NAs (0 actually)
  
  enth.new[is.na(enth.new)] <- 0
  # enth.new <- -enth.new
  
  # now the trick to place them all in the original order (too dirty maybe)
  
  enth <- rep(1, ncol(dt.x))
  names(enth) <- colnames(dt.x)
  
  if (!is.null(x.known)) {
    
    for (j in names(x.known))
      enth[names(enth) == j] <- x.known[names(x.known) == j]
    
  }
  
  for (j in names(enth.new))
    enth[names(enth) == j] <- enth.new[names(enth.new) == j]
  
  if (mode[1] == "postproc") {
    
    enth.dev.full <- rep(0, length(enth))
    names(enth.dev.full) <- colnames(dt.x)
    
    names(enth.dev) <- cln.unknown
    for (j in names(enth.dev))
      enth.dev.full[names(enth.dev.full) == j] <- enth.dev[names(enth.dev) == j]
    
    list(enth = enth, y.calc = y.calc, enth.dev = enth.dev.full)
    
  } else {
    
    list(enth = enth, y.calc = y.calc) 
    
  }
  
}






