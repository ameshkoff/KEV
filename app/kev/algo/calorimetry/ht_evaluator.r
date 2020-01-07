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
                                                             , const.threshold = 5e-5
                                                             , eq.threshold = 1e-08
                                                             , eq.thr.type = c("rel", "abs"))
                                  , metrics = "mse"
                                  , mode = c("base", "grid", "debug")
                                  , verbose = TRUE) {
  
  
  
  
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
  
  kev.direct.search.work <- function() {
    
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
        
        # browser()
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
        
        
        for (i in 1:(search.density * 2)) {
          
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
        
        if (lrate.init < const.threshold)
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
        
        if (lrate.init < const.threshold)
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

ht.objective.function <- function() {
  
  work.fn <- function(values.tuned, method, objective.fn.args) {
    
    # insert tuned values into the constant vector
    
    cnst.m <- objective.fn.args$cnst.m
    values.tuned.ind <- objective.fn.args$values.tuned.ind
    
    cnst.m[values.tuned.ind] <- values.tuned
    
    # run equilibrium evaluator
    
    dt.res.m <- newton.wrapper(cnst.m, dt.coef.m, dt.conc.m, part.eq, reac.nm, eq.thr.type[1], eq.threshold)
    colnames(dt.res.m) <- dt.coef[, name]
    
    if (any(is.na(dt.res.m)))
      return(list(err = 1e+12))
    
    cnst.tune.nm <- which(colnames(dt.res.m) %in% cnst.tune)
    
    #  run partial molar properties evaluator
    
    
    
    
    
  }
  
}


# enthalpies evaluator ------------------------------------- #

ht.enth.evaluator <- function() {
  
  
  
}






