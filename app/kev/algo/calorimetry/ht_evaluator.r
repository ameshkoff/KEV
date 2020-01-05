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
                                  , start.values
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

kev.direct.search <- function(values.calc.m
                              , value.opt.ind = 1:1
                              , objective.fn.extended = function(){1}
                              , hardstop = 1000
                              , mode = c("base", "grid", "debug")
                              , method = c("lm", "basic wls")
                              , algorithm = c("direct search", "basic search")
                              , lrate.init) {
  
  kev.direct.search.work <- function() {
    
    # first step ---------------------------- #
    
    grid.opt <- data.table(step.id = integer(), step.type = character(), err = numeric(), closed = integer())
    
    for (i in values.opt.ind) {
      
      grid.opt[, eval(as.character(i)) := numeric()]
      grid.opt[, paste0(eval(i), "__step") := numeric()]
      
    }
    
    grid.opt <- rbind(grid.opt, list(step.id = 1, step.type = "xpl", closed = length(values.opt.ind)), use.names = TRUE, fill = TRUE)
    
    for (i in values.opt.ind) {
      
      grid.opt[step.id == 1, eval(as.character(i)) := values.calc.m[i]]
      grid.opt[, eval(paste0(i, "__step")) := eval(as.name(as.character(i)))]
      
    }
    
    # get starting error
    
    err.v <- objective.fn.extended(values.calc.m, method)
    grid.opt[, err := err.v]

    # loop --------------------------------- #
    
    step.iter <- 2
    cnst.iter <- values.opt.ind[1]
    cnst.tune.wrk <- values.opt.ind
    
    for (j in 1:(hardstop)) {
      
      cnst.back <- values.calc.m[cnst.iter]
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
        
        for (k in values.opt.ind) {
          
          step.tmp <- grid.opt[step.success, eval(as.name(paste0(k, "__step")))]
          grid.opt[step.iter, eval(paste0(k, "__step")) := step.tmp]
          grid.opt[step.iter, eval(as.character(k)) := values.calc.m[k]]
          
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
          values.calc.m[cnst.iter] <- cnst.curr
          
          dt.step <- rbind(dt.step
                           , list(cnst.curr, objective.fn.extended(values.calc.m, method))
                           , use.names = FALSE)
          
        }
        
        cnst.curr <- dt.step[err == min(err), cnst][1]
        err.curr <- dt.step[, min(err)]
        
        
        if (mode[1] == "debug")
          print(paste(step.iter, cnst.iter, cnst.curr, cnst.back, step))
        
        if (mode[1] == "grid") {
          
          grid.opt[step.iter, `:=`(err = dt.step[2, err], closed = length(cnst.tune.wrk))]
          grid.opt[step.iter, eval(as.character(cnst.iter)) := dt.step[2, cnst]]
          values.calc.m[cnst.iter] <- dt.step[2, cnst]
          
        } else {
          
          if (err.curr < err.base) {
            
            grid.opt[step.iter, `:=`(err = err.curr, closed = closed + 1)]
            grid.opt[step.iter, eval(as.character(cnst.iter)) := cnst.curr]
            values.calc.m[cnst.iter] <- cnst.curr
            
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
            
            values.calc.m[cnst.iter] <- cnst.back
            
          }
          
        }
        
        tmp <- grid.opt[step.iter, c("step.id", paste0(values.opt.ind, "__step")), with = FALSE]
        tmp <- melt(tmp, id.vars = "step.id", measure.vars = paste0(values.opt.ind, "__step"), variable.factor = FALSE)
        
        cnst.tune.wrk <- as.integer(str_extract(unlist(tmp[, variable]), "^[0-9]+"))
        
        if (lrate.init < const.threshold)
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
        
        cnst.back <- values.calc.m
        
        step.success.xpl.prev <- tail(which(grid.opt[, step.type] != "ptrn"), 2)[1]
        
        values.calc.m[values.opt.ind] <-
          unlist(
            grid.opt[step.iter, c(as.character(values.opt.ind)), with = FALSE] +
              grid.opt[step.success, c(as.character(values.opt.ind)), with = FALSE] -
              grid.opt[step.success.xpl.prev, c(as.character(values.opt.ind)), with = FALSE]
          )
        
        err.curr <- objective.fn.extended(values.calc.m, method)
        
        if (err.curr < err.base) {
          
          grid.opt[step.iter, c(as.character(values.opt.ind))] <- as.list(values.calc.m[values.opt.ind])
          grid.opt[step.iter, err := err.curr]
          
        } else {
          
          grid.opt[step.iter, c(as.character(values.opt.ind))] <- as.list(cnst.back[values.opt.ind])
          grid.opt[step.iter, err := err.base]
          values.calc.m <- cnst.back
          
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
          values.calc.m[cnst.iter] <- cnst.curr
          
          dt.step <- rbind(dt.step
                           , list(cnst.curr, objective.fn.extended(values.calc.m, method))
                           , use.names = FALSE)
          
        }
        
        cnst.curr <- dt.step[err == min(err), cnst][1]
        err.curr <- dt.step[, min(err)]
        
        if (mode[1] == "debug")
          print(paste(step.iter, cnst.iter, cnst.curr, cnst.back, step))
        
        if (err.curr < err.base) {
          
          grid.opt[step.iter, `:=`(err = err.curr, closed = closed + 1)]
          grid.opt[step.iter, eval(as.character(cnst.iter)) := cnst.curr]
          values.calc.m[cnst.iter] <- cnst.curr
          
        } else {
          
          grid.opt[step.iter, `:=`(err = err.base, closed = closed + 1)]
          values.calc.m[cnst.iter] <- cnst.back
          
        }
        
        tmp <- grid.opt[step.iter, c("step.id", paste0(values.opt.ind, "__step")), with = FALSE]
        tmp <- melt(tmp, id.vars = "step.id", measure.vars = paste0(values.opt.ind, "__step"), variable.factor = FALSE)
        
        cnst.tune.wrk <- as.integer(str_extract(unlist(tmp[, variable]), "^[0-9]+"))
        
        if (lrate.init < const.threshold)
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
    
    # return
    
    list(grid.opt = grid.opt, values.calc.m = values.calc.m, lrate.fin = lrate.init)

  }
  
}



# objective function --------------------------------------- #

ht.objective.function <- function() {
  
  
  
}


# enthalpies evaluator ------------------------------------- #

ht.enth.evaluator <- function() {
  
  
  
}






