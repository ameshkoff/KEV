# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #



# ---------------------- load libraries ----------------------

# I/O
library(readr)
library(openxlsx)
# data structure
library(data.table)
# computation
library(MASS)
library(Matrix)
library(Hmisc)
# strings
library(stringi)
library(stringr)




eq.conc.exec <- function(sep = ";", subdir = "", bs.name = "molecule1", thr.type = c("rel", "abs"), threshold = 1e-08, verbose = FALSE) {
  
  # initialize and update helper variables
  
  tbl <- c("cnst", "dt.coef", "dt.conc")
  
  if (subdir != "")
    subdir <- paste0("/", subdir, "/")
 
   
  # load data --------------------------------- #
  
  dt.load <- function() {
    
    if (sep == ";") {
      
      assign(tbl[1], as.data.table(read.csv2(paste0("input", subdir, "k_constants_log10.csv"), stringsAsFactors = FALSE, colClasses = "character")
                                   , keep.rownames = FALSE), envir = parent.frame())
      assign(tbl[2], as.data.table(read.csv2(paste0("input", subdir, "stech_coefficients.csv"), stringsAsFactors = FALSE, colClasses = "character")
                                   , keep.rownames = FALSE), envir = parent.frame())
      assign(tbl[3], as.data.table(read.csv2(paste0("input", subdir, "concentrations.csv")
                                             , stringsAsFactors = FALSE, colClasses = "character", skip = 1)
                                   , keep.rownames = FALSE), envir = parent.frame())
      assign("part.eq", as.data.table(read.csv2(paste0("input", subdir, "concentrations.csv")
                                                , stringsAsFactors = FALSE, colClasses = "character", header = FALSE , nrows = 1)
                                      , keep.rownames = FALSE), envir = parent.frame())
      
    } else if (sep == ",") {
      
      assign(tbl[1], as.data.table(read.csv(paste0("input", subdir, "k_constants_log10.csv"), stringsAsFactors = FALSE, colClasses = "character")
                                   , keep.rownames = FALSE), envir = parent.frame())
      assign(tbl[2], as.data.table(read.csv(paste0("input", subdir, "stech_coefficients.csv"), stringsAsFactors = FALSE, colClasses = "character")
                                   , keep.rownames = FALSE), envir = parent.frame())
      assign(tbl[3], as.data.table(read.csv(paste0("input", subdir, "concentrations.csv"), stringsAsFactors = FALSE, colClasses = "character")
                                   , keep.rownames = FALSE), envir = parent.frame(), skip = 1)
      assign("part.eq", as.data.table(read.csv2(paste0("input", subdir, "concentrations.csv")
                                                , stringsAsFactors = FALSE, colClasses = "character", header = FALSE , nrows = 1)
                                      , keep.rownames = FALSE), envir = parent.frame())
      
    }
    
  }
  
  
  # preprocessing ----------------------------- #
  
  dt.preproc <- function() {
    
    # scalars
    
    assign("part.nm", ncol(dt.coef), envir = parent.frame())
    assign("reac.nm", nrow(dt.coef) + part.nm, envir = parent.frame())
    
    cnst <- rbind(rep(0, part.nm), cnst, use.names = FALSE)
    
    # complete coefficients data table
    
    cln <- colnames(dt.coef)
    
    assign("dt.coef", rbind(as.data.table(diag(part.nm)), dt.coef, use.names = FALSE), envir = parent.frame())
    setnames(dt.coef, cln)
    
    # matrices
    
    for (j in tbl) {
      
      f <- eval(as.name(j))
      
      if (sep == ";") {
        
        cln <- colnames(f)
        
        for (i in cln) {
          
          # replace commas with points
          f[, eval(i) := str_replace(eval(as.name(i)), "\\,", ".")]
          
        }
        
      }
      
      # to numbers
      
      f <- as.matrix(f)
      f <- apply(f, 2, as.numeric)
      
      assign(paste0(j, ".m"), f, envir = parent.frame())
      
    }
    
    assign("part.eq", which(part.eq[1] == "eq"), envir = parent.frame())
    
    # create names
    
    dt.coef[, name := ""]
    
    cln <- colnames(dt.coef)
    cln <- cln[cln != "name"]
    
    for (i in cln) {
      
      dt.coef[eval(as.name(i)) > 0, name := paste0(name, " + ", i)]
      dt.coef[eval(as.name(i)) < 0, name := paste0(name, " - ", i)]
      
    }
    
    dt.coef[, name := str_replace(name, "^ *\\+ *", "")]
    dt.coef[, name := str_replace(name, "^ *\\-", "-")]
    dt.coef[, name := paste(name, 1:.N, sep = "_"), name]
    
    # restore constants
    
    cnst.m <- (10 ^ cnst.m)
    assign("cnst.m", log(cnst.m), envir = parent.frame())
    
  }
  
  # evaluating ------------------------------ #
  
  newton.evaluator <- function(cnst.m, dt.coef.m, dt.conc.in, part.eq = integer(), max.it = 1000) {
    
    dt.conc.out <- copy(dt.conc.in)
    
    for (iter in 1:max.it) {
      
      if (length(part.eq) > 0)
        dt.conc.out[part.eq] <- dt.conc.in[part.eq]
      
      # base concentrations equation
      conc.base.res <- t(dt.coef.m) %*% exp(cnst.m + dt.coef.m %*% log(dt.conc.out))
      
      # product concentrations equation
      conc.prod.res <- exp(cnst.m + dt.coef.m %*% log(dt.conc.out))
      
      # jacobian matrix
      jc <- t(dt.coef.m) %*% (dt.coef.m * as.vector(conc.prod.res))
      
      # error vector
      err.v <- t(dt.coef.m) %*% conc.prod.res - dt.conc.in
      
      if (length(part.eq) > 0)
        err.v[part.eq] <- as.numeric(0)
      
      # step
      tmp <- exp(log(dt.conc.out) - 1 * ginv(jc, tol = 0) %*% err.v)
      
      # check accuracy
      if (thr.type == "rel") {
        
        accr <- mean(abs(1 - dt.conc.out / tmp))
        
      } else if (thr.type == "abs") {
        
        accr <- mean(abs(log(dt.conc.out) - log(tmp)))
        
      }
      
      dt.conc.out <- tmp
      
      if (accr < threshold) {
        
        break
        
      }
      
      # print steps for longer evaluation
      if (iter %% 100 == 0) print(iter)
      
      # iterator
      iter <- iter + 1
      
    }
    
    list(out = conc.prod.res, iter = iter)
    
  }
  
  # loop for every solution --------------------------- #
  
  newton.wrapper <- function(cnst.m, dt.coef.m, dt.conc.m, part.eq = integer()) {
    
    dt.res <- matrix(ncol = reac.nm, nrow = 0)
    
    for (i in 1:nrow(dt.conc.m)) {
      
      dt.conc.in <- copy(dt.conc.m[i, ])
      out <- newton.evaluator(cnst.m, dt.coef.m, dt.conc.in, part.eq)
      
      dt.res <- rbind(dt.res, as.numeric(out[[1]]))
      
    }
    
    dt.res
    
  }
  
  # conditional fractions ------------------------------ #
  
  cond.fractions <- function() {
    
    cln <- colnames(dt.res)
    cln <- cln[cln %like% bs.name]
    
    dt.frac <- t(round(100 * as.matrix(dt.res[, cln, with = FALSE]) /
                         (dt.conc.m[, bs.name] %*% t(dt.coef.m[dt.coef[, eval(as.name(bs.name))] != 0, bs.name])), 2))
    
    dt.frac <- data.table(dt.frac, keep.rownames = TRUE)
    
    tmp <- data.table(rn = paste0("-lg(C(", bs.name, "))"), t(data.table(round(-log10(dt.res[, eval(as.name(paste0(bs.name, "_1")))]), 2))))
    
    dt.frac <- rbind(tmp, dt.frac)
    
    cln <- colnames(dt.frac)
    setnames(dt.frac, str_replace(cln, "^V", "S_"))
    
    dt.frac  
    
  }
  
  # save data to files -------------------------------- #
  
  dt.save <- function() {
    
    dir.create(file.path(paste0("output", subdir)), showWarnings = FALSE)
    
    if (sep == ";") {
      
      write.csv2(dt.res, file = paste0("output", subdir, "equilibrium_concentrations.csv"))
      write.csv2(dt.frac, file = paste0("output", subdir, bs.name, "_fractions.csv"))
      write.csv2(dt.err, file = paste0("output", subdir, bs.name, "_percent_error.csv"))
      
    } else {
      
      write.csv(dt.res, file = paste0("output", subdir, "equilibrium_concentrations.csv"))
      write.csv(dt.frac, file = paste0("output", subdir, bs.name, "_fractions.csv"))
      write.csv(dt.err, file = paste0("output", subdir, bs.name, "_percent_error.csv"))
      
    }
    
  }
  
  
  # run --------------------------------------------- #
  

  dt.load()
  dt.preproc()
  
  dt.res.m <- newton.wrapper(cnst.m, dt.coef.m, dt.conc.m, part.eq)
  dt.res <- data.table(dt.res.m)
  
  setnames(dt.res, dt.coef[, name])
  
  # check errors
  
  dt.conc.calc <- t(exp(as.vector(cnst.m) + dt.coef.m %*% log(t(dt.res[, 1:part.nm])))) %*% dt.coef.m
  
  dt.err <- (dt.conc.calc - dt.conc.m) / dt.conc.m
  
  # fractions
  dt.frac <- cond.fractions()
  
  # show results and save
  
  dt.save()
  
  if (verbose) {
  
    print("== Equilibrium concentrations ==")
    print(dt.res)
    print("")
    print("")
    print(paste("== Fractions of", bs.name, "=="))
    print(dt.frac)
    
  } else {
    
    list("dt.eq.conc" = dt.res, "dt.frac" = dt.frac, "dt.coef.m" = dt.coef.m, "part.eq" = part.eq
         , "dt.bs.conc" = dt.conc.m, "k.cnst.ln" = cnst.m, "dt.err.m" = dt.err)
    
  }
  
}









