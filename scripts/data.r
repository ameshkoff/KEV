# ########################################################## #
#                                                            #
# Name: 
# Author: AMeshkov
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
library(gmp)
library(Rmpfr)
library(Hmisc)
# strings
library(stringi)
library(stringr)





# load data --------------------------------- #

dt.load <- function(subdir = "", sep = ";") {

  if (subdir != "")
    subdir <- paste0("/", subdir, "/")
  
  tbl <- c("cnst", "dt.coef", "dt.conc")
  
  if (sep == ";") {
    
    assign(tbl[1], as.data.table(read.csv2(paste0("data.raw", subdir, "k_constants_log10.csv"), stringsAsFactors = FALSE, colClasses = "character")
                                 , keep.rownames = FALSE), envir = .GlobalEnv)
    assign(tbl[2], as.data.table(read.csv2(paste0("data.raw", subdir, "stech_coefficients.csv"), stringsAsFactors = FALSE, colClasses = "character")
                                    , keep.rownames = FALSE), envir = .GlobalEnv)
    assign(tbl[3], as.data.table(read.csv2(paste0("data.raw", subdir, "concentrations.csv")
                                           , stringsAsFactors = FALSE, colClasses = "character", skip = 1)
                                    , keep.rownames = FALSE), envir = .GlobalEnv)
    assign("part.eq", as.data.table(read.csv2(paste0("data.raw", subdir, "concentrations.csv")
                                           , stringsAsFactors = FALSE, colClasses = "character", header = FALSE , nrows = 1)
                                 , keep.rownames = FALSE), envir = .GlobalEnv)
    
  } else if (sep == ",") {
    
    assign(tbl[1], as.data.table(read.csv(paste0("data.raw", subdir, "k_constants_log10.csv"), stringsAsFactors = FALSE, colClasses = "character")
                                 , keep.rownames = FALSE), envir = .GlobalEnv)
    assign(tbl[2], as.data.table(read.csv(paste0("data.raw", subdir, "stech_coefficients.csv"), stringsAsFactors = FALSE, colClasses = "character")
                                    , keep.rownames = FALSE), envir = .GlobalEnv)
    assign(tbl[3], as.data.table(read.csv(paste0("data.raw", subdir, "concentrations.csv"), stringsAsFactors = FALSE, colClasses = "character")
                                    , keep.rownames = FALSE), envir = .GlobalEnv, skip = 1)
    assign("part.eq", as.data.table(read.csv2(paste0("data.raw", subdir, "concentrations.csv")
                                              , stringsAsFactors = FALSE, colClasses = "character", header = FALSE , nrows = 1)
                                    , keep.rownames = FALSE), envir = .GlobalEnv)
    
  }
  
  
}


# preprocessing ----------------------------- #

dt.preproc <- function(prec = 106, sep = ";") {
  
  # scalars
  
  assign("part.nm", ncol(dt.coef), envir = .GlobalEnv)
  assign("reac.nm", nrow(dt.coef) + part.nm, envir = .GlobalEnv)
  
  cnst <- rbind(rep(0, part.nm), cnst, use.names = FALSE)
  
  # complete coefficients data table
  
  cln <- colnames(dt.coef)
  
  assign("dt.coef", rbind(as.data.table(diag(part.nm)), dt.coef, use.names = FALSE), envir = .GlobalEnv)
  setnames(dt.coef, cln)
  
  # matrices
  
  tbl <- c("cnst", "dt.coef", "dt.conc")
  
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
    
    assign(paste0(j, ".m"), f, envir = .GlobalEnv)
    
  }
  
  assign("part.eq", which(part.eq[1] == "eq"), envir = .GlobalEnv)
  
  # create names
  
  dt.coef[, name := ""]
  
  cln <- colnames(dt.coef)
  cln <- cln[cln != "name"]
  
  for (i in cln) {
    
    dt.coef[eval(as.name(i)) > 0, name := paste0(name, " + ", i)]
    dt.coef[eval(as.name(i)) < 0, name := paste0(name, " - ", i)]
    
  }
  
  dt.coef[, name := str_replace(name, "^ *\\+ *", "")]
  dt.coef[, name := paste(name, 1:.N, sep = "_"), name]
  
  # restore constants
  
  cnst.m <- (10 ^ cnst.m)
  # cnst.m <- c(rep(1, part.nm), cnst.m)
  assign("cnst.m", log(cnst.m), envir = .GlobalEnv)
  
}

# evaluating ------------------------------ #

newton.evaluator <- function(cnst.m, dt.coef.m, dt.conc.in, part.eq = integer(), max.it = 1000) {
  
  dt.conc.out <- copy(dt.conc.in)
  
  for (iter in 1:max.it) {
    
    if (length(part.eq) > 0) {
      
      dt.conc.out[part.eq] <- dt.conc.in[part.eq]
      
    }

    # base concentrations equation
    conc.base.res <- t(dt.coef.m) %*% exp(cnst.m + dt.coef.m %*% log(dt.conc.out))
    # browser()
    # product concentrations equation
    conc.prod.res <- exp(cnst.m + dt.coef.m %*% log(dt.conc.out))
    
    # jacobian matrix
    jc <- t(dt.coef.m) %*% (dt.coef.m * as.vector(conc.prod.res))
    
    # error vector
    err.v <- t(dt.coef.m) %*% conc.prod.res - dt.conc.in
    
    # step
    mlt <- 1e+0 # 1e+12
    
    tmp <- jc * mlt
    tmp <- ginv(tmp, tol = 0) * mlt
    # tmp <- solve(tmp) * mlt
    # tmp <- exp(log(dt.conc.out) - 1 * as.matrix(solve(Matrix(jc))) %*% err.v)
    tmp <- exp(log(dt.conc.out) - 1 * tmp %*% err.v)
    # tmp <- exp(log(dt.conc.out) - as.numeric(solve(jc) %*% err.v))
    
    # check accuracy
    accr <- mean(abs(log(dt.conc.out) - log(tmp)))
    dt.conc.out <- tmp
    
    if (accr < 1e-08) {
      
      # tmp <- exp(cnst.m + dt.coef.m %*% log(dt.conc.out))
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
    out <- newton.evaluator(cnst.m, dt.coef.m, dt.conc.in)
    
    dt.res <- rbind(dt.res, as.numeric(out[[1]]))
    
    # print(i)
    
  }
  
  dt.res
  
}


# run -----------------------------------------------

dt.load(subdir = "ds.5p", sep = ";")
dt.preproc(prec = 66, sep = ";")

dt.res <- newton.wrapper(cnst.m, dt.coef.m, dt.conc.m, part.eq)
dt.res <- data.table(dt.res)

setnames(dt.res, dt.coef[, name])

dt.res











