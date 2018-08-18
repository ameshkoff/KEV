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
library(Hmisc)
# strings
library(stringi)
library(stringr)





# load data --------------------------------- #

dt.load <- function(subdir = "", sep = ";") {

  if (subdir != "")
    subdir <- paste0("/", subdir, "/")
  
  if (sep == ";") {
    
    assign("cnst", as.data.table(read.csv2(paste0("data.raw", subdir, "k_constants_log10.csv"), stringsAsFactors = FALSE)
                                 , keep.rownames = FALSE), envir = .GlobalEnv)
    assign("dt.coef", as.data.table(read.csv2(paste0("data.raw", subdir, "stech_coefficients.csv"), stringsAsFactors = FALSE)
                                    , keep.rownames = FALSE), envir = .GlobalEnv)
    assign("dt.conc", as.data.table(read.csv2(paste0("data.raw", subdir, "concentrations.csv"), stringsAsFactors = FALSE)
                                    , keep.rownames = FALSE), envir = .GlobalEnv)
    
  } else if (sep == ",") {
    
    assign("cnst", as.data.table(read.csv(paste0("data.raw", subdir, "k_constants_log10.csv"), stringsAsFactors = FALSE)
                                 , keep.rownames = FALSE), envir = .GlobalEnv)
    assign("dt.coef", as.data.table(read.csv(paste0("data.raw", subdir, "stech_coefficients.csv"), stringsAsFactors = FALSE)
                                    , keep.rownames = FALSE), envir = .GlobalEnv)
    assign("dt.conc", as.data.table(read.csv(paste0("data.raw", subdir, "concentrations.csv"), stringsAsFactors = FALSE)
                                    , keep.rownames = FALSE), envir = .GlobalEnv)
    
  }
  
  
}


# preprocessing ----------------------------- #

dt.preproc <- function() {
  
  # scalars
  
  assign("cnst.nm", nrow(cnst), envir = .GlobalEnv)
  assign("part.nm", ncol(dt.coef), envir = .GlobalEnv)
  assign("reac.nm", nrow(dt.coef) + part.nm, envir = .GlobalEnv)
  
  # complete coefficients matrix
  
  cln <- colnames(dt.coef)
  
  assign("dt.coef", rbind(as.data.table(diag(part.nm)), dt.coef, use.names = FALSE), envir = .GlobalEnv)
  setnames(dt.coef, cln)
  
  # create names
  
  dt.coef[, name := ""]
  
  cln <- colnames(dt.coef)
  cln <- cln[cln != "name"]
  
  for (i in cln) {
    
    dt.coef[eval(as.name(i)) > 0, name := paste0(name, " + ", i)]
    dt.coef[eval(as.name(i)) < 0, name := paste0(name, " - ", i)]
    
  }
  
  dt.coef[, name := str_replace(name, "^ *\\+ *", "")]
  # dt.coef[name %like% "\\+", name := paste(name, 1:.N, sep = "_"), name]
  dt.coef[, name := paste(name, 1:.N, sep = "_"), name]
  
  # restore constants
  
  cnst <- (10 ^ unlist(cnst))
  cnst <- c(rep(1, part.nm), cnst)
  assign("cnst", log(cnst), envir = .GlobalEnv)
  
  # stechiometric coefficients to matrix
  
  assign("dt.coef.m", as.matrix(dt.coef[, !c("name"), with = FALSE]), envir = .GlobalEnv)
  
  # base concentrations to matrix
  
  tmp <- dt.conc[2:nrow(dt.conc), cln, with = FALSE]
  
  cln <- colnames(tmp)
  for (i in cln)
    tmp[, eval(i) := as.numeric(str_replace(eval(as.name(i)), "\\,", "."))]
  
  assign("dt.conc.m", as.matrix(tmp), envir = .GlobalEnv)
  
  assign("part.eq", which(dt.conc[1] == "eq"), envir = .GlobalEnv)
  
}

# evaluating ------------------------------ #

newton.evaluator <- function(cnst, dt.coef.m, dt.conc.in, part.eq = integer(), max.it = 1000) {
  
  dt.conc.out <- copy(dt.conc.in)
  
  for (iter in 1:max.it) {
    
    if (length(part.eq) > 0) {
      
      dt.conc.out[part.eq] <- dt.conc.in[part.eq]
      
    }
    
    # base concentrations equation
    conc.base.res <- as.bigq(t(dt.coef.m)) %*% as.bigq(exp(cnst + as.numeric(as.bigq(dt.coef.m) %*% as.bigq(log(dt.conc.out)))))
    
    # product concentrations equation
    conc.prod.res <- as.bigq(exp(cnst + as.numeric(as.bigq(dt.coef.m) %*% as.bigq(log(dt.conc.out)))))
    
    # jacobian matrix
    jc <- as.bigq(t(dt.coef.m)) %*% (as.bigq(dt.coef.m) * as.vector(conc.prod.res))
    
    # error vector
    err.v <- as.bigq(t(dt.coef.m)) %*% conc.prod.res - dt.conc.in

    # step
    # tmp <- exp(log(dt.conc.out) - 1 * as.matrix(solve(Matrix(jc))) %*% err.v)
    # tmp <- exp(log(dt.conc.out) - 1 * as.matrix(ginv(jc, tol = 0)) %*% err.v)
    tmp <- exp(log(dt.conc.out) - as.numeric(solve(jc) %*% err.v))
    
    # check accuracy
    accr <- mean(abs(log(dt.conc.out) - log(tmp)))
    dt.conc.out <- tmp
    
    if (accr < 1e-08) {
      
      # tmp <- exp(cnst + dt.coef.m %*% log(dt.conc.out))
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

newton.wrapper <- function(cnst, dt.coef.m, dt.conc.m, part.eq = integer()) {

  dt.res <- matrix(ncol = reac.nm, nrow = 0)
  
  for (i in 1:nrow(dt.conc.m)) {
    
    dt.conc.in <- copy(dt.conc.m[i, ])
    out <- newton.evaluator(cnst, dt.coef.m, dt.conc.in)
    
    dt.res <- rbind(dt.res, as.numeric(out[[1]]))
    
  }
  
  dt.res
  
}


# run -----------------------------------------------

dt.load(subdir = "ds.5p", sep = ";")
dt.preproc()

dt.res <- newton.wrapper(cnst, dt.coef.m, dt.conc.m, part.eq)
dt.res <- data.table(dt.res)

setnames(dt.res, dt.coef[, name])

dt.res











