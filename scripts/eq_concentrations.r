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





# load data --------------------------------- #

dt.load <- function(subdir = "", sep = ";", tbl) {

  if (subdir != "")
    subdir <- paste0("/", subdir, "/")
  
  if (sep == ";") {
    
    assign(tbl[1], as.data.table(read.csv2(paste0("input", subdir, "k_constants_log10.csv"), stringsAsFactors = FALSE, colClasses = "character")
                                 , keep.rownames = FALSE), envir = .GlobalEnv)
    assign(tbl[2], as.data.table(read.csv2(paste0("input", subdir, "stech_coefficients.csv"), stringsAsFactors = FALSE, colClasses = "character")
                                    , keep.rownames = FALSE), envir = .GlobalEnv)
    assign(tbl[3], as.data.table(read.csv2(paste0("input", subdir, "concentrations.csv")
                                           , stringsAsFactors = FALSE, colClasses = "character", skip = 1)
                                    , keep.rownames = FALSE), envir = .GlobalEnv)
    assign("part.eq", as.data.table(read.csv2(paste0("input", subdir, "concentrations.csv")
                                           , stringsAsFactors = FALSE, colClasses = "character", header = FALSE , nrows = 1)
                                 , keep.rownames = FALSE), envir = .GlobalEnv)
    
  } else if (sep == ",") {
    
    assign(tbl[1], as.data.table(read.csv(paste0("input", subdir, "k_constants_log10.csv"), stringsAsFactors = FALSE, colClasses = "character")
                                 , keep.rownames = FALSE), envir = .GlobalEnv)
    assign(tbl[2], as.data.table(read.csv(paste0("input", subdir, "stech_coefficients.csv"), stringsAsFactors = FALSE, colClasses = "character")
                                    , keep.rownames = FALSE), envir = .GlobalEnv)
    assign(tbl[3], as.data.table(read.csv(paste0("input", subdir, "concentrations.csv"), stringsAsFactors = FALSE, colClasses = "character")
                                    , keep.rownames = FALSE), envir = .GlobalEnv, skip = 1)
    assign("part.eq", as.data.table(read.csv2(paste0("input", subdir, "concentrations.csv")
                                              , stringsAsFactors = FALSE, colClasses = "character", header = FALSE , nrows = 1)
                                    , keep.rownames = FALSE), envir = .GlobalEnv)
    
  }
  
  
}


# preprocessing ----------------------------- #

dt.preproc <- function(sep = ";", tbl) {
  
  # scalars
  
  assign("part.nm", ncol(dt.coef), envir = .GlobalEnv)
  assign("reac.nm", nrow(dt.coef) + part.nm, envir = .GlobalEnv)
  
  cnst <- rbind(rep(0, part.nm), cnst, use.names = FALSE)
  
  # complete coefficients data table
  
  cln <- colnames(dt.coef)
  
  assign("dt.coef", rbind(as.data.table(diag(part.nm)), dt.coef, use.names = FALSE), envir = .GlobalEnv)
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
  dt.coef[, name := str_replace(name, "^ *\\-", "-")]
  dt.coef[, name := paste(name, 1:.N, sep = "_"), name]
  
  # restore constants
  
  cnst.m <- (10 ^ cnst.m)
  assign("cnst.m", log(cnst.m), envir = .GlobalEnv)
  
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
    accr <- mean(abs(log(dt.conc.out) - log(tmp)))
    dt.conc.out <- tmp
    
    if (accr < 1e-08) {
      
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
    
    # print(i)
    
  }
  
  dt.res
  
}

# conditional fractions ------------------------------ #

cond.fractions <- function(dt.coef, dt.conc.m, dt.coef.m, dt.res, bs.name) {
  
  cln <- colnames(dt.res)
  cln <- cln[cln %like% bs.name]
  
  dt.frac <- t(round(100 * as.matrix(dt.res[, cln, with = FALSE]) /
                       (dt.conc.m[, bs.name] %*% t(dt.coef.m[dt.coef[, eval(as.name(bs.name))] != 0, bs.name])), 2))
  
  dt.frac <- data.table(dt.frac, keep.rownames = TRUE)
  
  tmp <- data.table(rn = "-lg(CB)", t(data.table(round(-log10(dt.res[, eval(as.name(paste0(bs.name, "_1")))]), 2))))
  
  dt.frac <- rbind(tmp, dt.frac)
  
  cln <- colnames(dt.frac)
  setnames(dt.frac, str_replace(cln, "^V", "S_"))
  
  dt.frac  
  
}

# save data to files -------------------------------- #

dt.save <- function(dt.res, dt.frac, bs.name, subdir, sep) {
  
  if (subdir != "") subdir <- paste0("/", subdir, "/")
  
  dir.create(file.path(paste0("output", subdir)), showWarnings = FALSE)
  
  if (sep == ";") {
    
    write.csv2(dt.res, file = paste0("output", subdir, "equilibrium_concentrations.csv"))
    write.csv2(dt.frac, file = paste0("output", subdir, bs.name, "_fractions.csv"))
    
  } else {
    
    write.csv(dt.res, file = paste0("output", subdir, "equilibrium_concentrations.csv"))
    write.csv(dt.frac, file = paste0("output", subdir, bs.name, "_fractions.csv"))
    
  }
  
}


# run -----------------------------------------------

tbl <- c("cnst", "dt.coef", "dt.conc")
sp <- ";"
sbd <- "ds.eq"

dt.load(subdir = sbd, sep = sp, tbl)
dt.preproc(sep = sp, tbl)

dt.res <- newton.wrapper(cnst.m, dt.coef.m, dt.conc.m, part.eq)
dt.res <- data.table(dt.res)

setnames(dt.res, dt.coef[, name])

# fractions
bs.name <- "molecule3"
dt.frac <- cond.fractions(dt.coef, dt.conc.m, dt.coef.m, dt.res, bs.name)


# show results and save

dt.save(dt.res, dt.frac, bs.name, sbd, sp)

remove(sp, tbl)

dt.res
dt.frac








