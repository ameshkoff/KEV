# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #



# ---------------------- load libraries ----------------------

# I/O
# data structure
library(data.table)
# computation
library(MASS)
library(Matrix)
library(Hmisc)
# strings
library(stringi)
library(stringr)



# runner -------------------------------------------- #

eq.evaluation.runner <- function(mode = c("api", "script", "app")
                                 , sep = ";"
                                 , subdir = ""
                                 , bs.name = "molecule1"
                                 , thr.type = c("rel", "abs")
                                 , threshold = 1e-08
                                 , save.res = TRUE
                                 , dt.list = NULL
                                 , pc.name = NULL) {
  
  dir.start <- ""
  
  if (mode %in% c("script", "api"))
    dir.start <- "app/kev/"

  source(paste0(dir.start, "eq_data.r"), chdir = TRUE)
  source(paste0(dir.start, "eq_preproc.r"), chdir = TRUE)
  source(paste0(dir.start, "eq_evaluator.r"), chdir = TRUE)
  source(paste0(dir.start, "eq_postproc.r"), chdir = TRUE)
  source(paste0(dir.start, "eq_save.r"), chdir = TRUE)

  # load data
  
  if (mode == "script") {
    
    dt.ttl <- eq.scripts.load(sep, subdir)
    
  } else if (mode %in% c("app", "api")) {
    
    dt.ttl <- dt.list
    
  }
  
  dt.coef <- dt.ttl[["dt.coef"]]
  dt.conc <- dt.ttl[["dt.conc"]]
  cnst <- dt.ttl[["cnst"]]
  part.eq <- dt.ttl[["part.eq"]]
  
  # preproc data
  
  dt.ttl <- eq.preproc(dt.coef, cnst, dt.conc, part.eq)
  
  dt.coef <- dt.ttl[["dt.coef"]]
  dt.conc <- dt.ttl[["dt.conc"]]
  cnst.m <- dt.ttl[["cnst.m"]]
  part.eq <- dt.ttl[["part.eq"]]
  dt.coef.m <- dt.ttl[["dt.coef.m"]]
  dt.conc.m <- dt.ttl[["dt.conc.m"]]
  reac.nm <- dt.ttl[["reac.nm"]]
  part.nm <- dt.ttl[["part.nm"]]
  
  # run evaluator
  
  dt.res.m <- newton.wrapper(cnst.m, dt.coef.m, dt.conc.m, part.eq, reac.nm, thr.type, threshold)
  dt.res <- data.table(dt.res.m)
  
  setnames(dt.res, dt.coef[, name])
  
  # postprocessing
  
  dt.frac <- eq.cond.fractions(dt.res, bs.name, dt.coef, dt.coef.m, dt.conc.m, pc.name)
  dt.conc.calc <- eq.tot.conc.calc(dt.res, cnst.m, dt.coef.m, part.nm)
  dt.err <- eq.residuals(dt.conc.m, dt.conc.calc, part.eq)
  
  dt.conc.tot <- copy(dt.conc.m)
  dt.conc.tot[, part.eq] <- dt.conc.calc[, part.eq]
  
  # save
  
  if (mode == "script" & save.res) {
    
    eq.save(subdir, sep, dt.res, dt.frac, dt.err, bs.name)  
    
  }
  
  # return data
  
  if (mode %in% c("script", "api")) {
    
    list("dt.eq.conc" = dt.res
         , "dt.conc.input" = dt.conc.m
         , "dt.conc.tot" = dt.conc.tot
         , "dt.conc.calc" = dt.conc.calc
         , "dt.frac" = dt.frac
         , "dt.coef.m" = dt.coef.m
         , "part.eq" = part.eq
         , "k.cnst.ln" = cnst.m
         , "dt.err" = as.data.table(dt.err))
    
  } else {
    
    list("dt.res" = dt.res
         , "dt.frac" = dt.frac
         , "dt.err" = as.data.table(dt.err)
         , "dt.conc.tot" = as.data.table(dt.conc.tot))
    
  }
  
}









