# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2020                                                 #
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

# ht.evaluation.runner <- function(
                                   mode = "script" #c("api", "script", "app")
                                  sep = "," #";"
                                  subdir = "calorimetry/ds.3.ampoule"
                                  eq.thr.type = c("rel", "abs")
                                  eq.threshold = 1e-08
                                  cnst.tune = NULL
                                  cmp.tune = NULL
                                  algorithm = "direct search"
                                  method = "basic wls"
                                  ht.mode = c("base", "grid", "debug")
                                  search.density = 1
                                  lrate.init = .5
                                  ht.threshold = 5e-7
                                  save.res = TRUE
                                  dt.list = NULL
                                  filename = "data.xlsx"#) {
  
  #
  
  ht.threshold <- log(10 ^ ht.threshold)
  
  
  # source code ------------- #
  
  dir.start <- ""
  
  if (mode[1] %in% c("script", "api")) {
    
    dir.start <- "app/kev/algo/"
    
  } else {
    
    dir.start <- "algo/"
    
  }
  
  source(paste0(dir.start, "concentrations/eq_data.r"), chdir = TRUE)
  source(paste0(dir.start, "calorimetry/ht_data.r"), chdir = TRUE)
  
  source(paste0(dir.start, "concentrations/eq_preproc.r"), chdir = TRUE)
  source(paste0(dir.start, "calorimetry/ht_preproc.r"), chdir = TRUE)
   
  # source(paste0(dir.start, "concentrations/eq_evaluator.r"), chdir = TRUE)
  # source(paste0(dir.start, "calorimetry/ht_evaluator.r"), chdir = TRUE)
  # 
  # source(paste0(dir.start, "concentrations/eq_postproc.r"), chdir = TRUE)
  # source(paste0(dir.start, "calorimetry/ht_postproc.r"), chdir = TRUE)
  # 
  # source(paste0(dir.start, "calorimetry/ht_save.r"), chdir = TRUE)
  
  
  # load data ---------------- #
  
  if (mode[1] == "script") {
    
    dt.ttl <- c(eq.scripts.load(sep, subdir, filename)
                , ht.scripts.load(sep, subdir, filename))
    
  } else if (mode %in% c("app", "api")) {
    
    dt.ttl <- dt.list
    
  }
  
  dt.coef <- dt.ttl[["dt.coef"]]
  dt.conc <- dt.ttl[["dt.conc"]]
  cnst <- dt.ttl[["cnst"]]
  part.eq <- dt.ttl[["part.eq"]]

  dt.heat <- dt.ttl[["dt.heat"]]
  dt.enth <- dt.ttl[["dt.enth"]]
  
  if (is.null(cnst.tune))
    cnst.tune <- dt.ttl[["cnst.tune"]]
  
  if (is.null(cmp.tune))
    cmp.tune <- dt.ttl[["cmp.tune"]]
  
  calorimeter.type <- dt.ttl[["calorimeter.type"]]
  init.vol <- dt.ttl[["init.vol"]]
  
  
  # preproc data --------------- #
  
  dt.ttl <- eq.preproc(dt.coef, cnst, dt.conc, part.eq)
  
  dt.coef <- dt.ttl[["dt.coef"]]
  dt.conc <- dt.ttl[["dt.conc"]]
  cnst.m <- dt.ttl[["cnst.m"]]
  part.eq <- dt.ttl[["part.eq"]]
  dt.coef.m <- dt.ttl[["dt.coef.m"]]
  dt.conc.m <- dt.ttl[["dt.conc.m"]]
  reac.nm <- dt.ttl[["reac.nm"]]
  part.nm <- dt.ttl[["part.nm"]]
  conc.series <- dt.ttl[["conc.series"]]
  
  dt.ttl <- c(dt.ttl, ht.preproc(dt.heat, dt.enth, dt.coef, dt.conc.m, conc.series, calorimeter.type, cmp.tune))
  
  dt.heat <- dt.ttl[["dt.heat"]]
  dt.enth <- dt.ttl[["dt.enth"]]
  calorimeter.type <- dt.ttl[["calorimeter.type"]]
  calorimeter.type.coef <- dt.ttl[["calorimeter.type.coef"]]

  cnst.tune.nm <- which(dt.coef[, name] %in% cnst.tune)
  
  
  