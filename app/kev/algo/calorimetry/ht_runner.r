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
                                  subdir = "calorimetry/ds.4.cut.overfilled"
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
   
  source(paste0(dir.start, "concentrations/eq_evaluator.r"), chdir = TRUE)
  source(paste0(dir.start, "calorimetry/ht_evaluator.r"), chdir = TRUE)
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
  
  dt.ttl <- dt.ttl[names(dt.ttl) != "conc.series"]
  
  dt.ttl <- c(dt.ttl, ht.preproc(dt.heat, dt.enth, dt.coef, dt.conc.m, conc.series = conc.series, init.vol, calorimeter.type, cmp.tune))
  
  dt.heat <- dt.ttl[["dt.heat"]]
  dt.enth <- dt.ttl[["dt.enth"]]
  init.vol <- dt.ttl[["init.vol"]]
  calorimeter.type <- dt.ttl[["calorimeter.type"]]
  calorimeter.type.coef <- dt.ttl[["calorimeter.type.coef"]]
  conc.series <- dt.ttl[["conc.series"]]
  

  # run evaluator --------------- #

  cnst.tune.ind <- which(unlist(dt.coef[, name]) %in% cnst.tune)
  
  dt.list <- list(dt.coef = dt.coef
                  , cnst.m = cnst.m
                  , cnst.tune = cnst.tune
                  , values.tuned.ind = cnst.tune.ind
                  , dt.heat = dt.heat
                  , dt.enth = dt.enth
                  , calorimeter.type = calorimeter.type
                  , calorimeter.type.coef = calorimeter.type.coef
                  , init.vol = init.vol
                  , dt.coef.m = dt.coef.m
                  , dt.conc.m = dt.conc.m
                  , part.eq = part.eq
                  , reac.nm = reac.nm
                  , conc.series = conc.series)
  
  algorithm.options <- list(algorithm = "direct search"
                            , hardstop = 100
                            , lrate.init = .5
                            , search.density = 1
                            , value.threshold = 5e-5
                            , eq.threshold = 1e-08
                            , eq.thr.type = "rel")
  
  if (length(cnst.tune.ind) > 0) {
    
    exec.time <- system.time(
      dt.ttl <- kev.constant.optimizer(objective.fn = ht.objective.function
                                       , evaluation.fn = ht.enth.evaluator
                                       , values.init = cnst.m[cnst.tune.ind]
                                       , lower.bound = -Inf
                                       , upper.bound = Inf
                                       , dt.list = dt.list
                                       , algorithm.options = algorithm.options
                                       , metrics = "mse"
                                       , mode = c("base", "grid", "debug")
                                       , verbose = TRUE))[3]
    
    cnst.m[cnst.tune.ind] <- dt.ttl[["values.tuned"]]
    grid.opt <- dt.ttl[["grid.opt"]]
    lrate.fin <- dt.ttl[["lrate.fin"]]
    
  }
  
  
  # postprocessing -------------- #
  
  dt.list$cnst.m <- cnst.m
  
  algorithm.options$cnst.m <- dt.list$cnst.m
  algorithm.options$values.tuned.ind <- dt.list$values.tuned.ind
  
  if (!is.null(dt.list$conc.series)) algorithm.options[["conc.series"]] <- dt.list$conc.series

  objective.fn <- ht.objective.function(metrics = "mse", mode = "postproc", dt.list = dt.list)
  dt.ttl <- objective.fn(cnst.m[cnst.tune.ind], method = "basic wls", algorithm.options)
  
  cnst.m.10 <- log(exp(cnst.m), 10)
  
  
  
  
  
  
  
  