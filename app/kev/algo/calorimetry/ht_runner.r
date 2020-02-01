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

ht.evaluation.runner <- function(mode = "script"
                                 , sep = ";"
                                 , subdir = ""
                                 , eq.thr.type = c("rel", "abs")
                                 , eq.threshold = 1e-08
                                 , cnst.tune = NULL
                                 , cmp.tune = NULL
                                 , algorithm = "direct search"
                                 , method = "basic wls"
                                 , metrics = "mse"
                                 , ht.mode = c("base", "grid", "debug")
                                 , search.density = 1
                                 , lrate.init = .5
                                 , ht.threshold = 5e-7
                                 , save.res = FALSE
                                 , dt.list = NULL
                                 , filename = NULL) {
  
  #
  
  ht.threshold <- log(10 ^ ht.threshold)
  
  
  # source code ------------- #
  
  dir.start <- ""
  
  if (mode[1] %in% c("script", "api")) {
    
    dir.start <- "app/kev/algo/"
    
  } else {
    
    dir.start <- "algo/"
    
  }
  
  source(paste0(dir.start, "concentrations/eq_data.r"), chdir = TRUE, local = TRUE)
  source(paste0(dir.start, "calorimetry/ht_data.r"), chdir = TRUE, local = TRUE)
  
  source(paste0(dir.start, "concentrations/eq_preproc.r"), chdir = TRUE, local = TRUE)
  source(paste0(dir.start, "calorimetry/ht_preproc.r"), chdir = TRUE, local = TRUE)
  
  source(paste0(dir.start, "concentrations/eq_evaluator.r"), chdir = TRUE, local = TRUE)
  source(paste0(dir.start, "calorimetry/ht_evaluator.r"), chdir = TRUE, local = TRUE)
  
  source(paste0(dir.start, "concentrations/eq_postproc.r"), chdir = TRUE, local = TRUE)
  source(paste0(dir.start, "calorimetry/ht_postproc.r"), chdir = TRUE, local = TRUE)

  
  # load data ---------------- #
  
  if (mode[1] %in% c("script")) {
    
    dt.ttl <- c(eq.scripts.load(sep, subdir, filename)
                , ht.scripts.load(sep, subdir, filename))
    
  } else if (mode[1] %in% c("app", "api")) {
    
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
  
  algorithm.options <- list(algorithm = algorithm
                            , hardstop = 100
                            , lrate.init = lrate.init
                            , search.density = search.density
                            , value.threshold = ht.threshold
                            , eq.threshold = eq.threshold
                            , eq.thr.type = eq.thr.type[1])
  
  if (length(cnst.tune.ind) > 0) {
    
    exec.time <- system.time(
      dt.ttl <- kev.constant.optimizer(objective.fn = ht.objective.function
                                       , evaluation.fn = ht.enth.evaluator
                                       , values.init = cnst.m[cnst.tune.ind]
                                       , lower.bound = -Inf
                                       , upper.bound = Inf
                                       , dt.list = dt.list
                                       , algorithm.options = algorithm.options
                                       , metrics = metrics
                                       , mode = ht.mode[1]
                                       , verbose = TRUE))[3]
    
    cnst.m[cnst.tune.ind] <- dt.ttl[["values.tuned"]]
    grid.opt <- dt.ttl[["grid.opt"]]
    lrate.fin <- dt.ttl[["lrate.fin"]]
    
  }
  
  
  # postprocessing -------------- #
  
  # partial molar properties
  
  dt.list$cnst.m <- cnst.m
  
  algorithm.options$cnst.m <- dt.list$cnst.m
  algorithm.options$values.tuned.ind <- dt.list$values.tuned.ind
  
  if (!is.null(dt.list$conc.series)) algorithm.options[["conc.series"]] <- dt.list$conc.series

  objective.fn <- ht.objective.function(metrics = "mse", mode = "postproc", dt.list = dt.list)
  dt.ttl <- objective.fn(cnst.m[cnst.tune.ind], method = "basic wls", algorithm.options)
  
  heat.err <- dt.ttl[["err"]]
  dt.enth.calc <- dt.ttl[["dt.enth.calc"]][, .(reaction, value, dev)]
  dt.heat.calc <- dt.ttl[["dt.heat.calc"]]
  
  # concentrations
  
  dt.res.m <- newton.wrapper(cnst.m, dt.coef.m, dt.conc.m, part.eq, reac.nm, eq.thr.type[1], eq.threshold)
  colnames(dt.res.m) <- dt.coef[, name]
  
  dt.res <- data.table(dt.res.m)
  
  dt.conc.calc <- eq.tot.conc.calc(dt.res, cnst.m, dt.coef.m, part.nm)
  dt.conc.err <- eq.residuals(dt.conc.m, dt.conc.calc, part.eq)
  
  dt.conc.tot <- copy(dt.conc.m)
  dt.conc.tot[, part.eq] <- dt.conc.calc[, part.eq]
  
  if (length(cnst.tune.ind) > 0) {
    
    # covariance matrix
    
    cov.m <- ht.cov(heat.err
                    , cnst.m
                    , cnst.tune
                    , cnst.tune.ind
                    , dt.heat.calc
                    , objective.fn
                    , algorithm.options
                    , method
                    , ht.threshold)
    
    err.diff <- cov.m$err.diff
    cor.m <- cov.m$cor.m
    cov.m <- cov.m$cov.m
    
    
    # constants with deviations
    
    cnst.dev <- constant.validation(cnst.m
                                    , cnst.tune
                                    , objective.fn = ht.objective.function
                                    , evaluation.fn = ht.enth.evaluator
                                    , algorithm.options
                                    , metrics = metrics
                                    , dt.list
                                    , cov.m
                                    , ht.threshold
                                    , lrate.fin
                                    , method)
    
  } else {
    
    err.diff <- NULL
    cor.m <- NULL
    cov.m <- NULL
    cnst.dev <- NULL
    lrate.fin <- NULL
    
  }
  
  # heats: calculate residuals and r^2
  
  # rewrite
  dt.ttl <- heat.residuals(dt.heat, dt.heat.calc, dt.enth.calc, dt.enth)
  
  dt.heat.calc <- dt.ttl[["dt.heat.calc"]]
  adj.r.squared <- dt.ttl[["adj.r.squared"]]
  
  # `save` module moved to the fully independent function
  
  # return
  
  rtrn <- list("dt.eq.conc" = dt.res
               , "dt.heat.calc" = dt.heat.calc
               , "cnst.dev" = cnst.dev
               , "cor.m" = cor.m
               , "dt.enth.calc" = dt.enth.calc
               , "err.diff" = err.diff
               , "cnst.tune" = cnst.tune
               , "lrate.fin" = lrate.fin
               , "adj.r.squared" = adj.r.squared
               , "dt.coef.input" = dt.coef
               , "dt.conc.input" = dt.conc
               , "cnst.input" = cnst
               , "part.eq.input" = part.eq
               , "dt.heat.input" = dt.heat
               , "dt.enth.input" = dt.enth
               , "cmp.tune.input" = cmp.tune
               , "calorimeter.type.input" = calorimeter.type
               , "init.vol.input" = init.vol
               )
  
  if (save.res) {
    
    source(paste0(dir.start, "calorimetry/ht_save.r"), chdir = TRUE, local = TRUE)
    
    ht.save(rtrn, path = str_replace(paste0("output/", subdir), "\\/\\/", "/"), sep = sep, filename = filename)
    
  }
  
  rtrn
  
}
  
  
  
  
  
  
  
  