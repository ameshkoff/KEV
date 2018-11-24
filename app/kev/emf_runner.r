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

emf.evaluation.runner <- function(mode = c("api", "script", "app")
                                  , sep = ";"
                                  , subdir = ""
                                  , eq.thr.type = "rel"
                                  , eq.threshold = 1e-08
                                  , cnst.tune = NULL
                                  , algorithm = "direct search"
                                  , method = "basic wls"
                                  , emf.mode = c("base", "grid", "debug")
                                  , search.density = 1
                                  , lrate.init = .5
                                  , emf.threshold = 5e-7
                                  , save.res = TRUE
                                  , dt.list = NULL) {
  
  
  #
  
  emf.threshold <- log(10 ^ emf.threshold)
  
  
  # source code ------------- #
  
  dir.start <- ""
  
  if (mode %in% c("script", "api"))
    dir.start <- "app/KEV/"
  
  source(paste0(dir.start, "eq_data.r"), chdir = TRUE)
  source(paste0(dir.start, "emf_data.r"), chdir = TRUE)
  
  source(paste0(dir.start, "eq_preproc.r"), chdir = TRUE)
  source(paste0(dir.start, "emf_preproc.r"), chdir = TRUE)
  
  source(paste0(dir.start, "eq_evaluator.r"), chdir = TRUE)
  source(paste0(dir.start, "emf_evaluator.r"), chdir = TRUE)
  
  source(paste0(dir.start, "eq_postproc.r"), chdir = TRUE)
  source(paste0(dir.start, "emf_postproc.r"), chdir = TRUE)
  
  source(paste0(dir.start, "emf_save.r"), chdir = TRUE)
  
  
  # load data ---------------- #
  
  if (mode == "script") {
    
    dt.ttl <- c(eq.scripts.load(sep, subdir)
                , emf.scripts.load(sep, subdir))
    
  } else if (mode %in% c("app", "api")) {
    
    dt.ttl <- dt.list
    
  }
  
  dt.coef <- dt.ttl[["dt.coef"]]
  dt.conc <- dt.ttl[["dt.conc"]]
  cnst <- dt.ttl[["cnst"]]
  part.eq <- dt.ttl[["part.eq"]]
  
  dt.emf <- dt.ttl[["dt.emf"]]
  dt.params <- dt.ttl[["dt.params"]]
  
  if (is.null(cnst.tune))
    cnst.tune <- dt.ttl[["cnst.tune"]]
  
  
  # preproc data --------------- #
  
  dt.ttl <- c(eq.preproc(dt.coef, cnst, dt.conc, part.eq)
              , emf.preproc(dt.emf, dt.params))
  
  dt.coef <- dt.ttl[["dt.coef"]]
  dt.conc <- dt.ttl[["dt.conc"]]
  cnst.m <- dt.ttl[["cnst.m"]]
  part.eq <- dt.ttl[["part.eq"]]
  dt.coef.m <- dt.ttl[["dt.coef.m"]]
  dt.conc.m <- dt.ttl[["dt.conc.m"]]
  reac.nm <- dt.ttl[["reac.nm"]]
  part.nm <- dt.ttl[["part.nm"]]
  
  dt.emf <- dt.ttl[["dt.emf"]]
  dt.emf.err <- dt.ttl[["dt.emf.err"]]
  dt.params <- dt.ttl[["dt.params"]]
  dt.emf.m <- dt.ttl[["dt.emf.m"]]
  dt.emf.err.m <- dt.ttl[["dt.emf.err.m"]]
  dt.params.m <- dt.ttl[["dt.params.m"]]
  
  cnst.tune.nm <- which(dt.coef[, name] %in% cnst.tune)
  
  
  # run evaluator --------------- #
  
  exec.time <- system.time(
    dt.ttl <- emf.constant.optimizer(dt.coef, cnst.m, cnst.tune
                                     , dt.emf.m, dt.emf.err.m, dt.params.m
                                     , dt.coef.m, dt.conc.m, part.eq, reac.nm
                                     , hardstop = 1000
                                     , lrate.init
                                     , search.density
                                     , emf.threshold
                                     , eq.threshold
                                     , eq.thr.type
                                     , mode = emf.mode
                                     , method
                                     , algorithm))[3]
  
  cnst.m <- dt.ttl[["cnst.m"]]
  cnst.m.10 <- log(exp(cnst.m), 10)
  dt.params <- dt.ttl[["dt.params"]]
  dt.emf.calc <- dt.ttl[["dt.emf.calc"]]
  dt.res.m <- dt.ttl[["dt.res.m"]]
  emf.err <- tail(dt.ttl[["grid.opt"]][!is.na(err), err], 1)
  grid.opt <- dt.ttl[["grid.opt"]]
  lrate.fin <- dt.ttl[["lrate.fin"]]
  
  # postprocessing ---------------- #
  
  cnst.valid <- emf.cnst.validation(dt.coef, cnst.m, cnst.tune
                                    , dt.emf.m, dt.emf.err.m, dt.params.m
                                    , dt.coef.m, dt.conc.m, part.eq, reac.nm
                                    , lrate.fin
                                    , emf.threshold
                                    , eq.threshold
                                    , eq.thr.type
                                    , method)
  
  dt.res <- data.table(dt.res.m)
  
  dt.conc.calc <- eq.tot.conc.calc(dt.res, cnst.m, dt.coef.m, part.nm)
  dt.err <- eq.residuals(dt.conc.m, dt.conc.calc, part.eq)
  
  dt.conc.tot <- copy(dt.conc.m)
  dt.conc.tot[, part.eq] <- dt.conc.calc[, part.eq]
  
  cov.m <- emf.cov(emf.err
                   , cnst.m
                   , cnst.tune.nm
                   , dt.coef, dt.coef.m, dt.conc.m, part.eq, reac.nm
                   , dt.emf.m, dt.emf.err.m, dt.params.m
                   , eq.thr.type, eq.threshold
                   , method, emf.threshold)
  
  err.diff <- cov.m$err.diff
  cor.m <- cov.m$cor.m
  cov.m <- cov.m$cov.m
  
  cnst.dev <- emf.constant.deviations(cnst.m, cov.m, cnst.tune.nm, cnst.valid)
  
  if (is.null(dt.params.m)) {
    
    dt.params.dev <- emf.params.deviations(cnst.m
                                           , cnst.tune.nm
                                           , dt.coef, dt.coef.m, dt.conc.m, part.eq, reac.nm
                                           , dt.emf.m, dt.emf.err.m, dt.params.m
                                           , eq.thr.type, eq.threshold
                                           , method
                                           , emf.threshold)
    
    dt.params <- dt.params.dev$dt.params
    dt.params.dev <- dt.params.dev$dt.params.dev
    
  } else {
    
    dt.params.dev <- NULL
    
  }
  
  emf.res.abs <- emf.residuals(dt.emf.m, dt.emf.calc)
  emf.res.rel <- emf.res.abs$emf.res.rel
  emf.res.abs <- emf.res.abs$emf.res.abs
  
  
  # prepare data to return (emf calculated with errors)
  
  tbl <- objects()
  tbl <- tbl[tbl %in% c("dt.emf.calc", "emf.res.abs", "emf.res.rel")]
  
  for (i in tbl) {
    
    dt <- data.table("particle" = colnames(dt.emf), t(as.matrix(get(i))))
    
    cln <- colnames(dt)
    cln <- cln[cln %like% "^V[0-9]"]
    
    setnames(dt, cln, str_replace(cln, "^V", "S"))
    
    assign(i, dt)
    
  }
  
  
  # save
  
  if (mode == "script" & save.res) {
    
    target <- list(constant = cnst.tune, standard.potential = dt.params["standard.potential"], slope = dt.params["slope"])
    target <- setDT(lapply(target, "length<-", max(lengths(target))))[]
    target <- as.data.table(t(target), keep.rownames = TRUE)
    
    emf.save(subdir, sep, dt.res, dt.emf.calc, emf.res.abs, emf.res.rel, emf.err, cnst.dev, cor.m, err.diff, target)
    
  }

  
  # return data
  
  if (mode %in% c("script", "api")) {
    
    list("grid.opt" = grid.opt
         ,"dt.eq.conc" = dt.res
         , "dt.emf.calc" = dt.emf.calc
         , "emf.res.abs" = emf.res.abs
         , "emf.res.rel" = emf.res.rel
         , "emf.err" = emf.err
         , "cnst.dev" = cnst.dev
         , "cor.m" = cor.m
         , "dt.params" = dt.params
         , "dt.params.dev" = dt.params.dev
         , "err.diff" = err.diff
         , "cnst.tune" = cnst.tune
         , "exec.time" = exec.time
         , "lrate.fin" = lrate.fin)
    
  } else {
    
    # constants to data table
    
    dt.cnst.dev <- as.data.table(cnst.dev)
    dt.cnst.dev <- cbind(name = dt.coef[, name], dt.cnst.dev)
    
    # correlation matrix to data table
    
    dt.cor.m <- as.data.table(cor.m)
    setnames(dt.cor.m, cnst.tune)
    
    dt.cor.m <- as.data.frame(dt.cor.m)
    rownames(dt.cor.m) <- cnst.tune
    
    # remove extra data
    
    vld <- dt.emf.calc[!is.na(S1), which = TRUE]
    
    # return
    
    list("dt.eq.conc" = dt.res
         , "dt.emf.calc" = dt.emf.calc[vld]
         , "emf.res.abs" = emf.res.abs[vld]
         , "emf.res.rel" = emf.res.rel[vld]
         , "emf.err" = emf.err
         , "cnst.dev" = dt.cnst.dev # return data table with additional field for particle names
         , "cor.m" = dt.cor.m
         , "err.diff" = err.diff
         , "cnst.tune" = cnst.tune
         , "lrate.fin" = lrate.fin)
    
  }

}
  











