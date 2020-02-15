# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
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

nm.evaluation.runner <- function(mode = c("api", "script", "app")
                                 , sep = ";"
                                 , subdir = ""
                                 , eq.thr.type = c("rel", "abs")
                                 , eq.threshold = 1e-08
                                 , cnst.tune = NULL
                                 , wl.tune = NULL
                                 , algorithm = "direct search"
                                 , method = "basic wls"
                                 , nm.mode = c("base", "grid", "debug")
                                 , search.density = 1
                                 , lrate.init = .5
                                 , nm.threshold = 5e-7
                                 , save.res = TRUE
                                 , dt.list = NULL) {

  #
  
  nm.threshold <- log(10 ^ nm.threshold)
  
  
  # source code ------------- #
  
  dir.start <- ""
  
  if (mode %in% c("script", "api")) {
    
    dir.start <- "app/kev/algo/"
    
  } else {
    
    dir.start <- "algo/"
    
  }
  
  source(paste0(dir.start, "concentrations/eq_data.r"), chdir = TRUE)
  source(paste0(dir.start, "nmr/nm_data.r"), chdir = TRUE)
  
  source(paste0(dir.start, "concentrations/eq_preproc.r"), chdir = TRUE)
  source(paste0(dir.start, "nmr/nm_preproc.r"), chdir = TRUE)

  source(paste0(dir.start, "concentrations/eq_evaluator.r"), chdir = TRUE)
  source(paste0(dir.start, "nmr/nm_evaluator.r"), chdir = TRUE)

  source(paste0(dir.start, "concentrations/eq_postproc.r"), chdir = TRUE)
  source(paste0(dir.start, "nmr/nm_postproc.r"), chdir = TRUE)

  source(paste0(dir.start, "nmr/nm_save.r"), chdir = TRUE)
  
  
  # load data ---------------- #
  
  if (mode == "script") {
    
    dt.ttl <- c(eq.scripts.load(sep, subdir)
                , nm.scripts.load(sep, subdir))
    
  } else if (mode %in% c("app", "api")) {
    
    dt.ttl <- copy(dt.list)
    
  }
  
  dt.coef <- dt.ttl[["dt.coef"]]
  dt.conc <- dt.ttl[["dt.conc"]]
  cnst <- dt.ttl[["cnst"]]
  part.eq <- dt.ttl[["part.eq"]]
  
  dt.nm <- dt.ttl[["dt.nm"]]
  dt.ind <- dt.ttl[["dt.ind"]]
  
  if (is.null(cnst.tune))
    cnst.tune <- dt.ttl[["cnst.tune"]]

  
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
  
  dt.ttl <- nm.preproc(dt.nm, dt.ind, dt.coef, dt.conc.m, part.eq)
  
  dt.nm <- dt.ttl[["dt.nm"]]
  dt.ind <- dt.ttl[["dt.ind"]]
  cr.name <- dt.ttl[["cr.name"]]
  coef.b <- dt.ttl[["coef.b"]]
  conc.b <- dt.ttl[["conc.b"]]
  
  cnst.tune.nm <- which(dt.coef[, name] %in% cnst.tune)
  
  
  # run evaluator --------------- #
  
  exec.time <- system.time(
    dt.ttl <- nm.constant.optimizer(dt.coef, cnst.m, cnst.tune
                                 , dt.nm, dt.ind
                                 , dt.coef.m, dt.conc.m, part.eq, reac.nm
                                 , coef.b, conc.b
                                 , hardstop = 1000
                                 , lrate.init
                                 , search.density
                                 , nm.threshold
                                 , eq.threshold
                                 , eq.thr.type
                                 , mode = nm.mode
                                 , method
                                 , algorithm))[3]
  
  cnst.m <- dt.ttl[["cnst.m"]]
  cnst.m.10 <- log(exp(cnst.m), 10)
  ind.shift <- dt.ttl[["ind.shift"]]
  dt.nm.calc <- dt.ttl[["dt.nm.calc"]]
  dt.res.m <- dt.ttl[["dt.res.m"]]
  nm.err <- tail(dt.ttl[["grid.opt"]][!is.na(err), err], 1)
  grid.opt <- dt.ttl[["grid.opt"]]
  lrate.fin <- dt.ttl[["lrate.fin"]]
  
  # postprocessing ---------------- #
  
  cnst.valid <- nm.cnst.validation(dt.coef, cnst.m, cnst.tune
                                , dt.nm, dt.ind
                                , dt.coef.m, dt.conc.m, part.eq, reac.nm
                                , coef.b, conc.b
                                , lrate.fin
                                , nm.threshold
                                , eq.threshold
                                , eq.thr.type
                                , method)
  
  dt.res <- data.table(dt.res.m)
  
  dt.conc.calc <- eq.tot.conc.calc(dt.res, cnst.m, dt.coef.m, part.nm)
  dt.err <- eq.residuals(dt.conc.m, dt.conc.calc, part.eq)
  
  dt.conc.tot <- copy(dt.conc.m)
  dt.conc.tot[, part.eq] <- dt.conc.calc[, part.eq]
  
  cov.m <- nm.cov(nm.err
                  , cnst.m
                  , cnst.tune.nm
                  , dt.coef, dt.coef.m, dt.conc.m, part.eq, reac.nm
                  , dt.nm, dt.ind
                  , coef.b, conc.b
                  , eq.thr.type, eq.threshold
                  , method, nm.threshold)
  
  err.diff <- cov.m$err.diff
  cor.m <- cov.m$cor.m
  cov.m <- cov.m$cov.m
  
  cnst.dev <- nm.constant.deviations(cnst.m, cov.m, cnst.tune.nm, cnst.valid)
  
  ind.shift.dev <- nm.ind.shift.deviations(cnst.m
                                        , cnst.tune.nm
                                        , dt.coef, dt.coef.m, dt.conc.m, part.eq, reac.nm
                                        , dt.nm, dt.ind
                                        , coef.b, conc.b
                                        , eq.thr.type, eq.threshold
                                        , method
                                        , nm.threshold)
  
  ind.shift <- ind.shift.dev$ind.shift
  dt.nm.calc <- ind.shift.dev$dt.nm.calc
  ind.shift.dev <- ind.shift.dev$ind.shift.dev
  
  nm.res.abs <- nm.shift.residuals(dt.nm[, observation], dt.nm.calc[, calculated], reac.nm)
  
  adj.r.squared <- nm.res.abs$adj.r.squared
  nm.res.rel <- nm.res.abs$nm.res.rel
  nm.res.abs <- nm.res.abs$nm.res.abs
  
  # prepare data to return (transpose shifts data)
  
  tbl <- objects()
  tbl <- tbl[tbl %in% c("nm.res.abs", "nm.res.rel")]

  for (i in tbl) {
    
    dt <- dt.nm.calc[, .(signal, solution, calculated = get(i))]
    assign(i, dt)
    
  }

  tbl <- objects()
  tbl <- tbl[tbl %in% c("dt.nm.calc", "nm.res.abs", "nm.res.rel")]
  
  for (i in tbl) {
    
    dt <- get(i)
    
    dt[, particle := cr.name]
    dt <- dcast.data.table(dt, particle + signal ~ solution, value.var = "calculated")
    
    cln <- colnames(dt)
    cln <- cln[cln %like% "^V[0-9]"]
    
    setnames(dt, cln, str_replace(cln, "^V", "S"))
    
    assign(i, dt)
    
  }
  
  tbl <- objects()
  tbl <- tbl[tbl %in% c("ind.shift", "ind.shift.dev")]
  
  
  # save
  
  if (mode == "script" & save.res) {
    
    target <- list(constant = cnst.tune)
    target <- setDT(lapply(target, "length<-", max(lengths(target))))[]
    target <- as.data.table(t(target), keep.rownames = TRUE)
    
    nm.save(subdir, sep, dt.res, dt.nm.calc, nm.res.abs, nm.res.rel, nm.err, cnst.dev, cor.m, ind.shift, ind.shift.dev, err.diff, target)
    
  }
  
  
  # return data
  
  if (mode %in% c("script", "api")) {
    
    list("grid.opt" = grid.opt
         ,"dt.eq.conc" = dt.res
         , "dt.nm.calc" = dt.nm.calc
         , "nm.res.abs" = nm.res.abs
         , "nm.res.rel" = nm.res.rel
         , "nm.err" = nm.err
         , "cnst.dev" = cnst.dev
         , "cor.m" = cor.m
         , "ind.shift" = ind.shift
         , "ind.shift.dev" = ind.shift.dev
         , "err.diff" = err.diff
         , "cnst.tune" = cnst.tune
         , "exec.time" = exec.time
         , "lrate.fin" = lrate.fin
         , "adj.r.squared" = adj.r.squared)
    
  } else {
    
    # component name to constants
    dt.cnst.dev <- cbind(name = dt.coef[, name], cnst.dev)
    
    # correlation matrix to data table
    
    dt.cor.m <- as.data.table(cor.m)
    setnames(dt.cor.m, cnst.tune)
    
    dt.cor.m <- as.data.frame(dt.cor.m)
    rownames(dt.cor.m) <- cnst.tune
    
    # fill deviations for known individual shifts
    
    ind.shift.dev.res <- copy(ind.shift)
    
    if (is.data.table(ind.shift.dev)) {
      
      cln <- colnames(ind.shift.dev)
      
    }
    
    for (i in colnames(ind.shift.dev.res)) {
      
      if (i %in% cln) {
        
        ind.shift.dev.res[, eval(i) := ind.shift.dev[, eval(as.name(i))]]
        
      } else {
        
        ind.shift.dev.res[, eval(i) := 0]
        
      }
      
    }
    
    
    # return
    
    list("dt.eq.conc" = dt.res
         , "dt.nm.calc" = dt.nm.calc
         , "nm.res.abs" = nm.res.abs
         , "nm.res.rel" = nm.res.rel
         , "nm.err" = nm.err
         , "cnst.dev" = dt.cnst.dev
         , "cor.m" = dt.cor.m
         , "ind.shift" = ind.shift
         , "ind.shift.dev" = ind.shift.dev.res
         , "err.diff" = err.diff
         , "cnst.tune" = cnst.tune
         , "lrate.fin" = lrate.fin
         , "adj.r.squared" = adj.r.squared)
    
  }
  
}





  
  