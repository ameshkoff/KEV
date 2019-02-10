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

ab.evaluation.runner <- function(mode = c("api", "script", "app")
                                 , sep = ";"
                                 , subdir = ""
                                 , eq.thr.type = c("rel", "abs")
                                 , eq.threshold = 1e-08
                                 , cnst.tune = NULL
                                 , wl.tune = NULL
                                 , algorithm = "direct search"
                                 , method = "basic wls"
                                 , ab.mode = c("base", "grid", "debug")
                                 , search.density = 1
                                 , lrate.init = .5
                                 , ab.threshold = 5e-7
                                 , save.res = TRUE
                                 , dt.list = NULL) {

  #
  
  ab.threshold <- log(10 ^ ab.threshold)
  
  
  # source code ------------- #
  
  dir.start <- ""
  
  if (mode %in% c("script", "api"))
    dir.start <- "app/KEV/"
  
  source(paste0(dir.start, "eq_data.r"), chdir = TRUE)
  source(paste0(dir.start, "ab_data.r"), chdir = TRUE)
  
  source(paste0(dir.start, "eq_preproc.r"), chdir = TRUE)
  source(paste0(dir.start, "ab_preproc.r"), chdir = TRUE)
  
  source(paste0(dir.start, "eq_evaluator.r"), chdir = TRUE)
  source(paste0(dir.start, "ab_evaluator.r"), chdir = TRUE)
  
  source(paste0(dir.start, "eq_postproc.r"), chdir = TRUE)
  source(paste0(dir.start, "ab_postproc.r"), chdir = TRUE)
  
  source(paste0(dir.start, "ab_save.r"), chdir = TRUE)
  
  
  # load data ---------------- #
  
  if (mode == "script") {
    
    dt.ttl <- c(eq.scripts.load(sep, subdir)
                , ab.scripts.load(sep, subdir))
    
  } else if (mode %in% c("app", "api")) {
    
    dt.ttl <- dt.list
    
  }
  
  dt.coef <- dt.ttl[["dt.coef"]]
  dt.conc <- dt.ttl[["dt.conc"]]
  cnst <- dt.ttl[["cnst"]]
  part.eq <- dt.ttl[["part.eq"]]
  
  dt.ab <- dt.ttl[["dt.ab"]]
  dt.mol <- dt.ttl[["dt.mol"]]
  
  if (is.null(cnst.tune))
    cnst.tune <- dt.ttl[["cnst.tune"]]
  
  if (is.null(wl.tune))
    wl.tune <- dt.ttl[["wl.tune"]]
  
  
  # preproc data --------------- #
  
  dt.ttl <- c(eq.preproc(dt.coef, cnst, dt.conc, part.eq)
              , ab.preproc(dt.ab, dt.mol, wl.tune))
  
  dt.coef <- dt.ttl[["dt.coef"]]
  dt.conc <- dt.ttl[["dt.conc"]]
  cnst.m <- dt.ttl[["cnst.m"]]
  part.eq <- dt.ttl[["part.eq"]]
  dt.coef.m <- dt.ttl[["dt.coef.m"]]
  dt.conc.m <- dt.ttl[["dt.conc.m"]]
  reac.nm <- dt.ttl[["reac.nm"]]
  part.nm <- dt.ttl[["part.nm"]]
  
  dt.ab <- dt.ttl[["dt.ab"]]
  dt.ab.full <- dt.ttl[["dt.ab.full"]]
  dt.ab.err <- dt.ttl[["dt.ab.err"]]
  dt.ab.err.full <- dt.ttl[["dt.ab.err.full"]]
  dt.mol <- dt.ttl[["dt.mol"]]
  dt.mol.full <- dt.ttl[["dt.mol.full"]]
  dt.ab.m <- dt.ttl[["dt.ab.m"]]
  dt.ab.full.m <- dt.ttl[["dt.ab.full.m"]]
  dt.ab.err.m <- dt.ttl[["dt.ab.err.m"]]
  dt.ab.err.full.m <- dt.ttl[["dt.ab.err.full.m"]]
  dt.mol.m <- dt.ttl[["dt.mol.m"]]
  dt.mol.full.m <- dt.ttl[["dt.mol.full.m"]]
  partprod.nm <- dt.ttl[["partprod.nm"]]
  wavelength <- dt.ttl[["wavelength"]]
  
  cnst.tune.nm <- which(dt.coef[, name] %in% cnst.tune)
  
  
  # run evaluator --------------- #
  
  exec.time <- system.time(
    dt.ttl <- constant.optimizer(dt.coef, cnst.m, cnst.tune
                                 , dt.ab.m, dt.ab.err.m, dt.mol.m
                                 , dt.coef.m, dt.conc.m, part.eq, reac.nm
                                 , hardstop = 1000
                                 , lrate.init
                                 , search.density
                                 , ab.threshold
                                 , eq.threshold
                                 , eq.thr.type
                                 , mode = ab.mode
                                 , method
                                 , algorithm))[3]
  
  cnst.m <- dt.ttl[["cnst.m"]]
  cnst.m.10 <- log(exp(cnst.m), 10)
  mol.coef <- dt.ttl[["mol.coef"]]
  dt.ab.calc <- dt.ttl[["dt.ab.calc"]]
  dt.res.m <- dt.ttl[["dt.res.m"]]
  ab.err <- tail(dt.ttl[["grid.opt"]][!is.na(err), err], 1)
  grid.opt <- dt.ttl[["grid.opt"]]
  lrate.fin <- dt.ttl[["lrate.fin"]]
  
  # postprocessing ---------------- #
  
  cnst.valid <- cnst.validation(dt.coef, cnst.m, cnst.tune
                                , dt.ab.m, dt.ab.err.m, dt.mol.m
                                , dt.coef.m, dt.conc.m, part.eq, reac.nm
                                , lrate.fin
                                , ab.threshold
                                , eq.threshold
                                , eq.thr.type
                                , method)
  
  dt.res <- data.table(dt.res.m)
  
  dt.conc.calc <- eq.tot.conc.calc(dt.res, cnst.m, dt.coef.m, part.nm)
  dt.err <- eq.residuals(dt.conc.m, dt.conc.calc, part.eq)
  
  dt.conc.tot <- copy(dt.conc.m)
  dt.conc.tot[, part.eq] <- dt.conc.calc[, part.eq]
  
  cov.m <- ab.cov(ab.err
                 , cnst.m
                 , cnst.tune.nm
                 , dt.coef, dt.coef.m, dt.conc.m, part.eq, reac.nm
                 , dt.ab.m, dt.ab.err.m, dt.mol.m
                 , eq.thr.type, eq.threshold
                 , method, ab.threshold)
  
  err.diff <- cov.m$err.diff
  cor.m <- cov.m$cor.m
  cov.m <- cov.m$cov.m
  
  cnst.dev <- constant.deviations(cnst.m, cov.m, cnst.tune.nm, cnst.valid)
  
  mol.coef.dev <- molar.coef.deviations(cnst.m
                                        , cnst.tune.nm
                                        , dt.coef, dt.coef.m, dt.conc.m, part.eq, reac.nm
                                        , dt.ab.full.m, dt.ab.err.full.m, dt.mol.full.m
                                        , eq.thr.type, eq.threshold
                                        , method
                                        , ab.threshold)
  
  mol.coef <- mol.coef.dev$mol.coef
  dt.ab.calc <- mol.coef.dev$dt.ab.calc
  mol.coef.dev <- mol.coef.dev$mol.coef.dev
  
  ab.res.abs <- absorbance.residuals(dt.ab.full.m, dt.ab.calc)
  ab.res.rel <- ab.res.abs$ab.res.rel
  ab.res.abs <- ab.res.abs$ab.res.abs
  
  # prepare data to return (transpose wave data)

  tbl <- objects()
  tbl <- tbl[tbl %in% c("dt.ab.calc", "ab.res.abs", "ab.res.rel")]
  
  for (i in tbl) {
    
    dt <- data.table("wavelength" = wavelength, t(get(i)))
    
    cln <- colnames(dt)
    cln <- cln[cln %like% "^V[0-9]"]
    
    setnames(dt, cln, str_replace(cln, "^V", "S"))
    
    assign(i, dt)
    
  }
  
  tbl <- objects()
  tbl <- tbl[tbl %in% c("mol.coef", "mol.coef.dev")]
  
  cln <- colnames(mol.coef)
  cln <- cln[!(cln %in% colnames(dt.mol))]
  
  setnames(mol.coef.dev, cln)
  
  for (i in tbl) {
    
    dt <- data.table("wavelength" = wavelength, get(i))
    assign(i, dt)
    
  }
  
  
  # save
  
  if (mode == "script" & save.res) {
    
    if (is.null(wl.tune))
      wl.tune <- wavelength
    
    target <- list(constant = cnst.tune, wavelength = wl.tune)
    target <- setDT(lapply(target, "length<-", max(lengths(target))))[]
    target <- as.data.table(t(target), keep.rownames = TRUE)
    
    ab.save(subdir, sep, dt.res, dt.ab.calc, ab.res.abs, ab.res.rel, ab.err, cnst.dev, cor.m, mol.coef, mol.coef.dev, err.diff, target)
    
  }
  

  # return data
  
  if (mode %in% c("script", "api")) {
    
    list("grid.opt" = grid.opt
         ,"dt.eq.conc" = dt.res
         , "dt.ab.calc" = dt.ab.calc
         , "ab.res.abs" = ab.res.abs
         , "ab.res.rel" = ab.res.rel
         , "ab.err" = ab.err
         , "cnst.dev" = cnst.dev
         , "cor.m" = cor.m
         , "mol.coef" = mol.coef
         , "mol.coef.dev" = mol.coef.dev
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
    
    # molar coefficients to data table
    
    mol.coef.dev.full <- copy(mol.coef)
    cln <- colnames(mol.coef.dev)
    
    for (i in colnames(mol.coef.dev.full)) {
      
      if (i %in% cln) {
        
        mol.coef.dev.full[, eval(i) := mol.coef.dev[, eval(as.name(i))]]
        
      } else {
        
        mol.coef.dev.full[, eval(i) := 0]
        
      }
      
    }
    
    # remove extra data
    
    vld <- dt.ab.calc[!is.na(S1), which = TRUE]
    
    # return
    
    list("dt.eq.conc" = dt.res
         , "dt.ab.calc" = dt.ab.calc[vld]
         , "ab.res.abs" = ab.res.abs[vld]
         , "ab.res.rel" = ab.res.rel[vld]
         , "ab.err" = ab.err
         , "cnst.dev" = dt.cnst.dev # return data table with additional field for particle names
         , "cor.m" = dt.cor.m
         , "mol.coef" = mol.coef[vld]
         , "mol.coef.dev" = mol.coef.dev.full[vld]
         , "err.diff" = err.diff
         , "cnst.tune" = cnst.tune
         , "lrate.fin" = lrate.fin)
    
  }
  
}



# grid research -------------------------------------------- #

ab.evaluation.grid <- function(mode = c("api", "script", "app")
                               , sep = ";"
                               , subdir = ""
                               , eq.thr.type = c("rel", "abs")
                               , eq.threshold = 1e-08
                               , cnst.tune = c("HL", "H2L")
                               , wl.tune = NULL
                               , method = "basic wls"
                               , search.density = 1
                               , lrate.init = .5
                               , ab.threshold = 5e-7
                               , dt.list = NULL) {
  
  #
  
  ab.threshold <- log(10 ^ ab.threshold)
  
  
  # source code ------------- #
  
  dir.start <- ""
  
  if (mode %in% c("script", "api"))
    dir.start <- "app/KEV/"
  
  source(paste0(dir.start, "eq_data.r"), chdir = TRUE)
  source(paste0(dir.start, "ab_data.r"), chdir = TRUE)
  
  source(paste0(dir.start, "eq_preproc.r"), chdir = TRUE)
  source(paste0(dir.start, "ab_preproc.r"), chdir = TRUE)
  
  source(paste0(dir.start, "eq_evaluator.r"), chdir = TRUE)
  source(paste0(dir.start, "ab_evaluator.r"), chdir = TRUE)
  
  source(paste0(dir.start, "eq_postproc.r"), chdir = TRUE)
  source(paste0(dir.start, "ab_postproc.r"), chdir = TRUE)
  
  source(paste0(dir.start, "ab_save.r"), chdir = TRUE)
  
  
  # load data ---------------- #
  
  if (mode == "script") {
    
    dt.ttl <- c(eq.scripts.load(sep, subdir)
                , ab.scripts.load(sep, subdir))
    
  } else if (mode %in% c("app", "api")) {
    
    dt.ttl <- dt.list
    
  }
  
  dt.coef <- dt.ttl[["dt.coef"]]
  dt.conc <- dt.ttl[["dt.conc"]]
  cnst <- dt.ttl[["cnst"]]
  part.eq <- dt.ttl[["part.eq"]]
  
  dt.ab <- dt.ttl[["dt.ab"]]
  dt.mol <- dt.ttl[["dt.mol"]]
  
  
  # preproc data --------------- #
  
  dt.ttl <- c(eq.preproc(dt.coef, cnst, dt.conc, part.eq)
              , ab.preproc(dt.ab, dt.mol, wl.tune))
  
  dt.coef <- dt.ttl[["dt.coef"]]
  dt.conc <- dt.ttl[["dt.conc"]]
  cnst.m <- dt.ttl[["cnst.m"]]
  part.eq <- dt.ttl[["part.eq"]]
  dt.coef.m <- dt.ttl[["dt.coef.m"]]
  dt.conc.m <- dt.ttl[["dt.conc.m"]]
  reac.nm <- dt.ttl[["reac.nm"]]
  part.nm <- dt.ttl[["part.nm"]]
  
  dt.ab <- dt.ttl[["dt.ab"]]
  dt.ab.full <- dt.ttl[["dt.ab.full"]]
  dt.ab.err <- dt.ttl[["dt.ab.err"]]
  dt.ab.err.full <- dt.ttl[["dt.ab.err.full"]]
  dt.mol <- dt.ttl[["dt.mol"]]
  dt.mol.full <- dt.ttl[["dt.mol.full"]]
  dt.ab.m <- dt.ttl[["dt.ab.m"]]
  dt.ab.full.m <- dt.ttl[["dt.ab.full.m"]]
  dt.ab.err.m <- dt.ttl[["dt.ab.err.m"]]
  dt.ab.err.full.m <- dt.ttl[["dt.ab.err.full.m"]]
  dt.mol.m <- dt.ttl[["dt.mol.m"]]
  dt.mol.full.m <- dt.ttl[["dt.mol.full.m"]]
  partprod.nm <- dt.ttl[["partprod.nm"]]
  wavelength <- dt.ttl[["wavelength"]]
  
  cnst.tune.nm <- which(dt.coef[, name] %in% cnst.tune)
  
  
  # run evaluator --------------- #
  
  # create grid
  
  grid.iter <- lapply(cnst.m[cnst.tune.nm], function(x) { seq(x * .2, x * 2, lrate.init * x) })
  names(grid.iter) <- cnst.tune.nm
  
  grid.iter <- data.table(expand.grid(grid.iter))
  
  remove(grid.opt)
  
  # run loop
  
  for (i in 1:nrow(grid.iter)) {
    
    cnst.m[cnst.tune.nm] <- unlist(grid.iter[i])
    
    tmp <- constant.optimizer(dt.coef, cnst.m, cnst.tune
                                   , dt.ab.m, dt.ab.err.m, dt.mol.m
                                   , dt.coef.m, dt.conc.m, part.eq, reac.nm
                                   , hardstop = 1
                                   , lrate.init
                                   , search.density
                                   , ab.threshold
                                   , eq.threshold
                                   , eq.thr.type
                                   , mode = "grid"
                                   , method
                                   , algorithm = "basic search")$grid.opt
    
    if (exists("grid.opt")) {
      
      grid.opt <- rbind(grid.opt, tmp)
      
    } else {
      
      grid.opt <- copy(tmp)
      
    }
    
  }
  
  # postproc
  
  cln <- colnames(grid.opt)
  cln <- cln[cln %like% "^[0-9]+$"]
  
  for (cl in cln) {
    
    grid.opt[, eval(cl) := log(exp(eval(as.name(cl))), 10)]
    
  }
  
  grid.opt

}


