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

# nm.evaluation.runner <- function(mode = c("api", "script", "app")
#                                  , sep = ";"
#                                  , subdir = ""
#                                  , eq.thr.type = c("rel", "abs")
#                                  , eq.threshold = 1e-08
#                                  , cnst.tune = NULL
#                                  , wl.tune = NULL
#                                  , algorithm = "direct search"
#                                  , method = "basic wls"
#                                  , nm.mode = c("base", "grid", "debug")
#                                  , search.density = 1
#                                  , lrate.init = .5
#                                  , nm.threshold = 5e-7
#                                  , save.res = TRUE
#                                  , dt.list = NULL) {
mode = "script"
sep = ","
subdir = "nmr/dsn.1"
eq.thr.type = c("rel", "abs")
eq.threshold = 1e-08
cnst.tune = NULL
wl.tune = NULL
algorithm = "direct search"
method = "basic wls"
nm.mode = c("base", "grid", "debug")
search.density = 1
lrate.init = .5
nm.threshold = 5e-7
save.res = TRUE
dt.list = NULL
  
  #
  
  nm.threshold <- log(10 ^ nm.threshold)
  
  
  # source code ------------- #
  
  dir.start <- ""
  
  if (mode %in% c("script", "api"))
    dir.start <- "app/KEV/"
  
  source(paste0(dir.start, "eq_data.r"), chdir = TRUE)
  source(paste0(dir.start, "nm_data.r"), chdir = TRUE)
  
  source(paste0(dir.start, "eq_preproc.r"), chdir = TRUE)
  source(paste0(dir.start, "nm_preproc.r"), chdir = TRUE)

  # source(paste0(dir.start, "eq_evaluator.r"), chdir = TRUE)
  # source(paste0(dir.start, "nm_evaluator.r"), chdir = TRUE)
  # 
  # source(paste0(dir.start, "eq_postproc.r"), chdir = TRUE)
  # source(paste0(dir.start, "nm_postproc.r"), chdir = TRUE)
  # 
  # source(paste0(dir.start, "nm_save.r"), chdir = TRUE)
  
  
  # load data ---------------- #
  
  if (mode == "script") {
    
    dt.ttl <- c(eq.scripts.load(sep, subdir)
                , nm.scripts.load(sep, subdir))
    
  } else if (mode %in% c("app", "api")) {
    
    dt.ttl <- dt.list
    
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
  
  dt.ttl <- c(eq.preproc(dt.coef, cnst, dt.conc, part.eq)
              , nm.preproc(dt.nm, dt.ind))
  
  dt.coef <- dt.ttl[["dt.coef"]]
  dt.conc <- dt.ttl[["dt.conc"]]
  cnst.m <- dt.ttl[["cnst.m"]]
  part.eq <- dt.ttl[["part.eq"]]
  dt.coef.m <- dt.ttl[["dt.coef.m"]]
  dt.conc.m <- dt.ttl[["dt.conc.m"]]
  reac.nm <- dt.ttl[["reac.nm"]]
  part.nm <- dt.ttl[["part.nm"]]
  
  dt.nm <- dt.ttl[["dt.nm"]]
  dt.nm.err <- dt.ttl[["dt.nm.err"]]
  dt.ind <- dt.ttl[["dt.ind"]]
  dt.nm.m <- dt.ttl[["dt.nm.m"]]
  dt.nm.err.m <- dt.ttl[["dt.nm.err.m"]]
  dt.ind.m <- dt.ttl[["dt.ind.m"]]
  cr.name <- dt.ttl[["cr.name"]]
  
  cnst.tune.nm <- which(dt.coef[, name] %in% cnst.tune)
  
  
  # run evaluator --------------- #
  
  exec.time <- system.time(
    dt.ttl <- constant.optimizer(dt.coef, cnst.m, cnst.tune
                                 , dt.nm.m, dt.nm.err.m, dt.ind.m
                                 , dt.coef.m, dt.conc.m, part.eq, reac.nm
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
  mol.coef <- dt.ttl[["mol.coef"]]
  dt.nm.calc <- dt.ttl[["dt.nm.calc"]]
  dt.res.m <- dt.ttl[["dt.res.m"]]
  nm.err <- tail(dt.ttl[["grid.opt"]][!is.na(err), err], 1)
  grid.opt <- dt.ttl[["grid.opt"]]
  lrate.fin <- dt.ttl[["lrate.fin"]]
  
  # postprocessing ---------------- #
  
  cnst.valid <- cnst.validation(dt.coef, cnst.m, cnst.tune
                                , dt.nm.m, dt.nm.err.m, dt.ind.m
                                , dt.coef.m, dt.conc.m, part.eq, reac.nm
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
                  , dt.nm.m, dt.nm.err.m, dt.ind.m
                  , eq.thr.type, eq.threshold
                  , method, nm.threshold)
  
  err.diff <- cov.m$err.diff
  cor.m <- cov.m$cor.m
  cov.m <- cov.m$cov.m
  
  cnst.dev <- constant.deviations(cnst.m, cov.m, cnst.tune.nm, cnst.valid)
  
  mol.coef.dev <- molar.coef.deviations(cnst.m
                                        , cnst.tune.nm
                                        , dt.coef, dt.coef.m, dt.conc.m, part.eq, reac.nm
                                        , dt.nm.full.m, dt.nm.err.full.m, dt.ind.full.m
                                        , eq.thr.type, eq.threshold
                                        , method
                                        , nm.threshold)
  
  mol.coef <- mol.coef.dev$mol.coef
  dt.nm.calc <- mol.coef.dev$dt.nm.calc
  mol.coef.dev <- mol.coef.dev$mol.coef.dev
  
  nm.res.nms <- absorbance.residuals(dt.nm.full.m, dt.nm.calc)
  nm.res.rel <- nm.res.nms$nm.res.rel
  nm.res.nms <- nm.res.nms$nm.res.nms
  
  # prepare data to return (transpose wave data)
  
  tbl <- objects()
  tbl <- tbl[tbl %in% c("dt.nm.calc", "nm.res.nms", "nm.res.rel")]
  
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
  cln <- cln[!(cln %in% colnames(dt.ind))]
  
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
    
    nm.save(subdir, sep, dt.res, dt.nm.calc, nm.res.nms, nm.res.rel, nm.err, cnst.dev, cor.m, mol.coef, mol.coef.dev, err.diff, target)
    
  }
  
  
  # return data
  
  if (mode %in% c("script", "api")) {
    
    list("grid.opt" = grid.opt
         ,"dt.eq.conc" = dt.res
         , "dt.nm.calc" = dt.nm.calc
         , "nm.res.nms" = nm.res.nms
         , "nm.res.rel" = nm.res.rel
         , "nm.err" = nm.err
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
    
    vld <- dt.nm.calc[!is.na(S1), which = TRUE]
    
    # return
    
    list("dt.eq.conc" = dt.res
         , "dt.nm.calc" = dt.nm.calc[vld]
         , "nm.res.nms" = nm.res.nms[vld]
         , "nm.res.rel" = nm.res.rel[vld]
         , "nm.err" = nm.err
         , "cnst.dev" = dt.cnst.dev # return data table with additional field for particle names
         , "cor.m" = dt.cor.m
         , "mol.coef" = mol.coef[vld]
         , "mol.coef.dev" = mol.coef.dev.full[vld]
         , "err.diff" = err.diff
         , "cnst.tune" = cnst.tune
         , "lrate.fin" = lrate.fin)
    
  }
  
# }



# # grid research -------------------------------------------- #
# 
# nm.evaluation.grid <- function(mode = c("api", "script", "app")
#                                , sep = ";"
#                                , subdir = ""
#                                , eq.thr.type = c("rel", "abs")
#                                , eq.threshold = 1e-08
#                                , cnst.tune = c("HL", "H2L")
#                                , wl.tune = NULL
#                                , method = "basic wls"
#                                , search.density = 1
#                                , lrate.init = .5
#                                , nm.threshold = 5e-7
#                                , dt.list = NULL) {
#   
#   #
#   
#   nm.threshold <- log(10 ^ nm.threshold)
#   
#   
#   # source code ------------- #
#   
#   dir.start <- ""
#   
#   if (mode %in% c("script", "api"))
#     dir.start <- "app/KEV/"
#   
#   source(paste0(dir.start, "eq_data.r"), chdir = TRUE)
#   source(paste0(dir.start, "nm_data.r"), chdir = TRUE)
#   
#   source(paste0(dir.start, "eq_preproc.r"), chdir = TRUE)
#   source(paste0(dir.start, "nm_preproc.r"), chdir = TRUE)
#   
#   source(paste0(dir.start, "eq_evaluator.r"), chdir = TRUE)
#   source(paste0(dir.start, "nm_evaluator.r"), chdir = TRUE)
#   
#   source(paste0(dir.start, "eq_postproc.r"), chdir = TRUE)
#   source(paste0(dir.start, "nm_postproc.r"), chdir = TRUE)
#   
#   source(paste0(dir.start, "nm_save.r"), chdir = TRUE)
#   
#   
#   # load data ---------------- #
#   
#   if (mode == "script") {
#     
#     dt.ttl <- c(eq.scripts.load(sep, subdir)
#                 , nm.scripts.load(sep, subdir))
#     
#   } else if (mode %in% c("app", "api")) {
#     
#     dt.ttl <- dt.list
#     
#   }
#   
#   dt.coef <- dt.ttl[["dt.coef"]]
#   dt.conc <- dt.ttl[["dt.conc"]]
#   cnst <- dt.ttl[["cnst"]]
#   part.eq <- dt.ttl[["part.eq"]]
#   
#   dt.nm <- dt.ttl[["dt.nm"]]
#   dt.ind <- dt.ttl[["dt.ind"]]
#   
#   
#   # preproc data --------------- #
#   
#   dt.ttl <- c(eq.preproc(dt.coef, cnst, dt.conc, part.eq)
#               , nm.preproc(dt.nm, dt.ind, wl.tune))
#   
#   dt.coef <- dt.ttl[["dt.coef"]]
#   dt.conc <- dt.ttl[["dt.conc"]]
#   cnst.m <- dt.ttl[["cnst.m"]]
#   part.eq <- dt.ttl[["part.eq"]]
#   dt.coef.m <- dt.ttl[["dt.coef.m"]]
#   dt.conc.m <- dt.ttl[["dt.conc.m"]]
#   reac.nm <- dt.ttl[["reac.nm"]]
#   part.nm <- dt.ttl[["part.nm"]]
#   
#   dt.nm <- dt.ttl[["dt.nm"]]
#   dt.nm.full <- dt.ttl[["dt.nm.full"]]
#   dt.nm.err <- dt.ttl[["dt.nm.err"]]
#   dt.nm.err.full <- dt.ttl[["dt.nm.err.full"]]
#   dt.ind <- dt.ttl[["dt.ind"]]
#   dt.ind.full <- dt.ttl[["dt.ind.full"]]
#   dt.nm.m <- dt.ttl[["dt.nm.m"]]
#   dt.nm.full.m <- dt.ttl[["dt.nm.full.m"]]
#   dt.nm.err.m <- dt.ttl[["dt.nm.err.m"]]
#   dt.nm.err.full.m <- dt.ttl[["dt.nm.err.full.m"]]
#   dt.ind.m <- dt.ttl[["dt.ind.m"]]
#   dt.ind.full.m <- dt.ttl[["dt.ind.full.m"]]
#   partprod.nm <- dt.ttl[["partprod.nm"]]
#   wavelength <- dt.ttl[["wavelength"]]
#   
#   cnst.tune.nm <- which(dt.coef[, name] %in% cnst.tune)
#   
#   
#   # run evaluator --------------- #
#   
#   # create grid
#   
#   grid.iter <- lapply(cnst.m[cnst.tune.nm], function(x) { seq(x * .2, x * 2, lrate.init * x) })
#   names(grid.iter) <- cnst.tune.nm
#   
#   grid.iter <- data.table(expand.grid(grid.iter))
#   
#   remove(grid.opt)
#   
#   # run loop
#   
#   for (i in 1:nrow(grid.iter)) {
#     
#     cnst.m[cnst.tune.nm] <- unlist(grid.iter[i])
#     
#     tmp <- constant.optimizer(dt.coef, cnst.m, cnst.tune
#                               , dt.nm.m, dt.nm.err.m, dt.ind.m
#                               , dt.coef.m, dt.conc.m, part.eq, reac.nm
#                               , hardstop = 1
#                               , lrate.init
#                               , search.density
#                               , nm.threshold
#                               , eq.threshold
#                               , eq.thr.type
#                               , mode = "grid"
#                               , method
#                               , algorithm = "basic search")$grid.opt
#     
#     if (exists("grid.opt")) {
#       
#       grid.opt <- rbind(grid.opt, tmp)
#       
#     } else {
#       
#       grid.opt <- copy(tmp)
#       
#     }
#     
#   }
#   
#   # postproc
#   
#   cln <- colnames(grid.opt)
#   cln <- cln[cln %like% "^[0-9]+$"]
#   
#   for (cl in cln) {
#     
#     grid.opt[, eval(cl) := log(exp(eval(as.name(cl))), 10)]
#     
#   }
#   
#   grid.opt
#   
# }
# 
# 
