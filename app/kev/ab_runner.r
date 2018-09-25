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

# args

mode <- "script"
sep <- ","
subdir <- "dsl.3"
cnst.tune <- c("HL", "H2L")
ab.threshold <- log(10 ^ 5e-7)


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

source(paste0(dir.start, "eq_save.r"), chdir = TRUE)


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
            , ab.preproc(dt.ab, dt.mol))

dt.coef <- dt.ttl[["dt.coef"]]
dt.conc <- dt.ttl[["dt.conc"]]
cnst.m <- dt.ttl[["cnst.m"]]
part.eq <- dt.ttl[["part.eq"]]
dt.coef.m <- dt.ttl[["dt.coef.m"]]
dt.conc.m <- dt.ttl[["dt.conc.m"]]
reac.nm <- dt.ttl[["reac.nm"]]
part.nm <- dt.ttl[["part.nm"]]

dt.ab <- dt.ttl[["dt.ab"]]
dt.ab.err <- dt.ttl[["dt.ab.err"]]
dt.mol <- dt.ttl[["dt.mol"]]
dt.ab.m <- dt.ttl[["dt.ab.m"]]
dt.ab.err.m <- dt.ttl[["dt.ab.err.m"]]
dt.mol.m <- dt.ttl[["dt.mol.m"]]
partprod.nm <- dt.ttl[["partprod.nm"]]

cnst.tune.nm <- which(dt.coef[, name] %in% cnst.tune)


# run evaluator --------------- #

exec.time <- system.time(
  dt.ttl <- constant.optimizer(dt.coef, cnst.m, cnst.tune
                               , dt.ab.m, dt.ab.err.m, dt.mol.m
                               , dt.coef.m, dt.conc.m, part.eq, reac.nm
                               , hardstop = 400
                               , lrate.init = .5
                               , search.density = 1
                               , ab.threshold
                               , eq.threshold = 1e-08
                               , eq.thr.type = "rel"
                               , mode = "base", method = "basic wls", algorithm = "direct search"))[3]

cnst.m <- dt.ttl[["cnst.m"]]
cnst.m.10 <- log(exp(cnst.m), 10)
mol.coef <- dt.ttl[["mol.coef"]]
dt.ab.calc <- dt.ttl[["dt.ab.calc"]]
dt.res.m <- dt.ttl[["dt.res.m"]]
ab.err <- tail(dt.ttl[["grid.opt"]][!is.na(err), err], 1)


# postprocessing ---------------- #

dt.res <- data.table(dt.res.m)

dt.conc.calc <- eq.tot.conc.calc(dt.res, cnst.m, dt.coef.m, part.nm)
dt.err <- eq.residuals(dt.conc.m, dt.conc.calc, part.eq)

dt.conc.tot <- copy(dt.conc.m)
dt.conc.tot[, part.eq] <- dt.conc.calc[, part.eq]

cov.m <- ab.cov(ab.err
               , cnst.m
               , cnst.tune.nm
               , dt.ab.err.m, dt.coef, dt.coef.m, dt.conc.m, part.eq, reac.nm
               , eq.thr.type = "rel", eq.threshold = 1e-08
               , method = "basic wls", ab.threshold)

cor.m <- cov.m$cor.m
cov.m <- cov.m$cov.m

cnst.dev <- constant.deviations(cnst.m, cov.m, cnst.tune.nm)
mol.coef.dev <- molar.coef.deviations(cnst.m
                                      , cnst.tune.nm
                                      , dt.coef, dt.coef.m, dt.conc.m, part.eq, reac.nm
                                      , eq.thr.type = "rel", eq.threshold = 1e-08
                                      , method = "basic wls"
                                      , ab.threshold)




