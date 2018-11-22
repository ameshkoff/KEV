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

mode = "script"
sep = ","
subdir = "emf/dsp.1"
eq.thr.type = "rel"
eq.threshold = 1e-08
cnst.tune = NULL
algorithm = "direct search"
method = "basic wls"
emf.mode = c("base", "grid", "debug")
search.density = 1
lrate.init = .5
emf.threshold = 5e-7
save.res = TRUE
dt.list = NULL
check.sens = TRUE
  
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
# source(paste0(dir.start, "emf_evaluator.r"), chdir = TRUE)

source(paste0(dir.start, "eq_postproc.r"), chdir = TRUE)
# source(paste0(dir.start, "emf_postproc.r"), chdir = TRUE)

# source(paste0(dir.start, "emf_save.r"), chdir = TRUE)


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










