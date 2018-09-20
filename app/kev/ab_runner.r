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
subdir <- "dsl.2"
cnst.tune <- c("CuL2")#"SB"#"CuL2"
lrate.init <- .5
search.density <- 1
ab.threshold <- 5e-5

# load data

dir.start <- ""

if (mode %in% c("script", "api"))
  dir.start <- "app/KEV/"

source(paste0(dir.start, "eq_data.r"), chdir = TRUE)
source(paste0(dir.start, "ab_data.r"), chdir = TRUE)

source(paste0(dir.start, "eq_preproc.r"), chdir = TRUE)
source(paste0(dir.start, "ab_preproc.r"), chdir = TRUE)

source(paste0(dir.start, "eq_evaluator.r"), chdir = TRUE)
source(paste0(dir.start, "eq_postproc.r"), chdir = TRUE)
source(paste0(dir.start, "eq_save.r"), chdir = TRUE)

# load data

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

# preproc data

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












