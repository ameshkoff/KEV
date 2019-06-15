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
library(ggplot2)
# computation
library(MASS)
library(Matrix)
library(Hmisc)
# strings
library(stringi)
library(stringr)

# Curves status as an object ----------------------------------

setClassUnion("character.or.NULL", c("character", "NULL"))
setClassUnion("numeric.or.NULL", c("numeric", "NULL"))
setClassUnion("data.table.or.NULL", c("data.table", "NULL"))

setClass("kev.curve", slots = list(mode = "character" # c("api", "script", "app")
                                   , sep = "character"
                                   , subdir = "character"
                                   , file = "character.or.NULL"
                                   , save.res = "logical"
                                   , dt.init = "data.table.or.NULL"
                                   , dt.par = "data.table.or.NULL"
                                   , cur.task = "character.or.NULL"
                                   , window.borders = "numeric.or.NULL"))



# load and cleanse initial data ------------------------------

cur.preproc.runner <- function(cur.params = kev.curve, dt.list = NULL) {

  # source code ------------- #
  
  dir.start <- ""
  
  if (cur.params@mode[1] %in% c("script", "api"))
    dir.start <- "app/KEV/"
  
  source(paste0(dir.start, "cur_data.r"), chdir = TRUE)
  source(paste0(dir.start, "cur_preproc.r"), chdir = TRUE)
  source(paste0(dir.start, "cur_preevaluator.r"), chdir = TRUE)
  source(paste0(dir.start, "cur_evaluator.r"), chdir = TRUE)
  source(paste0(dir.start, "cur_postproc.r"), chdir = TRUE)
  # source(paste0(dir.start, "cur_save.r"), chdir = TRUE)
  
  
  # load data ---------------- #
  
  if (cur.params@mode[1] == "script") {
    
    dt.ttl <- cur.scripts.load(cur.params@sep, cur.params@subdir, cur.params@file)
  
  } else if (cur.params@mode[1] %in% c("app", "api")) {
    
    dt.ttl <- dt.list
    
  }
  
  
  # preproc data --------------- #
  
  dt.ttl <- cur.preproc(dt.ttl)
  
  dt.cur <- dt.ttl[["dt.cur"]]
  cur.task <- dt.ttl[["cur.task"]]
  window.borders <- dt.ttl[["window.borders"]]
  dt.par <- dt.ttl[["dt.par"]]
  
  
  # define assumptions --------------- #
  
  if (is.null(dt.par) || nrow(dt.par) == 0) {
    
    dt.par <- cur.assumptions(dt.cur
                              , cur.task
                              , window.borders
                              , dt.par
                              , smooth.delimiter = 30)$dt.par
  }
  
  cur.params@dt.init <- dt.cur
  cur.params@dt.par <- dt.par
  cur.params@cur.task <- cur.task
  cur.params@window.borders <- window.borders
  
  cur.params

}

# run loading & preprocessing -----------------------------

cur.status <- new("kev.curve"
                  , mode = "script"
                  , sep = ";"
                  , subdir = "curves/dsc.1.no.assumptions/semicolon"
                  , file = NULL
                  , save.res = TRUE
                  , dt.init = NULL
                  , dt.par = NULL
                  , cur.task = NULL
                  , window.borders = NULL)

cur.status <- cur.preproc.runner(cur.status)


# run modelling

frm <- cur.formula.create(dt.par[as.numeric(name) > 200], dt.cur[label > 200])

start.values <- frm[["start.values"]]
frm <- frm[["formula"]]

cur.plot.initial(list(dt.init = dt.cur[label > 210]
                      , formula.init = frm
                      , start.values = start.values))

md <-
  nls(frm
    , dt.cur[label > 200]
    , start = start.values)

cur.plot.model(list(dt.init = dt.cur[label > 200]
                    , model = md))










