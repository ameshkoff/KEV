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

# curve status as an object ----------------------------------

setOldClass("nls")

setClassUnion("character.or.NULL", c("character", "NULL"))
setClassUnion("numeric.or.NULL", c("numeric", "NULL"))
setClassUnion("list.or.NULL", c("list", "NULL"))
setClassUnion("data.table.or.NULL", c("data.table", "NULL"))
setClassUnion("formula.or.NULL", c("formula", "NULL"))
setClassUnion("nls.or.NULL", c("nls", "NULL"))

setClass("kev.curve", slots = list(mode = "character"
                                   , sep = "character"
                                   , subdir = "character"
                                   , file = "character.or.NULL"
                                   , save.res = "logical"
                                   , dt.init = "data.table.or.NULL"
                                   , dt.par = "data.table.or.NULL"
                                   , cur.task = "character.or.NULL"
                                   , window.borders = "numeric.or.NULL"
                                   , formula.init = "formula.or.NULL"
                                   , start.values = "list.or.NULL"
                                   , model = "nls.or.NULL"
                                   , metrics = "list.or.NULL"
                                   , model.status = "character"))


# load and preproccess initial data --------------------------

cur.data.runner <- function(mode = c("api", "script", "app")
                            , sep = ","
                            , subdir = ""
                            , file = NULL
                            , save.res = TRUE
                            , dt.list = NULL) {

  cur.status <- new("kev.curve"
                    , mode = mode
                    , sep = sep
                    , subdir = subdir
                    , file = file
                    , save.res = save.res
                    , dt.init = NULL
                    , dt.par = NULL
                    , cur.task = NULL
                    , window.borders = NULL
                    , formula.init = NULL
                    , start.values = NULL
                    , model = NULL
                    , metrics = NULL
                    , model.status = "Warning: Not Runned")

  # source code ------------- #
  
  dir.start <- ""
  
  if (cur.status@mode[1] %in% c("script", "api"))
    dir.start <- "app/KEV/"
  
  source(paste0(dir.start, "cur_data.r"), chdir = TRUE)
  source(paste0(dir.start, "cur_preproc.r"), chdir = TRUE)
  source(paste0(dir.start, "cur_evaluator.r"), chdir = TRUE)
  # source(paste0(dir.start, "cur_save.r"), chdir = TRUE)
  
  
  # load data ---------------- #
  
  if (cur.status@mode[1] == "script") {
    
    dt.ttl <- cur.scripts.load(cur.status@sep, cur.status@subdir, cur.status@file)
  
  } else if (cur.status@mode[1] %in% c("app", "api")) {
    
    dt.ttl <- dt.list
    
  }
  
  
  # preproc data --------------- #
  
  dt.ttl <- cur.preproc(dt.ttl)
  
  dt.cur <- dt.ttl[["dt.cur"]]
  cur.task <- dt.ttl[["cur.task"]]
  window.borders <- dt.ttl[["window.borders"]]
  dt.par <- dt.ttl[["dt.par"]]
  
  
  # initial guess --------------- #
  
  if (is.null(dt.par) || nrow(dt.par) == 0) {
    
    dt.par <- cur.initial.guess(dt.cur
                              , cur.task
                              , window.borders
                              , dt.par
                              , smooth.delimiter = 30)
  }
  
  # get initial values and formula --- #
  
  frm <- cur.formula.create(dt.par, dt.cur)
  
  start.values <- frm[["start.values"]]
  frm <- frm[["formula"]]
  
  # return  
  
  cur.status@dt.init <- dt.cur
  cur.status@dt.par <- dt.par
  cur.status@cur.task <- cur.task
  cur.status@window.borders <- window.borders
  cur.status@formula.init <- frm
  cur.status@start.values <- start.values
  
  cur.status

}


# plots ------------------------------------------------------

cur.plot.effects <- function(cur.status = kev.curve) {
  
  dt <- cur.status@dt.init
  frm <- cur.formula.create(cur.status@dt.par, dt)
  
  extr.effects <- cur.formula.effects(dt, frm$formula, frm$start.values)
  
  cln <- colnames(extr.effects)
  cln <- cln[cln %like% "^(Curve .*|label)$"]
  
  g <-
    ggplot() +
    geom_area(data = extr.effects, aes(x = label, y = observed, group = 1), color = "darkgrey", size = 1, fill = "grey") +
    geom_line(data = extr.effects, aes(x = label, y = predicted, group = 1), color = "darkblue", size = 1, linetype = 2) +
    geom_line(data = melt(extr.effects[, cln, with = FALSE], id.vars = "label", variable.name = "Curves")
              , aes(x = label, y = value, group = Curves, color = Curves)) +
    geom_rect(aes(xmin = -Inf, xmax = cur.status@window.borders[1], ymin = 0, ymax = Inf), alpha = .1) +
    geom_rect(aes(xmin = cur.status@window.borders[2], xmax = Inf, ymin = 0, ymax = Inf), alpha = .1)
  
  g
  
}


# metrics and residuals --------------------------------------

cur.model.metrics <- function(dt, model) {
  
  pred <- cur.model.predict(dt, model)
  obs <- dt[, value]
  
  residuals.abs <- pred - obs
  residuals.rel <- residuals.abs / obs
  
  residuals.abs <- dt[, .(label, residuals.abs = residuals.abs)]
  residuals.rel <- dt[, .(label, residuals.rel = residuals.rel)]
  
  r.squared <- 1 - (sum((obs - pred) ^ 2) / length(obs)) / var(obs)
  
  list(residuals.abs = residuals.abs, residuals.rel = residuals.rel, r.squared = r.squared)
  
}





