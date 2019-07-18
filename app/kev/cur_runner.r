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


if (Sys.info()["sysname"] %like% "indows")
  Sys.setenv("R_ZIPCMD" = "c:/Rtools/bin/zip.exe")


# curve status as an object ----------------------------------

setOldClass("nls")

setClassUnion("character.or.NULL", c("character", "NULL"))
setClassUnion("numeric.or.NULL", c("numeric", "NULL"))
setClassUnion("list.or.NULL", c("list", "NULL"))
setClassUnion("data.table.or.NULL", c("data.table", "NULL"))
setClassUnion("formula.or.NULL", c("formula", "NULL"))
setClassUnion("cur.model", c("nls", "list", "NULL"))

setClass("kev.curve", slots = list(mode = "character"
                                   , sep = "character"
                                   , subdir = "character"
                                   , dt.init = "data.table.or.NULL"
                                   , dt.par = "data.table.or.NULL"
                                   , cur.task = "character.or.NULL"
                                   , window.borders = "numeric.or.NULL"
                                   , formula.init = "formula.or.NULL"
                                   , start.values = "list.or.NULL"
                                   , model = "cur.model"
                                   , metrics = "list.or.NULL"
                                   , model.status = "character"))


# load and preproccess initial data --------------------------

cur.data.runner <- function(mode = c("api", "script", "app")
                            , sep = ","
                            , subdir = ""
                            , file = NULL
                            , dt.list = NULL) {

  cur.status <- new("kev.curve"
                    , mode = mode
                    , sep = sep
                    , subdir = subdir
                    , dt.init = NULL
                    , dt.par = NULL
                    , cur.task = NULL
                    , window.borders = NULL
                    , formula.init = NULL
                    , start.values = NULL
                    , model = NULL
                    , metrics = NULL
                    , model.status = "Not Runned")

  # source code ------------- #
  
  dir.start <- ""
  
  if (cur.status@mode[1] %in% c("script", "api"))
    dir.start <- "app/KEV/"
  
  source(paste0(dir.start, "cur_data.r"), chdir = TRUE)
  source(paste0(dir.start, "cur_preproc.r"), chdir = TRUE)
  source(paste0(dir.start, "cur_evaluator.r"), chdir = TRUE)

  
  # load data ---------------- #
  
  if (cur.status@mode[1] == "script") {
    
    dt.ttl <- cur.scripts.load(cur.status@sep, cur.status@subdir, file)
  
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

cur.model.metrics <- function(dt, pred) {
  
  # pred <- cur.model.predict(dt, model)
  obs <- dt[, value]
  
  residuals.abs <- pred - obs
  residuals.rel <- residuals.abs / obs
  
  residuals.abs <- dt[, .(label, residuals.abs = residuals.abs)]
  residuals.rel <- dt[, .(label, residuals.rel = residuals.rel)]
  
  rmse <- mean((obs - pred) ^ 2) ^ .5
  r.squared <- 1 - (sum((obs - pred) ^ 2) / length(obs)) / var(obs)
  mae <- mean(abs(obs - pred))
  
  list(residuals.abs = residuals.abs, residuals.rel = residuals.rel
       , r.squared = r.squared, rmse = rmse, mae = mae)
  
}


# save -------------------------------------------------------

cur.save <- function(cur.status = kev.curve
                     , file = NULL) {
  
  dir.output <- ""
  
  # define output directory for script mode
  
  if (cur.status@mode[1] %in% c("script", "api")) {
    
    dir.output <- "output/"
    
    if (!is.na(cur.status@subdir) || cur.status@subdir == "") {
      
      dir.output <- paste0(dir.output, "/", cur.status@subdir, "/")
      dir.output <- str_replace_all(dir.output, "//", "/")
      
      dir.create(file.path(dir.output), recursive = TRUE, showWarnings = FALSE)
      
    }

  }
  
  # prepare datasets
  
  dt.par <- cur.status@dt.par
  
  if (nrow(dt.par[(is.na(name) | name == "") & param %in% c("left", "right")]) < 2)
    dt.par <- rbind(dt.par, data.table(param = c("left", "right")
                                       , value = cur.status@window.borders), use.names = TRUE, fill = TRUE)

  if (nrow(dt.par[(is.na(name) | name == "") & param %in% c("cur.task")]) == 0)
    dt.par <- rbind(dt.par, data.table(param = "cur.task"
                                       , value = cur.status@cur.task), use.names = TRUE, fill = TRUE)
  
  
  dt.auc <- cur.auc(cur.status)[, !c("term"), with = FALSE]
  dt.object.effects <- cur.object.effects(cur.status)
  
  if (!is.null(cur.status@model)) {
    
    dt.residuals <- cbind(cur.status@metrics$residuals.abs, cur.status@metrics$residuals.rel[, .(residuals.rel)])
    
    nm <- names(cur.status@metrics)
    nm <- nm[!(nm %like% "residual")]
    
    dt.metrics <- as.data.table(cur.status@metrics[nm])
    
  } else {
    
    dt.residuals <- data.table(not.runned = "not.runned")
    dt.metrics <- data.table(not.runned = "not.runned")
    
  }
    
  
  # combine in a list to perform loop / vector operation
  
  dt.list <- list("input_data" = cur.status@dt.init
                  , "output_params" = dt.par
                  , "output_area_under_curve" = dt.auc
                  , "output_calculated_curves" = dt.object.effects
                  , "output_residuals" = dt.residuals
                  , "output_metrics" = dt.metrics)
  
  # save
  
  if (is.null(file) || str_detect(file, "\\.zip$")) {
    
    # plain text
    
    if (cur.status@sep == ";") {
      
      for (i in 1:length(dt.list))
        write.csv2(dt.list[[i]], file = paste0(dir.output, paste0(names(dt.list)[i], ".csv")), row.names = FALSE)

    } else if (cur.status@sep == ",") {
      
      for (i in 1:length(dt.list))
        write.csv(dt.list[[i]], file = paste0(dir.output, paste0(names(dt.list)[i], ".csv")), row.names = FALSE)
      
    } else if (cur.status@sep == "tab") {
      
      for (i in 1:length(dt.list))
        write.table(dt.list[[i]], file = paste0(dir.output, paste0(names(dt.list)[i], ".csv")), sep = "\t", row.names = FALSE)
      
    }

  } else if (str_detect(file, "\\.xlsx$")) {
    
    # XLSX
    
    if (cur.status@mode[1] %in% c("script", "api")) file <- paste0(dir.output, file)

    write.xlsx(dt.list, file)
        
  }

  # zip
  
  if (!is.null(file) && str_detect(file, "\\.zip$")) {
    
    utils::zip(paste0(dir.output, file), paste0(dir.output, names(dt.list), ".csv"))
    
    # remove garbage from the disc
    
    for (i in paste0(dir.output, names(dt.list), ".csv")) {
      
      if (file.exists(i))
        file.remove(i)
      
    }

  }

}





