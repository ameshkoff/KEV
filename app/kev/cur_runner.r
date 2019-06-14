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

# variables (convert to function args later)

# mode = c("api", "script", "app")
mode <- "script"
sep = ";"
subdir = "curves/dsc.1.no.assumptions/semicolon"
# subdir = "curves/dsc.1"
file = NULL
save.res = TRUE
dt.list = NULL


# source code ------------- #

dir.start <- ""

if (mode[1] %in% c("script", "api"))
  dir.start <- "app/KEV/"

source(paste0(dir.start, "cur_data.r"), chdir = TRUE)
source(paste0(dir.start, "cur_preproc.r"), chdir = TRUE)
source(paste0(dir.start, "cur_preevaluator.r"), chdir = TRUE)
# source(paste0(dir.start, "cur_evaluator.r"), chdir = TRUE)
# source(paste0(dir.start, "cur_postproc.r"), chdir = TRUE)
# source(paste0(dir.start, "cur_save.r"), chdir = TRUE)



# load data ---------------- #

if (mode[1] == "script") {
  
  dt.ttl <- cur.scripts.load(sep, subdir, file)
  #dt.ttl <-  cur.scripts.load(sep, subdir, "data.xlsx")
  
} else if (mode[1] %in% c("app", "api")) {
  
  dt.ttl <- dt.list
  
}


# preproc data --------------- #

dt.ttl <- cur.preproc(dt.ttl)

dt.cur <- dt.ttl[["dt.cur"]]
cur.task <- dt.ttl[["cur.task"]]
window.borders <- dt.ttl[["window.borders"]]
dt.par <- dt.ttl[["dt.par"]]


# define assumptions --------------- #

dt.ttl <- cur.assumptions(dt.cur
                          , cur.task
                          , window.borders
                          , dt.par
                          , smooth.delimiter = 30)

dt.par <- dt.ttl[["dt.par"]]


# curve functions

kev.gaussian <- function(x, amplitude, expvalue, hwhm) { amplitude * exp(-log(2) * (x - expvalue) ^ 2 / hwhm ^ 2) }

# create formula & initial values for the model

cur.formula <- function(dt.par, dt.cur) {
  
  fnc.list <- dt.par[, .(design, name)] %>% unique()
  
  frm <- c()
  start.values <- list()
  
  for (i in 1:nrow(fnc.list)) {
    
    if (fnc.list[i, design] == "gaussian") {
      
      frm <- c(frm, paste0("kev.gaussian(label, amplitude", i, ", expvalue", i, ", hwhm", i, ")"))
      
    } else if (fnc.list[i, design] == "lorentzian") {
      
      frm <- c(frm, paste0("kev.lorentzian(label, amplitude", i, ", expvalue", i, ", hwhm", i, ")"))
      
    } else {
      
      
    }
    
    tmp <- dt.par[design == fnc.list[i, design] & name == fnc.list[i, name], .(param, value)]
    
    new.values <- as.list(tmp[, value])
    names(new.values) <- paste0(tmp[, param], i)
    
    start.values <- c(start.values, new.values)
    
  }
  
  frm <- as.formula(paste("value ~ ", paste(frm, collapse = " + ")))
  
  list(formula = frm, start.values = start.values)
  
}


# run modelling

frm <- cur.formula(dt.par[as.numeric(name) > 200], dt.cur[label > 200])

start.values <- frm[["start.values"]]
frm <- frm[["formula"]]

md <-
  nls(frm
    , dt.cur[label > 210]
    , start = start.values)


plot(dt.cur[label > 210], type = "l")
lines(dt.cur[label > 210][, .(label, predict(md))], col = "red")


