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
subdir = "curves/dsc.1/semicolon"
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
border.left <- dt.ttl[["border.left"]]
border.right <- dt.ttl[["border.right"]]
dt.par <- dt.ttl[["dt.par"]]





