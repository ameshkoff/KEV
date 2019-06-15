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
source(paste0(dir.start, "cur_evaluator.r"), chdir = TRUE)
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



# run modelling

frm <- cur.formula.create(dt.par[as.numeric(name) > 200], dt.cur[label > 200])

start.values <- frm[["start.values"]]
frm <- frm[["formula"]]

init.pred <- cur.formula.execute(dt.cur[label > 210], formula = frm, scalar.values.list = start.values)
init.effects <- cur.formula.effects(dt.cur[label > 210], formula = frm, scalar.values.list = start.values)

cln <- colnames(init.effects)
cln <- cln[cln %like% "^(curve[0-9]+|label)$"]

ggplot(data = melt(init.effects[, cln, with = FALSE], id.vars = "label", variable.name = "Curves")) +
  geom_line(data = init.effects, aes(x = label, y = observed, group = 1), color = "darkgrey", size = 1) +
  geom_line(data = init.effects, aes(x = label, y = predicted, group = 1), color = "darkblue", size = 1, linetype = 2) +
  geom_line(aes(x = label, y = value, group = Curves, color = Curves))


md <-
  nls(frm
    , dt.cur[label > 210]
    , start = start.values)

md.pred <- cur.model.predict(dt.cur[label > 210], md)
md.effects <- cur.model.effects(dt.cur[label > 210], md)

cln <- colnames(md.effects)
cln <- cln[cln %like% "^(curve[0-9]+|label)$"]

ggplot(data = melt(md.effects[, cln, with = FALSE], id.vars = "label", variable.name = "Curves")) +
  geom_line(data = md.effects, aes(x = label, y = observed, group = 1), color = "darkgrey", size = 1) +
  geom_line(data = md.effects, aes(x = label, y = predicted, group = 1), color = "darkblue", size = 1, linetype = 2) +
  geom_line(aes(x = label, y = value, group = Curves, color = Curves))



plot(dt.cur[label > 210], type = "l")
lines(dt.cur[label > 210][, .(label, pred.init)], col = "blue")
lines(dt.cur[label > 210][, .(label, predict(md))], col = "red")








