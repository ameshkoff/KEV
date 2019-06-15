# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #


# plots ---------------------------------------------------- #

cur.plot.initial <- function(cur.ev.results = list(dt.init = NULL, formula.init = NULL, start.values = NULL)) {
  
  dt <- cur.ev.results$dt.init
  frm <- cur.ev.results$formula.init
  start.value <- cur.ev.results$start.values
  
  init.pred <- cur.formula.execute(dt, formula = frm, scalar.values.list = start.values)
  init.effects <- cur.formula.effects(dt, formula = frm, scalar.values.list = start.values)
  
  cln <- colnames(init.effects)
  cln <- cln[cln %like% "^(curve[0-9]+|label)$"]
  
  g <- 
    ggplot(data = melt(init.effects[, cln, with = FALSE], id.vars = "label", variable.name = "Curves")) +
      geom_area(data = init.effects, aes(x = label, y = observed, group = 1), color = "darkgrey", size = 1, fill = "grey") +
      geom_line(data = init.effects, aes(x = label, y = predicted, group = 1), color = "darkblue", size = 1, linetype = 2) +
      geom_line(aes(x = label, y = value, group = Curves, color = Curves))

  g
    
}

cur.plot.model <- function(cur.ev.results = list(dt.init = NULL, model = NULL)) {
  
  dt <- cur.ev.results$dt.init
  md <- cur.ev.results$model

  md.pred <- cur.model.predict(dt, md)
  md.effects <- cur.model.effects(dt, md)
  
  cln <- colnames(md.effects)
  cln <- cln[cln %like% "^(curve[0-9]+|label)$"]
  
  g <-
    ggplot(data = melt(md.effects[, cln, with = FALSE], id.vars = "label", variable.name = "Curves")) +
      geom_area(data = md.effects, aes(x = label, y = observed, group = 1), color = "darkgrey", size = 1, fill = "grey") +
      geom_line(data = md.effects, aes(x = label, y = predicted, group = 1), color = "darkblue", size = 1, linetype = 2) +
      geom_line(aes(x = label, y = value, group = Curves, color = Curves))

  g
    
}

