# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #


# plots ------------------------------------------------------

cur.plot.initial <- function(cur.status = kev.curve) {
  
  dt <- cur.status@dt.init
  frm <- cur.status@formula.init
  start.values <- cur.status@start.values
  
  init.pred <- cur.formula.execute(dt, formula = frm, scalar.values.list = start.values)
  init.effects <- cur.formula.effects(dt, formula = frm, scalar.values.list = start.values)
  
  cln <- colnames(init.effects)
  cln <- cln[cln %like% "^(Curve .*|label)$"]
  
  g <-
    ggplot() +
      geom_area(data = init.effects, aes(x = label, y = observed, group = 1), color = "darkgrey", size = 1, fill = "grey") +
      geom_line(data = init.effects, aes(x = label, y = predicted, group = 1), color = "darkblue", size = 1, linetype = 2) +
      geom_line(data = melt(init.effects[, cln, with = FALSE], id.vars = "label", variable.name = "Curves")
                , aes(x = label, y = value, group = Curves, color = Curves)) +
      geom_rect(aes(xmin = -Inf, xmax = cur.status@window.borders[1], ymin = 0, ymax = Inf), alpha = .1) +
      geom_rect(aes(xmin = cur.status@window.borders[2], xmax = Inf, ymin = 0, ymax = Inf), alpha = .1)
    
  g
    
}

cur.plot.model <- function(cur.status = cur.status) {
  
  dt <- cur.status@dt.init
  md <- cur.status@model

  md.pred <- cur.model.predict(dt, md)
  md.effects <- cur.model.effects(dt, md)
  
  cln <- colnames(md.effects)
  cln <- cln[cln %like% "^(Curve .*|label)$"]
  
  g <-
    ggplot() +
      geom_area(data = md.effects, aes(x = label, y = observed, group = 1), color = "darkgrey", size = 1, fill = "grey") +
      geom_line(data = md.effects, aes(x = label, y = predicted, group = 1), color = "darkblue", size = 1, linetype = 2) +
      geom_line(data = melt(md.effects[, cln, with = FALSE], id.vars = "label", variable.name = "Curves")
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




