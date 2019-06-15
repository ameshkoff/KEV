# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #


# curve functions ------------------------------------------ #

kev.gaussian <- function(x, amplitude, expvalue, hwhm) { amplitude * exp(-log(2) * (x - expvalue) ^ 2 / hwhm ^ 2) }

kev.lorentzian <- function(x, amplitude, expvalue, hwhm) { amplitude / (1 + (x - expvalue) ^ 2 / hwhm ^ 2) }


# formula & initial values for the NLS model --------------- #

cur.formula.create <- function(dt.par, dt.cur) {
  
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

cur.formula.execute <- function(dt, formula = NULL, terms = NULL, scalar.values.list) {
  
  # parse formula / terms
  
  if (!is.null(formula)) {
    
    rhs.expr <- paste(labels(terms(formula)), collapse = " + ")
    
  } else if (!is.null(terms)) {
    
    rhs.expr <- paste(terms, collapse = " + ")
    
  }
  
  rhs.expr <- str_replace(rhs.expr, "\\:", " * ")
  rhs.expr <- str_replace(rhs.expr, "\\bI\\(", "(")
  
  # extract scalar values from the list
  
  for (nm in names(scalar.values.list)) {
    
    rhs.expr <- str_replace(rhs.expr, nm, paste0("scalar.values.list[['", nm, "']]"))
    
  }
  
  # create final expression
  
  rhs.expr <- paste0("dt[, ", rhs.expr, "]")
  
  # evaluate & return
  
  pred <- eval(parse(text = rhs.expr))
  
  pred
  
}

cur.formula.effects <- function(dt, formula, scalar.values.list) {
  
  terms <- labels(terms(formula))
  
  dt <- dt[, .(label, observed = value)]
  
  for (trm in terms) {
    
    pred <- cur.formula.execute(dt, terms = trm, scalar.values.list = scalar.values.list)
    
    dt[, new := pred]
    setnames(dt, "new", paste0("curve", str_extract(trm, "[0-9]+")))
    
  }
  
  cln <- colnames(dt)
  cln <- cln[cln %like% "^curve[0-9]+$"]
  
  dt[, predicted := rowSums(dt[, cln, with = FALSE])]
  
  dt
  
}


# NLS model predicting and analysis ------------------------ #

cur.model.predict <- function(dt = NULL, model) {
  
  predict(model, newdata = dt)
  
}

cur.model.effects <- function(dt, model) {
  
  frm <- formula(model)
  coefs <- coefficients(model) %>% as.list()
  
  cur.formula.effects(dt, frm, coefs)
    
}


# modelling --------------------------------------------------

cur.remove.curves <- function(cur.status = kev.curve, min.label = NULL, max.label = NULL) {
  
  dt.par <- cur.status@dt.par
  
  if (!is.null(min.label))
    dt.par <- dt.par[as.numeric(name) >= min.label]
  
  if (!is.null(max.label))
    dt.par <- dt.par[as.numeric(name) <= max.label]
  
  cur.status@dt.par <- dt.par
  
  cur.status
  
}

cur.model <- function(cur.status = kev.curve) {
  
  # prepare
  
  dt <- cur.status@dt.init[label >= cur.status@window.borders[1] & label <= cur.status@window.borders[2]]
  dt.par <- cur.status@dt.par
  
  frm <- cur.formula.create(dt.par, dt)
  
  start.values <- frm[["start.values"]]
  frm <- frm[["formula"]]
  
  # run
  
  md <-
    nls(frm
        , dt
        , start = start.values)
  
  cur.status@model <- md
  
  # metrics
  
  mtr <- cur.model.metrics(dt, md)
  cur.status@metrics <- mtr
  
  # return
  
  cur.status
  
}


