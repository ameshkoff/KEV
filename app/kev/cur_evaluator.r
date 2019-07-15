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



# curve AUC functions -------------------------------------- #

kev.auc.gaussian <- function(x, amplitude, expvalue, hwhm) {  (amplitude * hwhm) / sqrt(log(2) / pi) }

kev.auc.lorentzian <- function(x, amplitude, expvalue, hwhm) { amplitude * hwhm * pi }



# formula & initial values for the NLS model --------------- #

cur.formula.create <- function(dt.par, dt.cur) {
  
  fnc.list <- dt.par[, .(design, name)] %>% unique()
  
  frm <- c()
  start.values <- list()
  
  for (i in 1:nrow(fnc.list)) {
    
    if (fnc.list[i, design] == "gaussian") {
      
      frm <- c(frm, paste0("kev.gaussian(label, `amplitude^^^", fnc.list[i, name]
                           , "`, `expvalue^^^", fnc.list[i, name], "`, `hwhm^^^", fnc.list[i, name], "`)"))
      
    } else if (fnc.list[i, design] == "lorentzian") {
      
      frm <- c(frm, paste0("kev.lorentzian(label, `amplitude^^^", fnc.list[i, name]
                           , "`, `expvalue^^^", fnc.list[i, name], "`, `hwhm^^^", fnc.list[i, name], "`)"))
      
    } else {
      
      
    }
    
    tmp <- dt.par[design == fnc.list[i, design] & name == fnc.list[i, name], .(param, value)]
    
    new.values <- as.list(tmp[, value])
    names(new.values) <- paste0(tmp[, param], "^^^", fnc.list[i, name])
    
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
    
    rhs.expr <- str_replace(rhs.expr
                            , fixed(paste0("`", nm, "`"))
                            , scalar.values.list[[nm]])

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
    
    cl <- str_split(trm, "`") %>% unlist()
    cl <- cl[cl %like% "\\^\\^\\^"][1] %>% str_replace("^.*\\^\\^\\^", "")
    
    setnames(dt, "new", paste0("Curve ", cl))
    
  }
  
  cln <- colnames(dt)
  cln <- cln[cln %like% "^Curve "]
  
  dt[, predicted := rowSums(dt[, cln, with = FALSE])]
  
  dt
  
}

cur.auc <- function(cur.status = kev.curve) {

  frm <- cur.formula.create(cur.status@dt.par, cur.status@dt.init)
  scalar.values.list <- frm$start.values

  terms <- labels(terms(frm$formula))
  terms <- terms[terms %like% "^kev\\."]
  
  dt <- data.table(term = terms)
  dt[, term := str_replace(term, "^kev\\.", "kev.auc.")]

  dt[, name := mapply(function(x) {
    cl <- str_split(x, "`") %>% unlist()
    cl <- cl[cl %like% "\\^\\^\\^"][1] %>% str_replace("^.*\\^\\^\\^", "")
  }, term)]
    
  for (nm in names(scalar.values.list)) {
    
    dt[, term := str_replace(term
                            , fixed(paste0("`", nm, "`"))
                            , scalar.values.list[[nm]])]
    
  }
  
  dt[, area := mapply(function(x) {eval(parse(text = x))}, term, SIMPLIFY = TRUE)]
  
  dt

}


# objective function --------------------------------------- #

cur.objective.rmse <- function(err.v) {
  
  # factory
  
  factory <- list()
  
  # logging
  
  err.v <- integer(0)
  pred.list <- list()
  
  factory$error.log <- function(){ err.v }
  factory$pred.log <- function(){ pred.list }

  # objective function
    
  factory$objective <- function(scalar.values.vector, dt, formula, obs) {
     
      scalar.values.list <- as.list(scalar.values.vector)
      
      pred <- cur.formula.execute(dt, formula = formula, scalar.values.list = scalar.values.list)

      rtrn <- mean((obs - pred) ^ 2) ^ .5
      
      err.v <<- c(err.v, rtrn)
      
      err.min.pos <- which(err.v == min(err.v)) %>% tail(1)
      if (err.min.pos != 1 && err.min.pos != length(err.v)) pred.list <<- tail(pred.list, length(err.v) - err.min.pos)
      pred.list[[as.character(length(err.v))]] <<- pred
      
      rtrn
      
    }
  
  factory
  
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

cur.object.effects <- function(cur.status = kev.curve) {
  
  frm <- cur.formula.create(cur.status@dt.par, cur.status@dt.init)
  cur.formula.effects(cur.status@dt.init, frm$formula, frm$start.values)
  
}

  
# processing ----------------------------------------------- #

cur.remove.curves <- function(cur.status = kev.curve, min.expvalue = NULL, max.expvalue = NULL) {
  
  dt.par <- cur.status@dt.par
  
  tmp <- dt.par[param == "expvalue", .(name, value)]
  
  if (!is.null(min.expvalue)) {
    
    dt.par <- dt.par[!(name %in% tmp[value < min.expvalue, name])]
    
  }

  if (!is.null(max.expvalue)) {
    
    dt.par <- dt.par[!(name %in% tmp[value > max.expvalue, name])]
    
  }
  
  cur.status@dt.par <- dt.par
  
  cur.status
  
}

cur.model.coefs <- function(md) {
  
  if (is(md, "nls")) {
    
    coefs <- as.data.table(summary(md)$parameters, keep.rownames = TRUE)
    coefs <- coefs[, .(param = rn, value = Estimate, st.error = `Std. Error`)]
    
  } else if (is.list(md) && !is.null(md$par)) {
    
    coefs <- md$par
    
    
    
    browser()
    
    
    
    
  }
  
  coefs
  
}

cur.parameters.update <- function(dt.par, md){
  
  dt.par <- copy(dt.par)
  coefs <- cur.model.coefs(md)
  
  coefs[, name := str_replace_all(str_extract(param, "\\^\\^\\^.*$"), "\\^\\^\\^", "")]
  coefs[, param := str_replace_all(str_extract(param, "^.*\\^\\^\\^"), "\\^\\^\\^", "")]

  dt.par <- coefs[dt.par, on = .(name = name, param = param)]
  dt.par[is.na(st.error), value := i.value]
  dt.par[, i.value := NULL]
  
  dt.par <- dt.par[, .(name, design, param, value, st.error)]
  
  dt.par
  
}


# modelling ------------------------------------------------ #

cur.model.gaussnewton <- function(cur.status = kev.curve) {
  
  # prepare
  
  dt <- cur.status@dt.init[label >= cur.status@window.borders[1] & label <= cur.status@window.borders[2]]
  dt.par <- cur.status@dt.par
  
  frm <- cur.formula.create(dt.par, dt)
  
  start.values <- frm[["start.values"]]
  frm <- frm[["formula"]]
  
  # run
  
  md <-
    try(  
      nls(frm
          , dt
          , start = start.values
          , control = list(maxiter = 500))
      , silent = TRUE
    )
  
  if (is(md, "nls")) {
    
    cur.status@model <- md
    cur.status@model.status <- "OK"
    cur.status@metrics <- cur.model.metrics(dt, cur.model.predict(dt, md))
    cur.status@dt.par <- cur.parameters.update(dt.par, md)
    
  } else if (is(md, "try-error")) {
    
    cur.status@model.status <- "Error: Convergence failed: Rolled back to the stable model"
    warning(paste("Error: Convergence failed:", attr(md, "condition")$message))
    
  }
  
  # return
  
  cur.status
  
}

cur.model.neldermead <- function(cur.status = kev.curve) {
  
  # prepare
  
  dt <- cur.status@dt.init[label >= cur.status@window.borders[1] & label <= cur.status@window.borders[2]]
  dt.par <- cur.status@dt.par
  
  frm <- cur.formula.create(dt.par, dt)
  
  start.values <- frm[["start.values"]]
  frm <- frm[["formula"]]
  
  # run
  
  pred.list <- list()
  # err.v <- ""
  
  obj.fn <- cur.objective.rmse()
  
  md <- optim(unlist(start.values), obj.fn$objective, dt = dt, formula = frm, obs = dt[, value]
              , method = "Nelder-Mead", control = list(maxit = 1e+4, reltol = 1e-7))
  
  if (is.list(md) && md$convergence < 10) {

    md$value.log <- obj.fn$error.log()
    md$pred.log <- obj.fn$pred.log()
    
    cur.status@model <- md
    cur.status@model.status <- "OK"
    cur.status@metrics <- cur.model.metrics(dt, cur.formula.execute(dt, formula = frm, scalar.values.list = as.list(md$par)))
    cur.status@dt.par <- cur.parameters.update(dt.par, md)
    
  } else if (md$convergence >= 10) {
    
    cur.status@model.status <- "Error: Convergence failed: Rolled back to the stable model"
    warning(paste("Error: Convergence failed:", md$message))
    
  }
  
  # return
  
  cur.status
  
}




cur.model <- function(cur.status = kev.curve, algorithm = "gaussnewton") {
  
  if (algorithm == "gaussnewton") {
    
    cur.status <- cur.model.gaussnewton(cur.status)
    
  } else if (algorithm == "neldermead") {
    
    cur.status <- cur.model.neldermead(cur.status)
    
  }
  
  
  # return
  
  cur.status
  
}






