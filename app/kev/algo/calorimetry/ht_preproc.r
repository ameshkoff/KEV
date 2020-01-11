# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2020                                                 #
#                                                            #
# ########################################################## #




ht.preproc <- function(dt.heat, dt.enth, dt.coef, dt.conc.m, conc.series, calorimeter.type, cmp.tune = NULL) {
  
  if (is.null(cmp.tune)) cmp.tune <- colnames(dt.coef)[1]
  if (is.null(conc.series)) conc.series <- ""
  
  # enthalpies ---------------------------------- #
  
  if (is.data.table(dt.enth)) {
    
    cln <- colnames(dt.enth)
    setnames(dt.enth, str_to_lower(cln))
    
    # remove standard errors (if output of the previously calculations loaded)
    
    cln <- colnames(dt.enth)
    if (length(cln[cln %like% "adj\\.r\\.squared"]) > 0)
      dt.enth <- dt.enth[, !(cln[cln %like% "adj\\.r\\.squared"]), with = FALSE]
    
    if (is.character(dt.enth[, value])){
      
      dt.enth[, value := str_replace(value, "\\,", ".")]
      dt.enth[, value := str_replace(value, " ", "")]
      dt.enth[, value := as.numeric(value)]
      
    }
    
    # check consistence
    
    enth.nm <- dt.enth[, reaction] %>% unique()
    coef.nm <- dt.coef[, name]
    
    if (length(enth.nm) > 0 && length(setdiff(enth.nm, coef.nm)) > 0) {
      
      stop("Enthalpies reaction names are inconsistent with the component and product names provided with the stechiometric coefficients data")
      
    }
    
  } else {
    
    dt.enth <- data.table(reaction = character(), value = numeric())
    
  }

  # add rows for basic components
    
  cln <- colnames(dt.coef)
  cln <- cln[!(cln %in% c("name"))]
  
  dt.enth <- rbind(data.table(reaction = cln, value = rep(0, length(cln))), dt.enth)
  
  
  # heats --------------------------------------- #

  cln <- dt.heat[, data]
  dt.heat <- transpose(dt.heat[, !"data", with = FALSE])
  setnames(dt.heat, cln)
  
  for(cl in cln[cln != "series"])
    dt.heat[, eval(cl) := as.numeric(str_replace(str_replace_all(eval(as.name(cl)), " ", ""), "\\,", "."))]
  
  if ((length(cln[cln == "series"]) > 0 && (sort(unique(conc.series)) != sort(dt.heat[, series]))) && (nrow(dt.heat) != nrow(dt.conc.m)))
    stop("Input concentrations are inconsistent with the heats data. Heats should correspond either to the series or the experiments in the concentrations data")
  
  # calorimeter type coefficients
  
  calorimeter.type.coef <- data.table(dt.conc.m, series = conc.series)
  calorimeter.type.coef <- calorimeter.type.coef[, .SD[2:.N], by = series]
  calorimeter.type.coef <- calorimeter.type.coef[, eval(as.name(cmp.tune))]

  if (str_to_lower(calorimeter.type) %in% c("dsc", "overfilled")) {
    
    calorimeter.type.coef <- rep(1, times = length(calorimeter.type.coef))
    
  } else if (str_to_lower(calorimeter.type) %in% c("ampoule")) {
    
    calorimeter.type.coef <- 1 / calorimeter.type.coef
    
  }
  
  # return
  
  list("dt.heat" = dt.heat
       , "dt.enth" = dt.enth
       , "calorimeter.type.coef" = calorimeter.type.coef
       )
  
}









