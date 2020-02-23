# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2020                                                 #
#                                                            #
# ########################################################## #




ht.preproc <- function(dt.heat, dt.enth, dt.coef, dt.conc.m, conc.series, init.vol, calorimeter.type, cmp.tune = NULL) {
  
  # misc --------------------------------------- #
  
  if (is.null(cmp.tune)) cmp.tune <- colnames(dt.coef)[1]
  if (is.null(conc.series)) conc.series <- rep("", nrow(dt.conc.m))
  
  calorimeter.type <- str_to_lower(calorimeter.type)
  
  init.vol <- str_replace_all(init.vol, "\\,", ".")
  init.vol <- str_replace_all(init.vol, " ", "")
  init.vol <- as.numeric(init.vol)
  
  # enthalpies ---------------------------------- #
  
  if (is.data.table(dt.enth) && ncol(dt.enth) > 1) {
    
    cln <- colnames(dt.enth)
    setnames(dt.enth, str_to_lower(cln))
    
    # remove standard errors (if output of the previously calculations loaded)
    
    cln <- colnames(dt.enth)
    if (length(cln[cln %like% "^dev"]) > 0)
      dt.enth <- dt.enth[, !(cln[cln %like% "^dev"]), with = FALSE]
    
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

  cln <- colnames(dt.heat)

  if (length(cln[cln %like% "^observ"]) == 0) {
    
    cln <- dt.heat[, data]
    dt.heat <- transpose(dt.heat[, !"data", with = FALSE])
    setnames(dt.heat, cln)
    
  }
  
  cln.remove <- cln[cln %like% "^(res|heats)"]
  if (length(cln.remove) > 0)
    dt.heat <- dt.heat[, !cln.remove, with = FALSE]
  
  for(cl in cln[!(cln %in% c("series", cln.remove))])
    dt.heat[, eval(cl) := as.numeric(str_replace(str_replace_all(eval(as.name(cl)), " ", ""), "\\,", "."))]
  
  if ((length(cln[cln == "series"]) > 0 && (sort(unique(conc.series)) != sort(dt.heat[, series]))) && (nrow(dt.heat) != nrow(dt.conc.m)))
    stop("Input concentrations are inconsistent with the heats data. Heats should correspond either to the series or the experiments in the concentrations data")
  
  if (length(cln[cln == "volumes"]) == 0) dt.heat[, volumes := 1]
  
  dt.heat[, heats := observation - dilution]
  
  # calorimeter type coefficients
  
  calorimeter.type.coef <- data.table(dt.conc.m, series = conc.series)
  calorimeter.type.coef <- calorimeter.type.coef[, .SD[2:.N], by = series]
  calorimeter.type.coef <- calorimeter.type.coef[, eval(as.name(cmp.tune))]
  
  if (calorimeter.type %in% c("dsc", "overfilled")) {
    
    calorimeter.type.coef <- rep(1, times = length(calorimeter.type.coef))
    
  } else if (str_to_lower(calorimeter.type) %in% c("ampoule")) {
    
    calorimeter.type.coef <- 1 / calorimeter.type.coef
    
  }
  
  # return
  
  list("dt.heat" = dt.heat
       , "dt.enth" = dt.enth
       , "init.vol" = init.vol
       , "calorimeter.type" = calorimeter.type
       , "calorimeter.type.coef" = calorimeter.type.coef
       , "conc.series" = conc.series
       )
  
}




