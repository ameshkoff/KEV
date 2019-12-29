# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2020                                                 #
#                                                            #
# ########################################################## #




ht.preproc <- function(dt.heat, dt.enth, dt.coef, conc.series, cmp.tune = NULL) {
  
  # enthalpies
  
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
    
  }
  
  # heats

  cln <- dt.heat[, data]
  dt.heat <- transpose(dt.heat[, !"data", with = FALSE])
  setnames(dt.heat, cln)
  
  cln <- cln[cln != "series"]
  for(cl in cln)
    dt.heat[, eval(cl) := as.numeric(str_replace(str_replace_all(eval(as.name(cl)), " ", ""), "\\,", "."))]
  
  if ((sort(unique(conc.series)) != sort(dt.heat[, series])) && (nrow(dt.heat) != nrow(dt.conc)))
    stop("Input concentrations are inconsistent with the heats data. Heats should correspond either to the series or the experiments in the concentrations data")
  
  list("dt.heat" = dt.heat
       , "dt.enth" = dt.enth
       )
  
}









