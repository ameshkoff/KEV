# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #




nm.preproc <- function(dt.nm, dt.ind, dt.coef, dt.conc.m, part.eq) {

  # extract signal names ---------------------- #
  
  nm.s <- dt.nm[data %like% "^obs", signal]
  
  # if some individual shifts are provided ---- #
  
  if (is.data.table(dt.ind)) {
    
    cln <- colnames(dt.ind)

    # remove standard errors (if output of the previously calculations loaded)
    
    cln <- colnames(dt.ind)
    if (length(cln[cln %like% "adj\\.r\\.squared"]) > 0)
      dt.ind <- dt.ind[, !(cln[cln %like% "adj\\.r\\.squared"]), with = FALSE]
    
    # check consistence
    
    ind.s <- dt.ind[, signal]
    
    if (length(ind.s) != length(ind.s %in% nm.s)) {
      
      stop("Some individual shifts signals lack in the observed chemical shifts data")
      
    }
    
  }

  # extract core particle name ------- #
  
  cr.name <- dt.nm[, particle][1]
  
  # melt chemical shifts data -------- #

  dt.nm <- melt(dt.nm, id.vars = c("data", "signal", "particle")
                , variable.factor = FALSE, variable.name = "solution", value.name = "chem.shift")
  
  cln <- colnames(dt.nm)
  cln <- cln[(cln %in% c("solution", "chem.shift"))]
  
  dt.nm[, eval(cln) := lapply(.SD, str_replace, "\\,", "."), .SDcols = cln]
  dt.nm[, eval(cln) := lapply(.SD, as.numeric), .SDcols = cln]
  
  dt.nm <- dt.nm[order(signal, particle, solution)]
  
  # cast chemical shifts data.table in chemical shifts matrix and error tables
  
  dt.nm <- dcast.data.table(dt.nm, signal + particle + solution ~ data, fun.aggregate = sum, na.rm = TRUE)
  dt.nm[, `:=`(particle = NULL)]

  dt.nm[, wght := sum(deviation ^ 2) / ((deviation ^ 2) * .N), signal]
  
  # basis component stoich coefs ------- #
  
  dt.coef <- copy(dt.coef)
  
  cln <- colnames(dt.coef)
  cln <- cln[!(cln %in% c("name"))]
  
  # cleanse
  
  dt.coef[, eval(cln) := lapply(.SD, str_replace, "\\,", "."), .SDcols = cln]
  dt.coef[, eval(cln) := lapply(.SD, as.numeric), .SDcols = cln]
  
  coef.b <- dt.coef[eval(as.name(cr.name)) > 0, .(coef.b = eval(as.name(cr.name)), name)]
  
  coef.b.names <- coef.b[, name]
  coef.b <- coef.b[, coef.b]

  names(coef.b) <- coef.b.names
  
  # basis component total concentrations #
  
  conc.b <- dt.conc.m[, cr.name]
  
  if (length(part.eq) > 0 && which(colnames(dt.conc.m) == cr.name) %in% part.eq) {
    
    conc.b <- rep(conc.b[1], length(conc.b[1]))
    
  }
  
  # reorder dt.ind to correspond concentations, coefficients, coef.b and dt.nm
  
  if (is.data.table(dt.ind)) {
    
    cln <- colnames(dt.ind)
    cln <- cln[!(cln %like% "signal")]
    
    cln <- dt.coef[, name][dt.coef[, name] %in% cln]
    
    tmp <- dt.ind[, .(signal)]
    
    for (cl in cln) {
      tmp[, eval(cl) := dt.ind[, eval(as.name(cl))]]
    }
    
    dt.ind <- tmp
    
    dt.ind <- dt.ind[order(signal)]
    
  }
  
  # convert to numeric matrices -------- #
  
  tbl <- c("dt.nm", "dt.ind")
  
  # remove row names
  
  if (is.data.table(dt.ind)) {
    
    # dt.ind[, signal := NULL]

  } else {
    
    tbl <- tbl[!(tbl %like% "^dt.ind")]
    
    dt.ind <- NULL
    dt.ind.m <- NULL
    
  }
  
  
  #
  
  for (j in tbl) {
    
    f <- eval(as.name(j))
    
    cln <- colnames(f)
    cln <- cln[!(cln %in% c("signal", "solution"))]
    
    # cleanse
    
    f[, eval(cln) := lapply(.SD, str_replace, "\\,", "."), .SDcols = cln]
    f[, eval(cln) := lapply(.SD, as.numeric), .SDcols = cln]
    
    assign(j, f)
    
  }
  
  list("dt.nm" = dt.nm
       , "dt.ind" = dt.ind
       , "signal" = nm.s, "cr.name" = cr.name
       , "coef.b" = coef.b
       , "conc.b" = conc.b)
  
}









