# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #




nm.preproc <- function(dt.nm, dt.ind) {

  # extract signal names
  
  nm.s <- dt.nm[data %like% "^obs", signal]
  
  # if some individual shifts are provided
  
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

  # extract core particle name
  
  cr.name <- dt.nm[, particle][1]
  
  # melt chemical shifts data

  dt.nm <- melt(dt.nm, id.vars = c("data", "signal", "particle"), variable.name = "solution", value.name = "chem.shift")
  dt.nm <- dt.nm[order(signal, particle, solution)]
  
  # split chemical shifts data.table in chemical shifts matrix and error tables
  
  dt.nm.err <- dt.nm[data %like% "^dev"]
  dt.nm <- dt.nm[data %like% "^obs"]
  
  dt.nm.err[, `:=`(data = NULL, particle = NULL)]
  dt.nm[, `:=`(data = NULL, particle = NULL)]
  
  setnames(dt.nm.err, "chem.shift", "chem.shift.err")
  
  # explode individual shifts
  
  dt.ind <- merge(dt.nm[, !c("chem.shift"), with = FALSE], dt.ind, by = "signal")
  
  # convert to numeric matrices
  
  tbl <- c("dt.nm", "dt.nm.err", "dt.ind")
  
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
    
    for (i in cln) {
      
      # replace commas with points
      f[, eval(i) := str_replace(eval(as.name(i)), "\\,", ".")]
      
    }
    
    # to numbers
    
    f <- as.matrix(f[, cln, with = FALSE])
    class(f) <- "numeric"
    
    assign(paste0(j, ".m"), f)
    
  }
  
  list("dt.nm" = dt.nm, "dt.nm.m" = dt.nm.m
       , "dt.nm.err" = dt.nm.err, "dt.nm.err.m" = dt.nm.err.m
       , "dt.ind" = dt.ind, "dt.ind.m" = dt.ind.m
       , "signal" = nm.s, "cr.name" = cr.name)
  
}









