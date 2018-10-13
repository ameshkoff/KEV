# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #




eq.preproc <- function(dt.coef, cnst, dt.conc, part.eq) {
  
  tbl <- c("cnst", "dt.coef", "dt.conc")
  
  # scalars
  
  part.nm <- colnames(dt.coef)
  part.nm <- length(part.nm[part.nm != "name"])
  
  reac.nm <- nrow(dt.coef) + part.nm
  
  cnst <- rbind(rep(0, part.nm), cnst, use.names = FALSE)
  
  # complete coefficients data table
  
  cln <- colnames(dt.coef)
  tmp <- as.data.table(diag(part.nm))
  
  if (length(cln[cln == "name"]) != 0)
    tmp[, name := cln[cln != "name"]]
    
  dt.coef <- rbind(tmp, dt.coef, use.names = FALSE)
  
  setnames(dt.coef, cln)
  
  # matrices
  
  for (j in tbl) {
    
    f <- eval(as.name(j))
    
    cln <- colnames(f)
    cln <- cln[cln != "name"]
    
    for (i in cln) {
      
      # replace commas with points
      f[, eval(i) := str_replace(eval(as.name(i)), "\\,", ".")]

    }
    
    # to numbers
    
    f <- as.matrix(f[, cln, with = FALSE])
    f <- apply(f, 2, as.numeric)
    
    assign(paste0(j, ".m"), f)
    
  }
  
  part.eq <- which(part.eq[1] == "eq")
  
  # create names
  
  cln <- colnames(dt.coef)
  
  if (length(cln[cln == "name"]) == 0) {
    
    dt.coef[, name := ""]
    
    cln <- colnames(dt.coef)
    cln <- cln[cln != "name"]
    
    for (i in cln) {
      
      dt.coef[eval(as.name(i)) > 0, name := paste0(name, " + ", i)]
      dt.coef[eval(as.name(i)) < 0, name := paste0(name, " - ", i)]
      
    }
    
    dt.coef[, name := str_replace(name, "^ *\\+ *", "")]
    dt.coef[, name := str_replace(name, "^ *\\-", "-")]
    dt.coef[, name := paste(name, 1:.N, sep = "_"), name]
    
  }
  
  
  # restore constants
  
  cnst.m <- (10 ^ cnst.m)
  cnst.m <- log(cnst.m)
  
  list("dt.coef" = dt.coef, "dt.coef.m" = dt.coef.m
       , "dt.conc" = dt.conc, "dt.conc.m" = dt.conc.m
       , "cnst.m" = cnst.m
       , "part.eq" = part.eq
       , "reac.nm" = reac.nm
       , "part.nm" = part.nm)

}









