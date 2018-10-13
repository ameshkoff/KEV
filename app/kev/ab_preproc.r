# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #




ab.preproc <- function(dt.ab, dt.mol) {
  
  # scalars
  
  partprod.nm <- ncol(dt.ab) + nrow(dt.ab)

  # split absorbance data.table in absorbance matrix and error tables
  
  cln <- 1:ncol(dt.ab)
  cln.val <- cln[cln %% 2 == 1]
  cln.err <- cln[cln %% 2 == 0]
  
  dt.ab.err <- dt.ab[, cln.err, with = FALSE]
  dt.ab <- dt.ab[, cln.val, with = FALSE]
  
  cln <- paste0("V", 1:ncol(dt.ab))
  
  setnames(dt.ab, cln)
  setnames(dt.ab.err, cln)
  
  tbl <- c("dt.ab", "dt.ab.err", "dt.mol")
  
  # transpose known molar coefficients
  
  if (is.data.table(dt.mol)) {
    
    cln <- unlist(dt.mol[, 1, with = FALSE])
    
    dt.mol <- data.table(t(dt.mol[, !1, with = FALSE]))
    setnames(dt.mol, cln)
    
  } else {
    
    tbl <- tbl[tbl != "dt.mol"]
    dt.mol <- NULL
    dt.mol.m <- NULL
    
  }
  
  # convert to numeric matrices

  for (j in tbl) {
    
    f <- eval(as.name(j))
    
    cln <- colnames(f)
    
    for (i in cln) {
      
      # replace commas with points
      f[, eval(i) := str_replace(eval(as.name(i)), "\\,", ".")]
      
    }
    
    # to numbers
    
    f <- as.matrix(f)
    f <- apply(f, 2, as.numeric)
    
    assign(paste0(j, ".m"), f)
    
  }
  
  list("dt.ab" = dt.ab, "dt.ab.m" = dt.ab.m
       , "dt.ab.err" = dt.ab.err, "dt.ab.err.m" = dt.ab.err.m
       , "dt.mol" = dt.mol, "dt.mol.m" = dt.mol.m
       , "partprod.nm" = partprod.nm)
  
}









