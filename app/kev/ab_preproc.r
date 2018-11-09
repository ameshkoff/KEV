# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #




ab.preproc <- function(dt.ab, dt.mol) {
  
  # check consistence
  
  ab.w <- dt.ab[data %like% "^obs", wave.length]
  mol.w <- dt.mol[, wave.length]

  if (length(mol.w) != length(mol.w %in% ab.w)) {
    
    stop("Absorbance data is inconsistent with molar extinction coefficients")
    
  }
  
  # transpose absorbance data
  
  cln <- dt.ab[, paste0(data, "_", wave.length)]
  
  dt.ab <- data.table(t(dt.ab[, !c("data", "wave.length"), with = FALSE]))
  setnames(dt.ab, cln)

  # scalars
  
  partprod.nm <- ncol(dt.ab) + nrow(dt.ab)

  # split absorbance data.table in absorbance matrix and error tables
  
  cln <- colnames(dt.ab)
  cln.val <- cln[cln %like% "^obs"]
  cln.err <- cln[cln %like% "^dev"]
  
  dt.ab.err <- dt.ab[, cln.err, with = FALSE]
  dt.ab <- dt.ab[, cln.val, with = FALSE]
  
  cln <- colnames(dt.ab)
  cln <- str_replace(cln, "obs(ervation)*", "")
  cln <- paste0("L", cln)
  
  setnames(dt.ab, cln)
  setnames(dt.ab.err, cln)
  
  tbl <- c("dt.ab", "dt.ab.err", "dt.mol")
  
  # transpose known molar coefficients
  
  if (is.data.table(dt.mol)) {
    
    dt.mol[, wave.length := NULL]
    
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
       , "partprod.nm" = partprod.nm
       , "wave.length" = ab.w)
  
}









