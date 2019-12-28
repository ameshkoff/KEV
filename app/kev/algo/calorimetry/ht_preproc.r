# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2020                                                 #
#                                                            #
# ########################################################## #




ht.preproc <- function(dt.heat, dt.enth, cmp.tune = NULL) {
  
  # backward compatibility
  
  cln <- colnames(dt.heat)
  if (length(cln[cln == "wave.length"]) > 0)
    setnames(dt.heat, "wave.length", "wavelength")
  
  if (is.data.table(dt.enth)) {
    
    cln <- colnames(dt.enth)
    if (length(cln[cln == "wave.length"]) > 0)
      setnames(dt.enth, "wave.length", "wavelength")
    
    # remove standard errors (if output of the previously calculations loaded)
    
    cln <- colnames(dt.enth)
    if (length(cln[cln %like% "adj\\.r\\.squared"]) > 0)
      dt.enth <- dt.enth[, !(cln[cln %like% "adj\\.r\\.squared"]), with = FALSE]
    
    # wavelengths consistence again
    # after first check : to preserve wavelength vector to keep consistence for second use of the dataset
    
    if (is.character(dt.heat[, wavelength])){
      
      dt.heat[, wavelength := str_replace(wavelength, "\\,", ".")]
      dt.heat[, wavelength := str_replace(wavelength, " ", "")]
      
    }
    
    if (is.character(dt.enth[, wavelength])){
      
      dt.enth[, wavelength := str_replace(wavelength, "\\,", ".")]
      dt.enth[, wavelength := str_replace(wavelength, " ", "")]
      
    }
    
    # check consistence
    
    ht.w <- dt.heat[data %like% "^obs", wavelength]
    mol.w <- dt.enth[, wavelength]
    
    if (length(mol.w) != length(mol.w %in% ht.w)) {
      
      stop("Absorbance data is inconsistent with molar extinction coefficients")
      
    }
    
    # remove uncalculatable absorbance
    
    dt.heat <- dt.heat[wavelength %in% mol.w]
    
  } else {
    
    # after first check : to preserve wavelength vector to keep consistence for second use of the dataset
    
    if (is.character(dt.heat[, wavelength])){
      
      dt.heat[, wavelength := str_replace(wavelength, "\\,", ".")]
      dt.heat[, wavelength := str_replace(wavelength, " ", "")]
      
    }
    
  }
  
  
  ht.w <- dt.heat[data %like% "^obs", wavelength]
  
  # transpose absorbance data
  
  cln <- dt.heat[, paste0(data, "_", wavelength)]
  
  dt.heat <- data.table(t(dt.heat[, !c("data", "wavelength"), with = FALSE]))
  setnames(dt.heat, cln)
  
  # scalars
  
  partprod.nm <- ncol(dt.heat) + nrow(dt.heat)
  
  # split absorbance data.table in absorbance matrix and error tables
  
  cln <- colnames(dt.heat)
  cln.val <- cln[cln %like% "^obs"]
  cln.err <- cln[cln %like% "^(dev|err)"]
  
  dt.ht.err <- dt.heat[, cln.err, with = FALSE]
  dt.heat <- dt.heat[, cln.val, with = FALSE]
  
  cln <- colnames(dt.heat)
  cln <- str_replace(cln, "obs(ervation)*", "")
  cln <- paste0("L", cln)
  
  setnames(dt.heat, cln)
  setnames(dt.ht.err, cln)
  
  # split absorbance into full and work data
  
  dt.ht.full <- copy(dt.heat)
  dt.ht.err.full <- copy(dt.ht.err)
  
  if (!is.null(cmp.tune)) {
    
    cln <- colnames(dt.heat)
    cln <- cln[cln %in% paste0("L_", cmp.tune)]
    
    dt.heat <- dt.heat[, cln, with = FALSE]
    dt.ht.err <- dt.ht.err[, cln, with = FALSE]
    
  }
  
  # convert to numeric matrices
  
  tbl <- c("dt.heat", "dt.ht.err", "dt.ht.full", "dt.ht.err.full", "dt.enth", "dt.enth.full")
  
  # remove row names
  
  if (is.data.table(dt.enth)) {
    
    dt.enth.full <- copy(dt.enth)
    
    if (!is.null(cmp.tune)) {
      
      if (is.character(dt.enth[, wavelength])){
        
        dt.enth[, wavelength := str_replace(wavelength, "\\,", ".")]
        dt.enth[, wavelength := str_replace(wavelength, " ", "")]
        
      }
      
      dt.enth <- dt.enth[wavelength %in% cmp.tune]
      
    }
    
    dt.enth[, wavelength := NULL]
    dt.enth.full[, wavelength := NULL]
    
  } else {
    
    tbl <- tbl[!(tbl %like% "^dt.enth")]
    
    dt.enth <- NULL
    dt.enth.m <- NULL
    
    dt.enth.full <- NULL
    dt.enth.full.m <- NULL
    
  }
  
  
  #
  
  for (j in tbl) {
    
    f <- eval(as.name(j))
    
    if (j %in% c("dt.ht.full", "dt.ht.err.full"))
      f <- as.data.table(t(f), keep.rownames = FALSE)
    
    cln <- colnames(f)
    
    for (i in cln) {
      
      # replace commas with points
      f[, eval(i) := str_replace(eval(as.name(i)), "\\,", ".")]
      
    }
    
    if (j %in% c("dt.ht.full", "dt.ht.err.full"))
      f <- as.data.table(t(f), keep.rownames = FALSE)
    
    # to numbers
    
    f <- as.matrix(f)
    class(f) <- "numeric"
    
    assign(paste0(j, ".m"), f)
    
  }
  
  list("dt.heat" = dt.heat, "dt.ht.m" = dt.ht.m
       , "dt.ht.err" = dt.ht.err, "dt.ht.err.m" = dt.ht.err.m
       , "dt.ht.full" = dt.ht.full, "dt.ht.full.m" = dt.ht.full.m
       , "dt.ht.err.full" = dt.ht.err.full, "dt.ht.err.full.m" = dt.ht.err.full.m
       , "dt.enth" = dt.enth, "dt.enth.m" = dt.enth.m
       , "dt.enth.full" = dt.enth.full, "dt.enth.full.m" = dt.enth.full.m
       , "partprod.nm" = partprod.nm
       , "wavelength" = ht.w)
  
}









