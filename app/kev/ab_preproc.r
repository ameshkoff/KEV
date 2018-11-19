# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #




ab.preproc <- function(dt.ab, dt.mol, wl.tune = NULL) {
  
  # backward compatibility
  
  cln <- colnames(dt.ab)
  if (length(cln[cln == "wave.length"]) > 0)
    setnames(dt.ab, "wave.length", "wavelength")
  
  cln <- colnames(dt.mol)
  if (length(cln[cln == "wave.length"]) > 0)
    setnames(dt.mol, "wave.length", "wavelength")
  
  # remove standard errors (if output of the previously calculations loaded)
  
  cln <- colnames(dt.mol)
  if (length(cln[cln %like% "adj\\.r\\.squared"]) > 0)
    dt.mol <- dt.mol[, !(cln[cln %like% "adj\\.r\\.squared"]), with = FALSE]
  
  # check consistence

  ab.w <- dt.ab[data %like% "^obs", wavelength]
  mol.w <- dt.mol[, wavelength]

  if (length(mol.w) != length(mol.w %in% ab.w)) {
    
    stop("Absorbance data is inconsistent with molar extinction coefficients")
    
  }
  
  # wavelenghts consistence again
  
  if (is.character(dt.ab[, wavelength])){
    
    dt.ab[, wavelength := str_replace(wavelength, "\\,", ".")]
    dt.ab[, wavelength := str_replace(wavelength, " ", "")]

  }

  if (is.character(dt.mol[, wavelength])){
    
    dt.mol[, wavelength := str_replace(wavelength, "\\,", ".")]
    dt.mol[, wavelength := str_replace(wavelength, " ", "")]
    
  }
  
  # remove uncalculatable absorbance
  
  dt.ab <- dt.ab[wavelength %in% mol.w]
  ab.w <- dt.ab[data %like% "^obs", wavelength]
  
  # transpose absorbance data
  
  cln <- dt.ab[, paste0(data, "_", wavelength)]
  
  dt.ab <- data.table(t(dt.ab[, !c("data", "wavelength"), with = FALSE]))
  setnames(dt.ab, cln)

  # scalars
  
  partprod.nm <- ncol(dt.ab) + nrow(dt.ab)

  # split absorbance data.table in absorbance matrix and error tables
  
  cln <- colnames(dt.ab)
  cln.val <- cln[cln %like% "^obs"]
  cln.err <- cln[cln %like% "^(dev|err)"]
  
  dt.ab.err <- dt.ab[, cln.err, with = FALSE]
  dt.ab <- dt.ab[, cln.val, with = FALSE]
  
  cln <- colnames(dt.ab)
  cln <- str_replace(cln, "obs(ervation)*", "")
  cln <- paste0("L", cln)
  
  setnames(dt.ab, cln)
  setnames(dt.ab.err, cln)
  
  # split absorbance into full and work data
  
  dt.ab.full <- copy(dt.ab)
  dt.ab.err.full <- copy(dt.ab.err)
  
  if (!is.null(wl.tune)) {
    
    cln <- colnames(dt.ab)
    cln <- cln[cln %in% paste0("L_", wl.tune)]
    
    dt.ab <- dt.ab[, cln, with = FALSE]
    dt.ab.err <- dt.ab.err[, cln, with = FALSE]
    
  }

  # remove row names
  
  if (is.data.table(dt.mol)) {
    
    dt.mol.full <- copy(dt.mol)
    
    if (!is.null(wl.tune)) {
      
      if (is.character(dt.mol[, wavelength])){
        
        dt.mol[, wavelength := str_replace(wavelength, "\\,", ".")]
        dt.mol[, wavelength := str_replace(wavelength, " ", "")]
        
      }
      
      dt.mol <- dt.mol[wavelength %in% wl.tune]
      
    }

    dt.mol[, wavelength := NULL]
    dt.mol.full[, wavelength := NULL]
    
  } else {
    
    tbl <- tbl[tbl != "dt.mol"]
    
    dt.mol <- NULL
    dt.mol.m <- NULL
    
    dt.mol.full <- NULL
    dt.mol.full.m <- NULL
    
  }
  
  # convert to numeric matrices

  tbl <- c("dt.ab", "dt.ab.err", "dt.ab.full", "dt.ab.err.full", "dt.mol", "dt.mol.full")
  
  for (j in tbl) {
    
    f <- eval(as.name(j))
    
    if (j %in% c("dt.ab.full", "dt.ab.err.full"))
      f <- as.data.table(t(f), keep.rownames = FALSE)
    
    cln <- colnames(f)
    
    for (i in cln) {
      
      # replace commas with points
      f[, eval(i) := str_replace(eval(as.name(i)), "\\,", ".")]
      
    }
    
    if (j %in% c("dt.ab.full", "dt.ab.err.full"))
      f <- as.data.table(t(f), keep.rownames = FALSE)
    
    # to numbers
    
    f <- as.matrix(f)
    # f <- apply(f, 2, as.numeric)
    class(f) <- "numeric"
    
    assign(paste0(j, ".m"), f)
    
  }
  
  list("dt.ab" = dt.ab, "dt.ab.m" = dt.ab.m
       , "dt.ab.err" = dt.ab.err, "dt.ab.err.m" = dt.ab.err.m
       , "dt.ab.full" = dt.ab.full, "dt.ab.full.m" = dt.ab.full.m
       , "dt.ab.err.full" = dt.ab.err.full, "dt.ab.err.full.m" = dt.ab.err.full.m
       , "dt.mol" = dt.mol, "dt.mol.m" = dt.mol.m
       , "dt.mol.full" = dt.mol.full, "dt.mol.full.m" = dt.mol.full.m
       , "partprod.nm" = partprod.nm
       , "wavelength" = ab.w)
  
}









