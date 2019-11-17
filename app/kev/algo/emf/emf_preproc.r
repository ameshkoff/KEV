# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #




emf.preproc <- function(dt.emf, dt.params) {
  
  tbl <- c("dt.emf", "dt.emf.err", "dt.params")
  
  # transpose emf data
  
  cln <- dt.emf[, paste0(data, "_", particle)]
  
  dt.emf <- data.table(t(dt.emf[, !c("data", "particle"), with = FALSE]))
  setnames(dt.emf, cln)
  
  # split emf data.table in emf matrix and error tables
  
  cln <- colnames(dt.emf)
  cln.val <- cln[cln %like% "^obs"]
  cln.err <- cln[cln %like% "^(dev|err)"]
  
  dt.emf.err <- dt.emf[, cln.err, with = FALSE]
  dt.emf <- dt.emf[, cln.val, with = FALSE]
  
  cln <- colnames(dt.emf)
  cln <- str_replace(cln, "obs(ervation)*\\_", "")
  # cln <- paste0("EMF", cln)
  
  setnames(dt.emf, cln)
  setnames(dt.emf.err, cln)
  
  # remove row names
  
  if (is.data.table(dt.params)) {
    
    # unify params names from scripts and GUI
    dt.params <- dt.params[, 1:2, with = FALSE]
    setnames(dt.params, c("param", "value"))

    cln <- dt.params[, param]
    dt.params <- as.data.table(t(dt.params)[2, , drop = FALSE])
    
    setnames(dt.params, cln)
        
    # # remove standard errors (if output of the previously calculations loaded)
    # cln <- colnames(dt.params)
    # if (length(cln[cln %like% "adj\\.r\\.squared"]) > 0)
    #   dt.params <- dt.params[, !(cln[cln %like% "adj\\.r\\.squared"]), with = FALSE]

  } else {
    
    tbl <- tbl[tbl != "dt.params"]
    
    dt.params <- NULL
    dt.params.m <- NULL
    
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
    class(f) <- "numeric"
    
    assign(paste0(j, ".m"), f)
    
  }
  
  list("dt.emf" = dt.emf, "dt.emf.m" = dt.emf.m
       , "dt.emf.err" = dt.emf.err, "dt.emf.err.m" = dt.emf.err.m
       , "dt.params" = dt.params, "dt.params.m" = dt.params.m)
  
}









