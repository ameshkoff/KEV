# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #




sp.preproc <- function(dt.sp) {
  
  for (i in names(dt.sp)) {
    
    # use iterator by names instead of iterating data.tables itself to save them back to the list
    dt <- dt.sp[[i]]
    
    # wavelengths and absorbance to numeric
    
    cln <- colnames(dt)

    for (j in cln) {
      
      dt[, eval(j) := str_replace_all(eval(as.name(j)), "\\,", ".")]
      dt[, eval(j) := str_replace_all(eval(as.name(j)), " ", "")]
      
      dt[, eval(j) := as.numeric(eval(as.name(j)))]
      
    }

    # transpose
    
    cln <- c("concentration", dt[, wavelength])

    dt <- data.table(t(dt[, !c("wavelength"), with = FALSE]), keep.rownames = TRUE)
    setnames(dt, cln)
    
    # concentration to numeric
    
    dt[, concentration := str_replace_all(concentration, "\\,", ".")]
    dt[, concentration := str_replace_all(concentration, " ", "")]
    
    dt[, concentration := as.numeric(concentration)]
    # browser()
    dt.sp[[i]] <- dt
    
  }

  dt.sp
  
}









