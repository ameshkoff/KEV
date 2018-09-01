# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #



eq.save <- function(subdir, sep, dt.res, dt.frac, dt.err, bs.name) {
  
  if (subdir != "")
    subdir <- paste0("/", subdir, "/")
  
  dir.create(file.path(paste0("output", subdir)), showWarnings = FALSE)
  
  if (sep == ";") {
    
    write.csv2(dt.res, file = paste0("output", subdir, "equilibrium_concentrations.csv"))
    write.csv2(dt.frac, file = paste0("output", subdir, bs.name, "_fractions.csv"))
    write.csv2(dt.err, file = paste0("output", subdir, "percent_error.csv"))
    
  } else {
    
    write.csv(dt.res, file = paste0("output", subdir, "equilibrium_concentrations.csv"))
    write.csv(dt.frac, file = paste0("output", subdir, bs.name, "_fractions.csv"))
    write.csv(dt.err, file = paste0("output", subdir, "percent_error.csv"))
    
  }
  
}
