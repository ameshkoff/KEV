# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #



ab.save <- function(subdir, sep, dt.res, dt.ab.calc, ab.res.abs, ab.res.rel, ab.err, nst.dev, cor.m, mol.coef, mol.coef.dev, err.diff) {
  
  if (subdir != "")
    subdir <- paste0("/", subdir, "/")
  
  dir.create(file.path(paste0("output", subdir)), showWarnings = FALSE)
  
  if (sep == ";") {
    
    write.csv2(dt.res, file = paste0("output", subdir, "equilibrium_concentrations.csv"))
    write.csv2(dt.frac, file = paste0("output", subdir, bs.name, "_fractions.csv"))
    write.csv2(dt.err, file = paste0("output", subdir, "percent_error.csv"))
    
  } else if (sep == ",") {
    
    write.csv(dt.res, file = paste0("output", subdir, "equilibrium_concentrations.csv"))
    write.csv(dt.frac, file = paste0("output", subdir, bs.name, "_fractions.csv"))
    write.csv(dt.err, file = paste0("output", subdir, "percent_error.csv"))
    
  } else if (sep == "tab") {
    
    write.table(dt.res, file = paste0("output", subdir, "equilibrium_concentrations.txt"), sep = "\t")
    write.table(dt.frac, file = paste0("output", subdir, bs.name, "_fractions.txt"), sep = "\t")
    write.table(dt.err, file = paste0("output", subdir, "percent_error.txt"), sep = "\t")
    
  }
  
}
