# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #



sp.save <- function(subdir, sep, dt.mol.full) {
  
  if (subdir != "")
    subdir <- paste0("/", subdir, "/")
  
  dir.create(file.path(paste0("output", subdir)), showWarnings = FALSE)
  
  if (sep == ";") {
    
    write.csv2(dt.mol.full, file = paste0("output", subdir, "molar_extinction_coefficients.csv"), row.names = FALSE)

  } else if (sep == ",") {
    
    write.csv(dt.mol.full, file = paste0("output", subdir, "molar_extinction_coefficients.csv"), row.names = FALSE)

  } else if (sep == "tab") {
    
    write.table(dt.mol.full, file = paste0("output", subdir, "molar_extinction_coefficients.txt"), sep = "\t", row.names = FALSE)

  }
  
}
