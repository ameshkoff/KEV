# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #



ab.save <- function(subdir, sep, dt.res, dt.ab.calc, ab.res.abs, ab.res.rel, ab.err
                    , cnst.dev, cor.m, mol.coef, mol.coef.dev, err.diff, target) {
  
  if (subdir != "")
    subdir <- paste0("/", subdir, "/")
  
  dir.create(file.path(paste0("output", subdir)), showWarnings = FALSE)
  
  if (sep == ";") {
    
    write.csv2(dt.res, file = paste0("output", subdir, "equilibrium_concentrations.csv"), row.names = FALSE)
    write.csv2(dt.ab.calc, file = paste0("output", subdir, "absorbance.csv"), row.names = FALSE)
    write.csv2(ab.res.abs, file = paste0("output", subdir, "absorbance_st_deviations_absolute.csv"), row.names = FALSE)
    write.csv2(ab.res.rel, file = paste0("output", subdir, "absorbance_st_deviations_relative.csv"), row.names = FALSE)
    write.csv2(cnst.dev, file = paste0("output", subdir, "constants_with_st_deviations.csv"), row.names = FALSE)
    write.csv2(cor.m, file = paste0("output", subdir, "correlation_matrix.csv"), row.names = FALSE)
    write.csv2(mol.coef, file = paste0("output", subdir, "molar_extinction_coefficients.csv"), row.names = FALSE)
    write.table(target, file = paste0("output", subdir, "target.csv"), sep = sep, dec = ",", row.names = FALSE, col.names = FALSE)

  } else if (sep == ",") {
    
    write.csv(dt.res, file = paste0("output", subdir, "equilibrium_concentrations.csv"), row.names = FALSE)
    write.csv(dt.ab.calc, file = paste0("output", subdir, "absorbance.csv"), row.names = FALSE)
    write.csv(ab.res.abs, file = paste0("output", subdir, "absorbance_st_deviations_absolute.csv"), row.names = FALSE)
    write.csv(ab.res.rel, file = paste0("output", subdir, "absorbance_st_deviations_relative.csv"), row.names = FALSE)
    write.csv(cnst.dev, file = paste0("output", subdir, "constants_with_st_deviations.csv"), row.names = FALSE)
    write.csv(cor.m, file = paste0("output", subdir, "correlation_matrix.csv"), row.names = FALSE)
    write.csv(mol.coef, file = paste0("output", subdir, "molar_extinction_coefficients.csv"), row.names = FALSE)
    write.table(target, file = paste0("output", subdir, "target.csv"), sep = sep, dec = ".", row.names = FALSE, col.names = FALSE)
    
  } else if (sep == "tab") {
    
    write.table(dt.res, file = paste0("output", subdir, "equilibrium_concentrations.csv"), sep = "\t", row.names = FALSE)
    write.table(dt.ab.calc, file = paste0("output", subdir, "absorbance.csv"), sep = "\t", row.names = FALSE)
    write.table(ab.res.abs, file = paste0("output", subdir, "absorbance_st_deviations_absolute.csv"), sep = "\t", row.names = FALSE)
    write.table(ab.res.rel, file = paste0("output", subdir, "absorbance_st_deviations_relative.csv"), sep = "\t", row.names = FALSE)
    write.table(cnst.dev, file = paste0("output", subdir, "constants_with_st_deviations.csv"), sep = "\t", row.names = FALSE)
    write.table(cor.m, file = paste0("output", subdir, "correlation_matrix.csv"), sep = "\t", row.names = FALSE)
    write.table(mol.coef, file = paste0("output", subdir, "molar_extinction_coefficients.csv"), sep = "\t", row.names = FALSE)
    write.table(target, file = paste0("output", subdir, "target.csv"), sep = "\t", row.names = FALSE, col.names = NA)
    
  }
  
}
