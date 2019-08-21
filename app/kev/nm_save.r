# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #



nm.save <- function(subdir, sep, dt.res, dt.nm.calc, nm.res.abs, nm.res.rel, nm.err
                    , cnst.dev, cor.m, ind.shift, ind.shift.dev, err.diff, target) {
  
  if (subdir != "")
    subdir <- paste0("/", subdir, "/")
  
  dir.create(file.path(paste0("output", subdir)), showWarnings = FALSE)
  
  if (sep == ";") {
    
    write.csv2(dt.res, file = paste0("output", subdir, "equilibrium_concentrations.csv"), row.names = FALSE)
    write.csv2(dt.nm.calc, file = paste0("output", subdir, "chemical_shifts.csv"), row.names = FALSE)
    write.csv2(nm.res.abs, file = paste0("output", subdir, "chemical_shifts_st_deviations_absolute.csv"), row.names = FALSE)
    write.csv2(nm.res.rel, file = paste0("output", subdir, "chemical_shifts_st_deviations_relative.csv"), row.names = FALSE)
    write.csv2(cnst.dev, file = paste0("output", subdir, "constants_with_st_deviations.csv"), row.names = FALSE)
    write.csv2(cor.m, file = paste0("output", subdir, "correlation_matrix.csv"), row.names = FALSE)
    write.csv2(ind.shift, file = paste0("output", subdir, "individual_shifts.csv"), row.names = FALSE)
    write.table(target, file = paste0("output", subdir, "target.csv"), sep = sep, dec = ",", row.names = FALSE, col.names = FALSE)
    
  } else if (sep == ",") {
    
    write.csv(dt.res, file = paste0("output", subdir, "equilibrium_concentrations.csv"), row.names = FALSE)
    write.csv(dt.nm.calc, file = paste0("output", subdir, "chemical_shifts.csv"), row.names = FALSE)
    write.csv(nm.res.abs, file = paste0("output", subdir, "chemical_shifts_st_deviations_absolute.csv"), row.names = FALSE)
    write.csv(nm.res.rel, file = paste0("output", subdir, "chemical_shifts_st_deviations_relative.csv"), row.names = FALSE)
    write.csv(cnst.dev, file = paste0("output", subdir, "constants_with_st_deviations.csv"), row.names = FALSE)
    write.csv(cor.m, file = paste0("output", subdir, "correlation_matrix.csv"), row.names = FALSE)
    write.csv(ind.shift, file = paste0("output", subdir, "individual_shifts.csv"), row.names = FALSE)
    write.table(target, file = paste0("output", subdir, "target.csv"), sep = sep, dec = ".", row.names = FALSE, col.names = FALSE)
    
  } else if (sep == "tab") {
    
    write.table(dt.res, file = paste0("output", subdir, "equilibrium_concentrations.csv"), sep = "\t", row.names = FALSE)
    write.table(dt.nm.calc, file = paste0("output", subdir, "chemical_shifts.csv"), sep = "\t", row.names = FALSE)
    write.table(nm.res.abs, file = paste0("output", subdir, "chemical_shifts_st_deviations_absolute.csv"), sep = "\t", row.names = FALSE)
    write.table(nm.res.rel, file = paste0("output", subdir, "chemical_shifts_st_deviations_relative.csv"), sep = "\t", row.names = FALSE)
    write.table(cnst.dev, file = paste0("output", subdir, "constants_with_st_deviations.csv"), sep = "\t", row.names = FALSE)
    write.table(cor.m, file = paste0("output", subdir, "correlation_matrix.csv"), sep = "\t", row.names = FALSE)
    write.table(ind.shift, file = paste0("output", subdir, "individual_shifts.csv"), sep = "\t", row.names = FALSE)
    write.table(target, file = paste0("output", subdir, "target.csv"), sep = "\t", row.names = FALSE, col.names = FALSE)
    
  }
  
}
