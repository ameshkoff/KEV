# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2020                                                 #
#                                                            #
# ########################################################## #

library(data.table)



ht.save.prepare.data <- function(dt.ttl = list(), dt.dict = data.table()) {
  
  # create setup, input concentrations etc.
  
  # setup
  
  dt.nms <- names(dt.ttl)
  dt.nms <- dt.nms[dt.nms %in% c("cnst.tune", "cmp.tune.input", "calorimeter.type.input", "init.vol.input")]
  
  dt.setup <- lapply(dt.nms, function(cl) { dt.ttl[[cl]] })
  names(dt.setup) <- dt.nms

  dt.setup <- Filter(Negate(is.null), dt.setup)
  
  max.l <- max(sapply(dt.setup, length))
  dt.setup <- lapply(dt.setup, function(x) { x <- c(x, rep(NA, max.l - length(x))) })
  
  dt.setup <- as.data.table(t(as.data.frame(dt.setup)), keep.rownames = TRUE)
  
  dt.setup <- merge(dt.dict[, .(rn = dt, file)], dt.setup, by = "rn")
  dt.setup[, rn := NULL]
  
  dt.setup[, file := str_remove(file, "\\_setup$")]
  
  dt.ttl[["setup"]] <- dt.setup
  
  # input concentrations
  
  part.eq.input <- rep("tot", ncol(dt.ttl$dt.conc.input))
  part.eq.input[dt.ttl$part.eq.input] <- "eq"
  
  dt.ttl$dt.conc.input <- rbind(as.list(part.eq.input)
                                , as.list(colnames(dt.ttl$dt.conc.input))
                                , dt.ttl$dt.conc.input, use.names = FALSE)

  # remove garbage
  
  dt.ttl <- dt.ttl[!(names(dt.ttl) %in% c(dt.nms, "part.eq.input"))]
  
  dt.ttl
    
}



ht.save <- function() {
  
  dt.dict <- fread("dt.dict.csv")
  
  function(dt.ttl = list()
           , path = ""
           , sep = ";"
           , filename = NULL) {
    
    # if (subdir != "")
    #   subdir <- paste0("/", subdir, "/")
    # 
    # dir.create(file.path(paste0("output", subdir)), showWarnings = FALSE)
    
    dir.create(path, showWarnings = FALSE)
    
    dt.ttl <- ht.save.prepare.data(dt.ttl, dt.dict)
    
    browser()
    
    if (sep == ";") {
      
      write.csv2(dt.res, file = paste0("output", subdir, "equilibrium_concentrations.csv"), row.names = FALSE)
      write.csv2(dt.ht.calc, file = paste0("output", subdir, "absorbance.csv"), row.names = FALSE)
      write.csv2(ht.res.abs, file = paste0("output", subdir, "absorbance_st_deviations_absolute.csv"), row.names = FALSE)
      write.csv2(ht.res.rel, file = paste0("output", subdir, "absorbance_st_deviations_relative.csv"), row.names = FALSE)
      write.csv2(cnst.dev, file = paste0("output", subdir, "constants_with_st_deviations.csv"), row.names = FALSE)
      write.csv2(cor.m, file = paste0("output", subdir, "correlation_matrix.csv"), row.names = FALSE)
      write.csv2(mol.coef, file = paste0("output", subdir, "molar_extinction_coefficients.csv"), row.names = FALSE)
      write.table(target, file = paste0("output", subdir, "target.csv"), sep = sep, dec = ",", row.names = FALSE, col.names = FALSE)
      
    } else if (sep == ",") {
      
      write.csv(dt.res, file = paste0("output", subdir, "equilibrium_concentrations.csv"), row.names = FALSE)
      write.csv(dt.ht.calc, file = paste0("output", subdir, "absorbance.csv"), row.names = FALSE)
      write.csv(ht.res.abs, file = paste0("output", subdir, "absorbance_st_deviations_absolute.csv"), row.names = FALSE)
      write.csv(ht.res.rel, file = paste0("output", subdir, "absorbance_st_deviations_relative.csv"), row.names = FALSE)
      write.csv(cnst.dev, file = paste0("output", subdir, "constants_with_st_deviations.csv"), row.names = FALSE)
      write.csv(cor.m, file = paste0("output", subdir, "correlation_matrix.csv"), row.names = FALSE)
      write.csv(mol.coef, file = paste0("output", subdir, "molar_extinction_coefficients.csv"), row.names = FALSE)
      write.table(target, file = paste0("output", subdir, "target.csv"), sep = sep, dec = ".", row.names = FALSE, col.names = FALSE)
      
    } else if (sep == "tab") {
      
      write.table(dt.res, file = paste0("output", subdir, "equilibrium_concentrations.csv"), sep = "\t", row.names = FALSE)
      write.table(dt.ht.calc, file = paste0("output", subdir, "absorbance.csv"), sep = "\t", row.names = FALSE)
      write.table(ht.res.abs, file = paste0("output", subdir, "absorbance_st_deviations_absolute.csv"), sep = "\t", row.names = FALSE)
      write.table(ht.res.rel, file = paste0("output", subdir, "absorbance_st_deviations_relative.csv"), sep = "\t", row.names = FALSE)
      write.table(cnst.dev, file = paste0("output", subdir, "constants_with_st_deviations.csv"), sep = "\t", row.names = FALSE)
      write.table(cor.m, file = paste0("output", subdir, "correlation_matrix.csv"), sep = "\t", row.names = FALSE)
      write.table(mol.coef, file = paste0("output", subdir, "molar_extinction_coefficients.csv"), sep = "\t", row.names = FALSE)
      write.table(target, file = paste0("output", subdir, "target.csv"), sep = "\t", row.names = FALSE, col.names = FALSE)
      
    }
    
  }
  
}

ht.save <- ht.save()


  
