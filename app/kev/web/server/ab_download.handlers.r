# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #



# absorbance download ---------------- #

output$ab.dt.coef.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "input_stoichiometric_coefficients.csv"
    
  },
  
  content = function(file) {
    
    if (ab.sep() == ";") {
      write.csv2(ab.dt.coef.data(), file, row.names = FALSE)
    } else {
      write.csv(ab.dt.coef.data(), file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$ab.dt.coef.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "input_stoichiometric_coefficients.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(ab.dt.coef.data(), file)
    
  }
  
)
# ----

output$ab.cnst.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "input_k_constants_log10.csv"
    
  },
  
  content = function(file) {
    
    if (ab.sep() == ";") {
      write.csv2(ab.cnst.data(), file, row.names = FALSE)
    } else {
      write.csv(ab.cnst.data(), file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$ab.cnst.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "input_k_constants_log10.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(ab.cnst.data(), file)
    
  }
  
)
# ----

output$ab.dt.conc.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "input_concentrations.csv"
    
  },
  
  content = function(file) {
    
    tmp <- ab.dt.conc.data()
    tmp <- rbind(data.table(t(data.table(colnames(tmp)))), tmp, use.names = FALSE)
    
    setnames(tmp, unlist(ab.part.eq.data()))
    
    if (ab.sep() == ";") {
      write.csv2(tmp, file, row.names = FALSE)
    } else {
      write.csv(tmp, file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$ab.dt.conc.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "input_concentrations.xlsx"
    
  },
  
  content = function(file) {
    
    tmp <- ab.dt.conc.data()
    tmp <- rbind(data.table(t(data.table(colnames(tmp)))), tmp, use.names = FALSE)
    
    setnames(tmp, unlist(ab.part.eq.data()))
    
    write.xlsx(tmp, file)
    
  }
  
)
# ----

output$dt.ab.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "input_absorbance.csv"
    
  },
  
  content = function(file) {
    
    if (ab.sep() == ";") {
      write.csv2(dt.ab.data(), file, row.names = FALSE)
    } else {
      write.csv(dt.ab.data(), file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$dt.ab.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "input_absorbance.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(dt.ab.data(), file)
    
  }
  
)
# ----

output$dt.mol.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "input_mol_ext_coefficients.csv"
    
  },
  
  content = function(file) {
    
    if (ab.sep() == ";") {
      write.csv2(dt.mol.data(), file, row.names = FALSE)
    } else {
      write.csv(dt.mol.data(), file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$dt.mol.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "input_mol_ext_coefficients.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(dt.mol.data(), file)
    
  }
  
)
# ----

output$ab.dt.res.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "equilibrium_concentrations.csv"
    
  },
  
  content = function(file) {
    
    if (ab.sep() == ";") {
      write.csv2(ab.dt.res.data(), file, row.names = FALSE)
    } else {
      write.csv(ab.dt.res.data(), file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$ab.dt.res.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "equilibrium_concentrations.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(ab.dt.res.data(), file)
    
  }
  
)
# ----

output$dt.ab.abs.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "absorbance_calculated_abs_errors.csv"
    
  },
  
  content = function(file) {
    
    if (ab.sep() == ";") {
      write.csv2(dt.ab.abs.data(), file, row.names = FALSE)
    } else {
      write.csv(dt.ab.abs.data(), file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$dt.ab.abs.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "absorbance_calculated_abs_errors.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(dt.ab.abs.data(), file)
    
  }
  
)
# ----

output$dt.ab.rel.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "absorbance_calculated_rel_errors.csv"
    
  },
  
  content = function(file) {
    
    if (ab.sep() == ";") {
      write.csv2(dt.ab.rel.data(), file, row.names = FALSE)
    } else {
      write.csv(dt.ab.rel.data(), file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$dt.ab.rel.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "absorbance_calculated_rel_errors.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(dt.ab.rel.data(), file)
    
  }
  
)
# ----

output$cnst.dev.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "constants_evaluated.csv"
    
  },
  
  content = function(file) {
    
    if (ab.sep() == ";") {
      write.csv2(cnst.dev.data(), file, row.names = FALSE)
    } else {
      write.csv(cnst.dev.data(), file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$cnst.dev.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "constants_evaluated.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(cnst.dev.data(), file)
    
  }
  
)
# ----

output$cor.m.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "correlation_matrix.csv"
    
  },
  
  content = function(file) {
    
    if (ab.sep() == ";") {
      write.csv2(cor.m.data(), file, row.names = FALSE)
    } else {
      write.csv(cor.m.data(), file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$cor.m.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "correlation_matrix.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(cor.m.data(), file)
    
  }
  
)
# ----

output$ab.adj.r.squared.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "adj_r_squared.csv"
    
  },
  
  content = function(file) {
    
    if (ab.sep() == ";") {
      write.csv2(ab.adj.r.squared.data(), file, row.names = FALSE)
    } else {
      write.csv(ab.adj.r.squared.data(), file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$ab.adj.r.squared.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "adj_r_squared.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(ab.adj.r.squared.data(), file)
    
  }
  
)
# ----

output$mol.coef.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "mol_ext_coefficients_calculated.csv"
    
  },
  
  content = function(file) {
    
    if (ab.sep() == ";") {
      write.csv2(mol.coef.data(), file, row.names = FALSE)
    } else {
      write.csv(mol.coef.data(), file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$mol.coef.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "mol_ext_coefficients_calculated.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(mol.coef.data(), file)
    
  }
  
)
# ----

output$kev.data.zip <- downloadHandler(
  # ----
  filename = function() {
    
    "kev.constants.data.zip"
    
  },
  
  content = function(file) {
    
    data.files <- c(
      
      ab.dt.coef = "input_stoichiometric_coefficients.csv"
      , ab.cnst = "input_k_constants_log10.csv"
      , ab.dt.conc = "input_concentrations.csv"
      , dt.ab = "input_absorbance.csv"
      , dt.mol = "input_mol_ext_coefficients.csv"
      , ab.dt.res = "equilibrium_concentrations.csv"
      , dt.ab.abs = "absorbance_calculated_abs_errors.csv"
      , dt.ab.rel = "absorbance_calculated_rel_errors.csv"
      , cnst.dev = "constants_evaluated.csv"
      , cor.m = "correlation_matrix.csv"
      , ab.adj.r.squared = "adj_r_squared.csv"
      , mol.coef = "mol_ext_coefficients_calculated.csv"
      , target = "target.csv"
      
    )
    
    # temporary directory to avoid permission issues
    
    curdir <- getwd()
    tmpdir <- tempdir()
    setwd(tmpdir)
    print(tempdir())
    
    
    for (i in length(data.files):1) {
      
      # check if all files are present (in case run before evaluation)
      
      dt <- NULL
      try(dt <- eval(expr = parse(text = paste0(names(data.files)[i], ".data()"))), silent = TRUE)
      
      if (ab.sep() == ";") {
        
        if (!is.null(dt)) {
          
          if (data.files[i] == "input_concentrations.csv") {
            
            dt <- ab.dt.conc.data()
            dt <- rbind(data.table(t(data.table(colnames(dt)))), dt, use.names = FALSE)
            
            setnames(dt, unlist(ab.part.eq.data()))
            
          }
          
          write.csv2(dt, data.files[i], row.names = FALSE)
          
        } else {
          
          data.files <- data.files[-i]
          
        }
        
      } else {
        
        if (!is.null(dt)) {
          
          if (data.files[i] == "input_concentrations.csv") {
            
            dt <- ab.dt.conc.data()
            dt <- rbind(data.table(t(data.table(colnames(dt)))), dt, use.names = FALSE)
            
            setnames(dt, unlist(ab.part.eq.data()))
            
          }
          
          write.csv(dt, data.files[i], row.names = FALSE)
          
        } else {
          
          data.files <- data.files[-i]
          
        }
      }
      
    }
    
    # create zip
    
    utils::zip(file, data.files)
    
    # remove garbage from the disc
    
    for (i in data.files) {
      
      if (file.exists(i))
        file.remove(i)
      
    }
    
    setwd(curdir)
    
  }
  
)
# ----

output$kev.data.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "kev.constants.data.xlsx"
    
  },
  
  content = function(file) {
    
    data.files <- c(
      
      ab.dt.coef = "input_stoich_coefficients"
      , ab.cnst = "input_k_constants_log10"
      , ab.dt.conc = "input_concentrations"
      , dt.ab = "input_absorbance"
      , dt.mol = "input_mol_ext_coefficients"
      , target = "target"
      , ab.dt.res = "equilibrium_concentrations"
      , dt.ab.abs = "absorbance_calc_abs_errors"
      , dt.ab.rel = "absorbance_calc_rel_errors"
      , cnst.dev = "constants_evaluated"
      , cor.m = "correlation_matrix"
      , ab.adj.r.squared = "adj_r_squared"
      , mol.coef = "mol_ext_coefficients_calc"
      
    )
    
    dt.list <- list()
    
    for (i in 1:length(data.files)) {
      
      # check if all files are present (in case run before evaluation)
      
      dt <- NULL
      try(dt <- eval(expr = parse(text = paste0(names(data.files)[i], ".data()"))), silent = TRUE)
      
      if (!is.null(dt)) {
        
        if (data.files[i] == "input_concentrations") {
          
          dt <- ab.dt.conc.data()
          dt <- rbind(data.table(t(data.table(colnames(dt)))), dt, use.names = FALSE)
          
          setnames(dt, unlist(ab.part.eq.data()))
          
        }
        
        dt.list[[eval(data.files[i])]] <- dt
        
      }
      
    }
    
    write.xlsx(dt.list, file)
    
  }
  
)
# ----

