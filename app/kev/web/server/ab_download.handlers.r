# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #



# absorbance download ---------------- #

output$ab.dt.coef.csv <- download_dt.coef.csv("ab")
output$ab.dt.coef.xlsx <- download_dt.coef.xlsx("ab")

output$ab.cnst.csv <- download_cnst.csv("ab")
output$ab.cnst.xlsx <- download_cnst.xlsx("ab")

output$ab.dt.conc.csv <- download_dt.conc.csv("ab")
output$ab.dt.conc.xlsx <- download_dt.conc.xlsx("ab")

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

output$ab.dt.res.csv <- download_dt.res.csv("ab")
output$ab.dt.res.xlsx <- download_dt.res.xlsx("ab")

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

output$ab.cnst.dev.csv <- download_cnst.dev.csv("ab")
output$ab.cnst.dev.xlsx <- download_cnst.dev.xlsx("ab")

output$ab.cor.m.csv <- download_cor.m.csv("ab")
output$ab.cor.m.xlsx <- download_cor.m.xlsx("ab")

output$ab.adj.r.squared.csv <- download_adj.r.squared.csv("ab")
output$ab.adj.r.squared.xlsx <- download_adj.r.squared.xlsx("ab")

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
      , ab.cnst.dev = "constants_evaluated.csv"
      , ab.cor.m = "correlation_matrix.csv"
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
      , ab.cnst.dev = "constants_evaluated"
      , ab.cor.m = "correlation_matrix"
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

