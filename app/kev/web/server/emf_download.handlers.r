# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #



# emf download ---------------- #

output$emf.dt.coef.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "input_stoichiometric_coefficients.csv"
    
  },
  
  content = function(file) {
    
    if (emf.sep() == ";") {
      write.csv2(emf.dt.coef.data(), file, row.names = FALSE)
    } else {
      write.csv(emf.dt.coef.data(), file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$emf.dt.coef.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "input_stoichiometric_coefficients.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(emf.dt.coef.data(), file)
    
  }
  
)
# ----

output$emf.cnst.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "input_k_constants_log10.csv"
    
  },
  
  content = function(file) {
    
    if (emf.sep() == ";") {
      write.csv2(emf.cnst.data(), file, row.names = FALSE)
    } else {
      write.csv(emf.cnst.data(), file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$emf.cnst.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "input_k_constants_log10.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(emf.cnst.data(), file)
    
  }
  
)
# ----

output$emf.dt.conc.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "input_concentrations.csv"
    
  },
  
  content = function(file) {
    
    tmp <- emf.dt.conc.data()
    tmp <- rbind(data.table(t(data.table(colnames(tmp)))), tmp, use.names = FALSE)
    
    setnames(tmp, unlist(emf.part.eq.data()))
    
    if (emf.sep() == ";") {
      write.csv2(tmp, file, row.names = FALSE)
    } else {
      write.csv(tmp, file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$emf.dt.conc.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "input_concentrations.xlsx"
    
  },
  
  content = function(file) {
    
    tmp <- emf.dt.conc.data()
    tmp <- rbind(data.table(t(data.table(colnames(tmp)))), tmp, use.names = FALSE)
    
    setnames(tmp, unlist(emf.part.eq.data()))
    
    write.xlsx(tmp, file)
    
  }
  
)
# ----

output$dt.emf.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "input_emf.csv"
    
  },
  
  content = function(file) {
    
    if (emf.sep() == ";") {
      write.csv2(dt.emf.data(), file, row.names = FALSE)
    } else {
      write.csv(dt.emf.data(), file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$dt.emf.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "input_emf.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(dt.emf.data(), file)
    
  }
  
)
# ----

output$emf.target.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "targets.csv"
    
  },
  
  content = function(file) {
    
    if (emf.sep() == ";") {
      write.csv2(emf.target.data(), file, row.names = FALSE)
    } else {
      write.csv(emf.target.data(), file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$emf.target.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "targets.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(emf.target.data(), file)
    
  }
  
)
# ----

output$emf.dt.res.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "equilibrium_concentrations.csv"
    
  },
  
  content = function(file) {
    
    if (emf.sep() == ";") {
      write.csv2(emf.dt.res.data(), file, row.names = FALSE)
    } else {
      write.csv(emf.dt.res.data(), file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$emf.dt.res.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "equilibrium_concentrations.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(emf.dt.res.data(), file)
    
  }
  
)
# ----

output$dt.emf.abs.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "emf_calculated_abs_errors.csv"
    
  },
  
  content = function(file) {
    
    if (emf.sep() == ";") {
      write.csv2(dt.emf.abs.data(), file, row.names = FALSE)
    } else {
      write.csv(dt.emf.abs.data(), file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$dt.emf.abs.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "emf_calculated_abs_errors.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(dt.emf.abs.data(), file)
    
  }
  
)
# ----

output$dt.emf.rel.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "emf_calculated_rel_errors.csv"
    
  },
  
  content = function(file) {
    
    if (emf.sep() == ";") {
      write.csv2(dt.emf.rel.data(), file, row.names = FALSE)
    } else {
      write.csv(dt.emf.rel.data(), file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$dt.emf.rel.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "emf_calculated_rel_errors.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(dt.emf.rel.data(), file)
    
  }
  
)
# ----

output$emf.cnst.dev.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "constants_evaluated.csv"
    
  },
  
  content = function(file) {
    
    if (emf.sep() == ";") {
      write.csv2(emf.cnst.dev.data(), file, row.names = FALSE)
    } else {
      write.csv(emf.cnst.dev.data(), file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$emf.cnst.dev.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "constants_evaluated.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(emf.cnst.dev.data(), file)
    
  }
  
)
# ----

output$emf.cor.m.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "correlation_matrix.csv"
    
  },
  
  content = function(file) {
    
    if (emf.sep() == ";") {
      write.csv2(emf.cor.m.data(), file, row.names = FALSE)
    } else {
      write.csv(emf.cor.m.data(), file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$emf.cor.m.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "correlation_matrix.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(emf.cor.m.data(), file)
    
  }
  
)
# ----

output$emf.adj.r.squared.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "adj_r_squared.csv"
    
  },
  
  content = function(file) {
    
    if (emf.sep() == ";") {
      write.csv2(emf.adj.r.squared.data(), file, row.names = FALSE)
    } else {
      write.csv(emf.adj.r.squared.data(), file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$emf.adj.r.squared.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "adj_r_squared.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(emf.adj.r.squared.data(), file)
    
  }
  
)
# ----

output$kev.emf.data.zip <- downloadHandler(
  # ----
  filename = function() {
    
    "kev.emf.constants.data.zip"
    
  },
  
  content = function(file) {
    
    data.files <- c(
      
      emf.dt.coef = "input_stoichiometric_coefficients.csv"
      , emf.cnst = "input_k_constants_log10.csv"
      , emf.dt.conc = "input_concentrations.csv"
      , dt.emf = "input_emf.csv"
      , emf.dt.res = "equilibrium_concentrations.csv"
      , dt.emf.abs = "emf_calculated_abs_errors.csv"
      , dt.emf.rel = "emf_calculated_rel_errors.csv"
      , emf.cnst.dev = "constants_evaluated.csv"
      , emf.cor.m = "correlation_matrix.csv"
      , emf.adj.r.squared = "adj_r_squared.csv"
      , emf.target = "target.csv"
      
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
      
      if (emf.sep() == ";") {
        
        if (!is.null(dt)) {
          
          if (data.files[i] == "input_concentrations.csv") {
            
            dt <- emf.dt.conc.data()
            dt <- rbind(data.table(t(data.table(colnames(dt)))), dt, use.names = FALSE)
            
            setnames(dt, unlist(emf.part.eq.data()))
            
          }
          
          write.csv2(dt, data.files[i], row.names = FALSE)
          
        } else {
          
          data.files <- data.files[-i]
          
        }
        
      } else {
        
        if (!is.null(dt)) {
          
          if (data.files[i] == "input_concentrations.csv") {
            
            dt <- emf.dt.conc.data()
            dt <- rbind(data.table(t(data.table(colnames(dt)))), dt, use.names = FALSE)
            
            setnames(dt, unlist(emf.part.eq.data()))
            
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

output$kev.emf.data.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "kev.emf.constants.data.xlsx"
    
  },
  
  content = function(file) {
    
    data.files <- c(
      
      emf.dt.coef = "input_stoich_coefficients"
      , emf.cnst = "input_k_constants_log10"
      , emf.dt.conc = "input_concentrations"
      , dt.emf = "input_emf"
      , emf.target = "targets"
      , emf.dt.res = "equilibrium_concentrations"
      , dt.emf.abs = "emf_calc_abs_errors"
      , dt.emf.rel = "emf_calc_rel_errors"
      , emf.cnst.dev = "constants_evaluated"
      , emf.cor.m = "correlation_matrix"
      , emf.adj.r.squared = "adj_r_squared"
      
    )
    
    dt.list <- list()
    
    for (i in 1:length(data.files)) {
      
      # check if all files are present (in case run before evaluation)
      
      dt <- NULL
      try(dt <- eval(expr = parse(text = paste0(names(data.files)[i], ".data()"))), silent = TRUE)
      
      if (!is.null(dt)) {
        
        if (data.files[i] == "input_concentrations") {
          
          dt <- emf.dt.conc.data()
          dt <- rbind(data.table(t(data.table(colnames(dt)))), dt, use.names = FALSE)
          
          setnames(dt, unlist(emf.part.eq.data()))
          
        }
        
        dt.list[[eval(data.files[i])]] <- dt
        
      }
      
    }
    
    write.xlsx(dt.list, file)
    
  }
  
)
# ----


