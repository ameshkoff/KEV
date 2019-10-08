# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #



# nmr (fast) download ---------------- #

output$nm.dt.coef.csv <- download_dt.coef.csv("nm")
output$nm.dt.coef.xlsx <- download_dt.coef.xlsx("nm")

output$nm.cnst.csv <- download_cnst.csv("nm")
output$nm.cnst.xlsx <- download_cnst.xlsx("nm")

output$nm.dt.conc.csv <- download_dt.conc.csv("nm")
output$nm.dt.conc.xlsx <- download_dt.conc.xlsx("nm")

output$dt.nm.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "input_chemical_shifts.csv"
    
  },
  
  content = function(file) {
    
    if (nm.sep() == ";") {
      write.csv2(dt.nm.data(), file, row.names = FALSE)
    } else {
      write.csv(dt.nm.data(), file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$dt.nm.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "input_chemical_shifts.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(dt.nm.data(), file)
    
  }
  
)
# ----

output$nm.dt.ind.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "input_individual_shifts.csv"
    
  },
  
  content = function(file) {
    
    if (nm.sep() == ";") {
      write.csv2(nm.dt.ind.data(), file, row.names = FALSE)
    } else {
      write.csv(nm.dt.ind.data(), file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$nm.dt.ind.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "input_individual_shifts.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(nm.dt.ind.data(), file)
    
  }
  
)
# ----

output$nm.dt.res.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "equilibrium_concentrations.csv"
    
  },
  
  content = function(file) {
    
    if (nm.sep() == ";") {
      write.csv2(nm.dt.res.data(), file, row.names = FALSE)
    } else {
      write.csv(nm.dt.res.data(), file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$nm.dt.res.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "equilibrium_concentrations.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(nm.dt.res.data(), file)
    
  }
  
)
# ----

output$dt.nm.abs.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "chemical_shifts_calculated_abs_errors.csv"
    
  },
  
  content = function(file) {
    
    if (nm.sep() == ";") {
      write.csv2(dt.nm.abs.data(), file, row.names = FALSE)
    } else {
      write.csv(dt.nm.abs.data(), file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$dt.nm.abs.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "chemical_shifts_calculated_abs_errors.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(dt.nm.abs.data(), file)
    
  }
  
)
# ----

output$dt.nm.rel.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "chemical_shifts_calculated_rel_errors.csv"
    
  },
  
  content = function(file) {
    
    if (nm.sep() == ";") {
      write.csv2(dt.nm.rel.data(), file, row.names = FALSE)
    } else {
      write.csv(dt.nm.rel.data(), file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$dt.nm.rel.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "chemical_shifts_calculated_rel_errors.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(dt.nm.rel.data(), file)
    
  }
  
)
# ----

output$nm.cnst.dev.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "constants_evaluated.csv"
    
  },
  
  content = function(file) {
    
    if (nm.sep() == ";") {
      write.csv2(nm.cnst.dev.data(), file, row.names = FALSE)
    } else {
      write.csv(nm.cnst.dev.data(), file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$nm.cnst.dev.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "constants_evaluated.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(nm.cnst.dev.data(), file)
    
  }
  
)
# ----

output$nm.cor.m.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "correlation_matrix.csv"
    
  },
  
  content = function(file) {
    
    if (nm.sep() == ";") {
      write.csv2(nm.cor.m.data(), file, row.names = FALSE)
    } else {
      write.csv(nm.cor.m.data(), file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$nm.cor.m.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "correlation_matrix.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(nm.cor.m.data(), file)
    
  }
  
)
# ----

output$nm.adj.r.squared.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "adj_r_squared.csv"
    
  },
  
  content = function(file) {
    
    if (nm.sep() == ";") {
      write.csv2(nm.adj.r.squared.data(), file, row.names = FALSE)
    } else {
      write.csv(nm.adj.r.squared.data(), file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$nm.adj.r.squared.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "adj_r_squared.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(nm.adj.r.squared.data(), file)
    
  }
  
)
# ----

output$nm.ind.shift.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "individual_shifts_calculated.csv"
    
  },
  
  content = function(file) {
    
    if (nm.sep() == ";") {
      write.csv2(nm.ind.shift.data(), file, row.names = FALSE)
    } else {
      write.csv(nm.ind.shift.data(), file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$nm.ind.shift.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "individual_shifts_calculated.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(nm.ind.shift.data(), file)
    
  }
  
)
# ----

output$kev.nm.data.zip <- downloadHandler(
  # ----
  filename = function() {
    
    "kev.nmr.constants.data.zip"
    
  },
  
  content = function(file) {
    
    data.files <- c(
      
      nm.dt.coef = "input_stoichiometric_coefficients.csv"
      , nm.cnst = "input_k_constants_log10.csv"
      , nm.dt.conc = "input_concentrations.csv"
      , dt.nm = "input_chemical_shifts.csv"
      , nm.dt.ind = "input_individual_shifts.csv"
      , nm.dt.res = "equilibrium_concentrations.csv"
      , dt.nm.abs = "chemical_shifts_calculated_abs_errors.csv"
      , dt.nm.rel = "chemical_shifts_calculated_rel_errors.csv"
      , nm.cnst.dev = "constants_evaluated.csv"
      , nm.cor.m = "correlation_matrix.csv"
      , nm.adj.r.squared = "adj_r_squared.csv"
      , nm.ind.shift = "individual_shifts_calculated.csv"
      , nm.target = "target.csv"
      
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
      
      if (nm.sep() == ";") {
        
        if (!is.null(dt)) {
          
          if (data.files[i] == "input_concentrations.csv") {
            
            dt <- nm.dt.conc.data()
            dt <- rbind(data.table(t(data.table(colnames(dt)))), dt, use.names = FALSE)
            
            setnames(dt, unlist(nm.part.eq.data()))
            
          }
          
          write.csv2(dt, data.files[i], row.names = FALSE)
          
        } else {
          
          data.files <- data.files[-i]
          
        }
        
      } else {
        
        if (!is.null(dt)) {
          
          if (data.files[i] == "input_concentrations.csv") {
            
            dt <- nm.dt.conc.data()
            dt <- rbind(data.table(t(data.table(colnames(dt)))), dt, use.names = FALSE)
            
            setnames(dt, unlist(nm.part.eq.data()))
            
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

output$kev.nm.data.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "kev.nmr.constants.data.xlsx"
    
  },
  
  content = function(file) {
    
    data.files <- c(
      
      nm.dt.coef = "input_stoich_coefficients"
      , nm.cnst = "input_k_constants_log10"
      , nm.dt.conc = "input_concentrations"
      , dt.nm = "input_chemical_shifts"
      , nm.dt.ind = "input_individual_shifts"
      , nm.target = "target"
      , nm.dt.res = "equilibrium_concentrations"
      , dt.nm.abs = "chemical_shifts_calc_abs_err"
      , dt.nm.rel = "chemical_shifts_calc_rel_err"
      , nm.cnst.dev = "constants_evaluated"
      , nm.cor.m = "correlation_matrix"
      , nm.adj.r.squared = "adj_r_squared"
      , nm.ind.shift = "individual_shifts_calc"
      
    )
    
    dt.list <- list()
    
    for (i in 1:length(data.files)) {
      
      # check if all files are present (in case run before evaluation)
      
      dt <- NULL
      try(dt <- eval(expr = parse(text = paste0(names(data.files)[i], ".data()"))), silent = TRUE)
      
      if (!is.null(dt)) {
        
        if (data.files[i] == "input_concentrations") {
          
          dt <- nm.dt.conc.data()
          dt <- rbind(data.table(t(data.table(colnames(dt)))), dt, use.names = FALSE)
          
          setnames(dt, unlist(nm.part.eq.data()))
          
        }
        
        dt.list[[eval(data.files[i])]] <- dt
        
      }
      
    }
    
    write.xlsx(dt.list, file)
    
  }
  
)
# ----
