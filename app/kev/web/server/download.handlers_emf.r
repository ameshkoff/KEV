# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #



# emf download ---------------- #

output$emf.dt.coef.csv <- download_dt.coef.csv("emf")
output$emf.dt.coef.xlsx <- download_dt.coef.xlsx("emf")

output$emf.cnst.csv <- download_cnst.csv("emf")
output$emf.cnst.xlsx <- download_cnst.xlsx("emf")

output$emf.dt.conc.csv <- download_dt.conc.csv("emf")
output$emf.dt.conc.xlsx <- download_dt.conc.xlsx("emf")

output$dt.emf.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "input_emf.csv"
    
  },
  
  content = function(file) {
    
    if (emf.sep() == ";") {
      write.csv2(dt.emf.data(), file, row.names = FALSE)
    } else if (emf.sep() == ",") {
      write.csv(dt.emf.data(), file, row.names = FALSE)
    } else if (emf.sep() == "tab") {
      write.table(dt.emf.data(), file, row.names = FALSE, sep = "\t")
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
    } else if (emf.sep() == ",") {
      write.csv(emf.target.data(), file, row.names = FALSE)
    } else if (emf.sep() == "tab") {
      write.table(emf.target.data(), file, row.names = FALSE, sep = "\t")
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

output$emf.dt.res.csv <- download_dt.res.csv("emf")
output$emf.dt.res.xlsx <- download_dt.res.xlsx("emf")

output$dt.emf.abs.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "emf_calculated_abs_errors.csv"
    
  },
  
  content = function(file) {
    
    if (emf.sep() == ";") {
      write.csv2(dt.emf.abs.data(), file, row.names = FALSE)
    } else if (emf.sep() == ",") {
      write.csv(dt.emf.abs.data(), file, row.names = FALSE)
    } else if (emf.sep() == "tab") {
      write.table(dt.emf.abs.data(), file, row.names = FALSE, sep = "\t")
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
    } else if (emf.sep() == ",") {
      write.csv(dt.emf.rel.data(), file, row.names = FALSE)
    } else if (emf.sep() == "tab") {
      write.table(dt.emf.rel.data(), file, row.names = FALSE, sep = "\t")
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

output$emf.cnst.dev.csv <- download_cnst.dev.csv("emf")
output$emf.cnst.dev.xlsx <- download_cnst.dev.xlsx("emf")

output$emf.cor.m.csv <- download_cor.m.csv("emf")
output$emf.cor.m.xlsx <- download_cor.m.xlsx("emf")

output$emf.adj.r.squared.csv <- download_adj.r.squared.csv("emf")
output$emf.adj.r.squared.xlsx <- download_adj.r.squared.xlsx("emf")

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
      
      if (!is.null(dt)) {
        
        if (data.files[i] == "input_concentrations.csv") {
          
          dt <- emf.dt.conc.data()
          dt <- rbind(data.table(t(data.table(colnames(dt)))), dt, use.names = FALSE)
          
          setnames(dt, unlist(emf.part.eq.data()))
          
        }
        
        if (emf.sep() == ";") {
          write.csv2(dt, data.files[i], row.names = FALSE)
        } else if (emf.sep() == ",") {
          write.csv(dt, data.files[i], row.names = FALSE)
        } else if (emf.sep() == "tab") {
          write.table(dt, data.files[i], row.names = FALSE, sep = "\t")
        }

      } else {
        
        data.files <- data.files[-i]
        
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


