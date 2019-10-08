# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #



# equilibrium download ---------------- #

output$eq.dt.coef.csv <- download_dt.coef.csv("eq")
output$eq.dt.coef.xlsx <- download_dt.coef.xlsx("eq")

output$eq.cnst.csv <- download_cnst.csv("eq")
output$eq.cnst.xlsx <- download_cnst.xlsx("eq")

output$eq.dt.conc.csv <- download_dt.conc.csv("eq")
output$eq.dt.conc.xlsx <- download_dt.conc.xlsx("eq")

output$eq.dt.conc.tot.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "total_concentrations.csv"
    
  },
  
  content = function(file) {
    
    tmp <- try(eq.dt.conc.tot.data())
    
    if (!is.data.frame(tmp))
      tmp <- data.frame(error = "Evaluate before downloading total concentrations")
    
    if (eq.sep() == ";") {
      write.csv2(tmp, file, row.names = FALSE)
    } else {
      write.csv(tmp, file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$eq.dt.conc.tot.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "total_concentrations.xlsx"
    
  },
  
  content = function(file) {
    
    tmp <- try(eq.dt.conc.tot.data())
    
    if (!is.data.frame(tmp))
      tmp <- data.frame(error = "Evaluate before downloading total concentrations")
    
    write.xlsx(tmp, file)
    
  }
  
)
# ----

output$eq.dt.res.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "equilibrium_concentrations.csv"
    
  },
  
  content = function(file) {
    
    if (eq.sep() == ";") {
      write.csv2(eq.dt.res.data(), file, row.names = FALSE)
    } else {
      write.csv(eq.dt.res.data(), file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$eq.dt.res.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "equilibrium_concentrations.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(eq.dt.res.data(), file)
    
  }
  
)
# ----

output$dt.frac.csv <- downloadHandler(
  # ----
  filename = function() {
    
    paste0(input$bs.name, "_fractions.csv")
    
  },
  
  content = function(file) {
    
    if (eq.sep() == ";") {
      write.csv2(dt.frac.data(), file, row.names = FALSE)
    } else {
      write.csv(dt.frac.data(), file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$dt.frac.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    paste0(input$bs.name, "_fractions.xlsx")
    
  },
  
  content = function(file) {
    
    write.xlsx(dt.frac.data(), file)
    
  }
  
)
# ----

output$dt.err.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "percent_error.csv"
    
  },
  
  content = function(file) {
    
    if (eq.sep() == ";") {
      write.csv2(dt.err.data(), file, row.names = FALSE)
    } else {
      write.csv(dt.err.data(), file, row.names = FALSE)
    }
    
  }
  
)
# ----

output$dt.err.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "percent_error.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(dt.err.data(), file)
    
  }
  
)
# ----

output$kev.eq.data.zip <- downloadHandler(
  # ----
  filename = function() {
    
    "kev.concentrations.data.zip"
    
  },
  
  content = function(file) {
    
    data.files <- c(
      
      eq.dt.coef = "input_stoichiometric_coefficients.csv"
      , eq.cnst = "input_k_constants_log10.csv"
      , eq.dt.conc = "input_concentrations.csv"
      , eq.dt.conc.tot = "total_concentrations.csv"
      , eq.dt.res = "equilibrium_concentrations.csv"
      , dt.frac = paste0(bs.name.data(), "_fractions.csv")
      , dt.err = "percent_error.csv"
      , bs.name = "component_names.csv"
      
      
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
      
      if (eq.sep() == ";") {
        
        if (!is.null(dt)) {
          
          if (data.files[i] == "input_concentrations.csv") {
            
            dt <- eq.dt.conc.data()
            dt <- rbind(data.table(t(data.table(colnames(dt)))), dt, use.names = FALSE)
            
            setnames(dt, unlist(eq.part.eq.data()))
            
          }
          
          if ((data.files[i] %like% "(particle|component)_names(\\.csv|\\.txt)$")) {
            
            write.table(dt, data.files[i], sep = ";", dec = ",", row.names = FALSE, col.names = FALSE)
            
          } else {
            
            write.csv2(dt, data.files[i], row.names = FALSE)
            
          }
          
        } else {
          
          data.files <- data.files[-i]
          
        }
        
      } else {
        
        if (!is.null(dt)) {
          
          if (data.files[i] == "input_concentrations.csv") {
            
            dt <- eq.dt.conc.data()
            dt <- rbind(data.table(t(data.table(colnames(dt)))), dt, use.names = FALSE)
            
            setnames(dt, unlist(eq.part.eq.data()))
            
          }
          
          if ((data.files[i] %like% "(particle|component)_names(\\.csv|\\.txt)$")) {
            
            write.table(dt, data.files[i], sep = ",", dec = ".", row.names = FALSE, col.names = FALSE)
            
          } else {
            
            write.csv(dt, data.files[i], row.names = FALSE)
            
          }
          
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

output$kev.eq.data.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "kev.concentrations.data.xlsx"
    
  },
  
  content = function(file) {
    
    data.files <- c(
      
      eq.dt.coef = "input_stoich_coefficients"
      , eq.cnst = "input_k_constants_log10"
      , eq.dt.conc = "input_concentrations"
      , eq.dt.conc.tot = "total_concentrations"
      , eq.dt.res = "equilibrium_concentrations"
      , dt.frac = paste0(bs.name.data(), "_fractions")
      , dt.err = "percent_error"
      , bs.name = "component_names"
      
    )
    
    dt.list <- list()
    
    for (i in 1:length(data.files)) {
      
      # check if all files are present (in case run before evaluation)
      
      dt <- NULL
      try(dt <- eval(expr = parse(text = paste0(names(data.files)[i], ".data()"))), silent = TRUE)
      
      if (!is.null(dt)) {
        
        if (data.files[i] == "input_concentrations") {
          
          dt <- eq.dt.conc.data()
          dt <- rbind(data.table(t(data.table(colnames(dt)))), dt, use.names = FALSE)
          
          setnames(dt, unlist(eq.part.eq.data()))
          
        }
        
        dt.list[[eval(data.files[i])]] <- dt
        
      }
      
    }
    
    write.xlsx(dt.list, file)
    
  }
  
)
# ----

