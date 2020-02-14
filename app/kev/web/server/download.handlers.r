# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #



# common download handlers ---------------- #


# dt.coef

download_dt.coef.csv <- function(module = c("eq", "ab", "emf", "nm", "ht")) {
  
  dt.coef.data <- eval(as.name(paste0(module[1], ".dt.coef.data")))
  sep.fun <- eval(as.name(paste0(module[1], ".sep")))
  
  hndlr <- downloadHandler(
    
    filename = function() {
      
      "input_stoichiometric_coefficients.csv"
      
    },
    
    content = function(file) {
      
      if (sep.fun() == ";") {
        write.csv2(dt.coef.data(), file, row.names = FALSE)
      } else if (sep.fun() == ",") {
        write.csv(dt.coef.data(), file, row.names = FALSE)
      } else if (sep.fun() == "tab") {
        write.table(dt.coef.data(), file, row.names = FALSE, sep = "\t")
      }
      
    }
    
  )
  
  return(hndlr)
  
}

download_dt.coef.xlsx <- function(module = c("eq", "ab", "emf", "nm", "ht")) {
  
  dt.coef.data <- eval(as.name(paste0(module[1], ".dt.coef.data")))

  hndlr <- downloadHandler(
    
    filename = function() {
      
      "input_stoichiometric_coefficients.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(dt.coef.data(), file)
      
    }
    
  )
  
  return(hndlr)
  
}


# cnst

download_cnst.csv <- function(module = c("eq", "ab", "emf", "nm", "ht")) {
  
  cnst.data <- eval(as.name(paste0(module[1], ".cnst.data")))
  sep.fun <- eval(as.name(paste0(module[1], ".sep")))
  
  hndlr <- downloadHandler(
    
    filename = function() {
      
      "k_constants_log10.csv"
      
    },
    
    content = function(file) {
      
      if (sep.fun() == ";") {
        write.csv2(cnst.data(), file, row.names = FALSE)
      } else if (sep.fun() == ",") {
        write.csv(cnst.data(), file, row.names = FALSE)
      } else if (sep.fun() == "tab") {
        write.table(cnst.data(), file, row.names = FALSE, sep = "\t")
      }
      
    }
    
  )
  
  return(hndlr)
  
}

download_cnst.xlsx <- function(module = c("eq", "ab", "emf", "nm", "ht")) {
  
  cnst.data <- eval(as.name(paste0(module[1], ".cnst.data")))

  hndlr <- downloadHandler(
    
    filename = function() {
      
      "k_constants_log10.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(cnst.data(), file)
      
    }
    
  )
  
  return(hndlr)
  
}


# dt.conc

download_dt.conc.csv <- function(module = c("eq", "ab", "emf", "nm", "ht")) {
  
  dt.conc.data <- eval(as.name(paste0(module[1], ".dt.conc.data")))
  part.eq.data <- eval(as.name(paste0(module[1], ".part.eq.data")))
  sep.fun <- eval(as.name(paste0(module[1], ".sep")))
  
  hndlr <- downloadHandler(
    
    filename = function() {
      
      "input_concentrations.csv"
      
    },
    
    content = function(file) {
      
      tmp <- dt.conc.data()
      tmp <- rbind(data.table(t(data.table(colnames(tmp)))), tmp, use.names = FALSE)
      
      setnames(tmp, unlist(part.eq.data()))
      
      if (sep.fun() == ";") {
        write.csv2(tmp, file, row.names = FALSE)
      } else if (sep.fun() == ",") {
        write.csv(tmp, file, row.names = FALSE)
      } else if (sep.fun() == "tab") {
        write.table(tmp, file, row.names = FALSE, sep = "\t")
      }
      
    }
    
  )
  
  return(hndlr)
  
}

download_dt.conc.xlsx <- function(module = c("eq", "ab", "emf", "nm", "ht")) {
  
  dt.conc.data <- eval(as.name(paste0(module[1], ".dt.conc.data")))
  part.eq.data <- eval(as.name(paste0(module[1], ".part.eq.data")))

  hndlr <- downloadHandler(
    
    filename = function() {
      
      "input_concentrations.xlsx"
      
    },
    
    content = function(file) {
      
      tmp <- dt.conc.data()
      tmp <- rbind(data.table(t(data.table(colnames(tmp)))), tmp, use.names = FALSE)
      
      setnames(tmp, unlist(part.eq.data()))
      
      write.xlsx(tmp, file)
      
    }
    
  )
  
  return(hndlr)
  
}


# dt.res

download_dt.res.csv <- function(module = c("eq", "ab", "emf", "nm", "ht")) {
  
  dt.res.data <- eval(as.name(paste0(module[1], ".dt.res.data")))
  sep.fun <- eval(as.name(paste0(module[1], ".sep")))
  
  hndlr <- downloadHandler(
    
    filename = function() {
      
      "equilibrium_concentrations.csv"
      
    },
    
    content = function(file) {
      
      if (sep.fun() == ";") {
        write.csv2(dt.res.data(), file, row.names = FALSE)
      } else if (sep.fun() == ",") {
        write.csv(dt.res.data(), file, row.names = FALSE)
      } else if (sep.fun() == "tab") {
        write.table(dt.res.data(), file, row.names = FALSE, sep = "\t")
      }
      
    }
    
  )
  
  return(hndlr)
  
}

download_dt.res.xlsx <- function(module = c("eq", "ab", "emf", "nm", "ht")) {
  
  dt.res.data <- eval(as.name(paste0(module[1], ".dt.res.data")))
  
  hndlr <- downloadHandler(
    # ----
    filename = function() {
      
      "equilibrium_concentrations.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(dt.res.data(), file)
      
    }
    
  )
  
  return(hndlr)
  
}


# cnst.dev

download_cnst.dev.csv <- function(module = c("eq", "ab", "emf", "nm", "ht")) {
  
  cnst.dev.data <- eval(as.name(paste0(module[1], ".cnst.dev.data")))
  sep.fun <- eval(as.name(paste0(module[1], ".sep")))
  
  hndlr <- downloadHandler(
    
    filename = function() {
      
      "constants_evaluated.csv"
      
    },
    
    content = function(file) {
      
      if (sep.fun() == ";") {
        write.csv2(cnst.dev.data(), file, row.names = FALSE)
      } else if (sep.fun() == ",") {
        write.csv(cnst.dev.data(), file, row.names = FALSE)
      } else if (sep.fun() == "tab") {
        write.table(cnst.dev.data(), file, row.names = FALSE, sep = "\t")
      }
      
    }
    
  )
  
  return(hndlr)
  
}

download_cnst.dev.xlsx <- function(module = c("eq", "ab", "emf", "nm", "ht")) {
  
  cnst.dev.data <- eval(as.name(paste0(module[1], ".cnst.dev.data")))
  
  hndlr <- downloadHandler(
    
    filename = function() {
      
      "constants_evaluated.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(cnst.dev.data(), file)
      
    }
    
  )
  
  return(hndlr)
  
}


# cor.m

download_cor.m.csv <- function(module = c("eq", "ab", "emf", "nm", "ht")) {
  
  cor.m.data <- eval(as.name(paste0(module[1], ".cor.m.data")))
  sep.fun <- eval(as.name(paste0(module[1], ".sep")))
  
  hndlr <- downloadHandler(
    
    filename = function() {
      
      "correlation_matrix.csv"
      
    },
    
    content = function(file) {
      
      if (sep.fun() == ";") {
        write.csv2(cor.m.data(), file, row.names = FALSE)
      } else if (sep.fun() == ",") {
        write.csv(cor.m.data(), file, row.names = FALSE)
      } else if (sep.fun() == "tab") {
        write.table(cor.m.data(), file, row.names = FALSE, sep = "\t")
      }
      
    }
    
  )
  
  return(hndlr)
  
}

download_cor.m.xlsx <- function(module = c("eq", "ab", "emf", "nm", "ht")) {
  
  cor.m.data <- eval(as.name(paste0(module[1], ".cor.m.data")))
  
  hndlr <- downloadHandler(
    
    filename = function() {
      
      "correlation_matrix.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(cor.m.data(), file)
      
    }
    
  )
  
  return(hndlr)
  
}


# adj.r.squared

download_adj.r.squared.csv <- function(module = c("eq", "ab", "emf", "nm", "ht")) {
  
  adj.r.squared.data <- eval(as.name(paste0(module[1], ".adj.r.squared.data")))
  sep.fun <- eval(as.name(paste0(module[1], ".sep")))
  
  hndlr <- downloadHandler(
    
    filename = function() {
      
      "adj_r_squared.csv"
      
    },
    
    content = function(file) {
      
      if (sep.fun() == ";") {
        write.csv2(adj.r.squared.data(), file, row.names = FALSE)
      } else if (sep.fun() == ",") {
        write.csv(adj.r.squared.data(), file, row.names = FALSE)
      } else if (sep.fun() == "tab") {
        write.table(adj.r.squared.data(), file, row.names = FALSE, sep = "\t")
      }
      
    }
    
  )
  
  return(hndlr)
  
}

download_adj.r.squared.xlsx <- function(module = c("eq", "ab", "emf", "nm", "ht")) {
  
  adj.r.squared.data <- eval(as.name(paste0(module[1], ".adj.r.squared.data")))
  
  hndlr <- downloadHandler(
    # ----
    filename = function() {
      
      "adj_r_squared.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(adj.r.squared.data(), file)
      
    }
    
  )
  
  return(hndlr)
  
}




