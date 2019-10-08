# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #



# common download handlers ---------------- #


# dt.coef

download_dt.coef.csv <- function(module = c("eq", "ab", "emf", "nm")) {
  
  dt.coef.data <- eval(as.name(paste0(module[1], ".dt.coef.data")))
  sep.fun <- eval(as.name(paste0(module[1], ".sep")))
  
  hndlr <- downloadHandler(
    
    filename = function() {
      
      "input_stoichiometric_coefficients.csv"
      
    },
    
    content = function(file) {
      
      if (sep.fun() == ";") {
        write.csv2(dt.coef.data(), file, row.names = FALSE)
      } else {
        write.csv(dt.coef.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  
  return(hndlr)
  
}

download_dt.coef.xlsx <- function(module = c("eq", "ab", "emf", "nm")) {
  
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

download_cnst.csv <- function(module = c("eq", "ab", "emf", "nm")) {
  
  cnst.data <- eval(as.name(paste0(module[1], ".cnst.data")))
  sep.fun <- eval(as.name(paste0(module[1], ".sep")))
  
  hndlr <- downloadHandler(
    
    filename = function() {
      
      "k_constants_log10.csv"
      
    },
    
    content = function(file) {
      
      if (sep.fun() == ";") {
        write.csv2(cnst.data(), file, row.names = FALSE)
      } else {
        write.csv(cnst.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  
  return(hndlr)
  
}

download_cnst.xlsx <- function(module = c("eq", "ab", "emf", "nm")) {
  
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

download_dt.conc.csv <- function(module = c("eq", "ab", "emf", "nm")) {
  
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
      } else {
        write.csv(tmp, file, row.names = FALSE)
      }
      
    }
    
  )
  
  return(hndlr)
  
}

download_dt.conc.xlsx <- function(module = c("eq", "ab", "emf", "nm")) {
  
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



