# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #



# data ------------------------------

# input data

server_dt.coef.data <- function(module = c("eq", "ab", "emf", "nm")) {
  
  dt.coef.name <- paste0(module[1], ".dt.coef")
  
  dt.coef.reac <- reactive({

    if (!is.null(input[[dt.coef.name]])) {
      
      dt.coef <- hot_to_r(input[[dt.coef.name]])
      
    } else {
      
      if (is.null(values[[dt.coef.name]])) {
        
        dt.coef <- as.data.table(matrix(rep(1, 16), 4))
        setnames(dt.coef, paste0("molecule", 1:4))
        
        if (module != "eq") dt.coef <- cbind(dt.coef, name = paste0("product", 1:4))
        
      } else {
        
        dt.coef <- values[[dt.coef.name]]
        
      }
      
    }
    
    dt.coef <- as.data.table(dt.coef)
    
    if (module == "eq") {
    
      setnames(dt.coef, eval(as.name(paste0(module, ".part.names.data")))()[1:ncol(dt.coef)])
      
    } else {
      
      setnames(dt.coef, c(eval(as.name(paste0(module, ".part.names.data")))()[1:(ncol(dt.coef) - 1)], "name"))
      
    }
    
    
    values[[dt.coef.name]] <- dt.coef
    
    dt.coef
    
  })
  
  # return
  
  return(dt.coef.reac)
  
}

server_part.names.data <- function(module = c("eq")) {
  
  part.names.name <- paste0(module[1], ".part.names")
  
  part.names.reac <- reactive({
    
    if (!is.null(input[[part.names.name]])) {
      
      part.names <- input[[part.names.name]]
      part.names <- str_split(part.names, "\\, *")
      part.names <- unlist(part.names)
      
    } else {
      
      if (is.null(values[[part.names.name]])) {
        
        part.names <- "molecule1"
        
      } else {
        
        part.names <- values[[part.names.name]]
        
      }
      
    }
    
    part.names <- str_trim(part.names)
    values[[part.names.name]] <- part.names
    
    part.names
    
  })
  
  return(part.names.reac)
  
}

server_dt.conc.data <- function(module = c("eq", "ab", "emf", "nm")) {
  
  dt.conc.name <- paste0(module[1], ".dt.conc")
  
  dt.conc.reac <- reactive({
    
    if (!is.null(input[[dt.conc.name]])) {
      
      dt.conc <- hot_to_r(input[[dt.conc.name]])
      
    } else {
      
      if (is.null(values[[dt.conc.name]])) {
        
        dt.conc <- as.data.table(matrix(rep(1e-03, 20), ncol = 4))
        setnames(dt.conc, paste0("molecule", 1:4))
        
      } else {
        
        dt.conc <- values[[dt.conc.name]]
        
      }
      
    }
    
    dt.conc <- as.data.table(dt.conc)
    setnames(dt.conc, eval(as.name(paste0(module, ".part.names.data")))()[1:ncol(dt.conc)])
    
    values[[dt.conc.name]] <- dt.conc
    
    dt.conc
    
  })
  
  # return
  
  return(dt.conc.reac)
  
}

server_part.eq.data <- function(module = c("eq", "ab", "emf", "nm")) {
  
  part.eq.name <- paste0(module[1], ".part.eq")
  
  part.eq.reac <- reactive({
    
    if (!is.null(input[[part.eq.name]])) {
      
      part.eq <- hot_to_r(input[[part.eq.name]])
      
    } else {
      
      if (is.null(values[[part.eq.name]])) {
        
        part.eq <- as.data.table(matrix(rep("tot", 4), ncol = 4))
        setnames(part.eq, paste0("molecule", 1:4))
        
      } else {
        
        part.eq <- values[[part.eq.name]]
        
      }
      
    }
    
    part.eq <- as.data.table(part.eq)
    
    values[[part.eq.name]] <- part.eq
    
    part.eq
    
  })
  
  # return
  
  return(part.eq.reac)
  
}

server_cnst.data <- function(module = c("eq", "ab", "emf", "nm")) {
  
  cnst.name <- paste0(module[1], ".cnst")
  
  cnst.reac <- reactive({
    
    if (!is.null(input[[cnst.name]])) {
      
      cnst <- hot_to_r(input[[cnst.name]])
      
    } else {
      
      if (is.null(values[[cnst.name]])) {
        
        cnst <- as.data.table(matrix(rep(1, 4), ncol = 1))
        setnames(cnst, "k_constants_log10")
        
      } else {
        
        cnst <- values[[cnst.name]]
        
      }
      
    }
    
    cnst <- as.data.table(cnst)
    
    values[[cnst.name]] <- cnst
    
    cnst
    
  })
  
  # return
  
  return(cnst.reac)
  
}










