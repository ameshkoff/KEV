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

server_part.names.data <- function(module = c("eq", "ab", "emf", "nm", "sp")) {
  
  part.names.name <- paste0(module[1], ".part.names")
  
  reactive({
    
    if (!is.null(input$part.names)) {
      
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
  
  part.names
  
}
  

