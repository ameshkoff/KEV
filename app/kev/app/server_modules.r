# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #



# data ------------------------------

# input data

common.input.data <- function(module = c("eq", "ab", "emf", "nm", "sp")) {
  
  dt.coef.name <- paste0(module[1], ".dt.coef")
  
  dt.coef <- reactive({
    
    if (!is.null(input[[dt.coef.name]])) {
      
      dt.coef <- hot_to_r(input[[dt.coef.name]])
      
    } else {
      
      if (is.null(values[[dt.coef.name]])) {
        
        dt.coef <- as.data.table(matrix(rep(1, 16), 4))
        setnames(dt.coef, paste0("molecule", 1:4))
        
      } else {
        
        dt.coef <- values[[dt.coef.name]]
        
      }
      
    }
    
    dt.coef <- as.data.table(dt.coef)
    setnames(dt.coef, part.names.data()[1:ncol(dt.coef)])
    
    values[[dt.coef.name]] <- dt.coef
    
    dt.coef
    
  })
  
  # return
  
  return(dt.coef)
  
}


# common.input.data <- function(input, output, session, module = c("eq", "ab", "emf", "nm", "sp")) {
#   
#   dt.coef.name <- paste0(module, ".dt.coef")
#   
#   dt.coef <- reactive({
#     
#     if (!is.null(input[[dt.coef.name]])) {
#       
#       dt.coef <- hot_to_r(input[[dt.coef.name]])
#       
#     } else {
#       
#       if (is.null(values[[dt.coef.name]])) {
#         
#         dt.coef <- as.data.table(matrix(rep(1, 16), 4))
#         setnames(dt.coef, paste0("molecule", 1:4))
#         
#       } else {
#         
#         dt.coef <- values[[dt.coef.name]]
#         
#       }
#       
#     }
#     
#     dt.coef <- as.data.table(dt.coef)
#     setnames(dt.coef, part.names.data()[1:ncol(dt.coef)])
#     
#     values[[dt.coef.name]] <- dt.coef
#     
#     dt.coef
#     
#   })
#   
#   # return
#   
#   return(dt.coef)
#   
# }
  
  

