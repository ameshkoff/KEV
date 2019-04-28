# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #




cur.preproc <- function(dt.ttl) {
  
  # cleanse main dataset (dt.cur)
  
  dt.cur <- dt.ttl[["dt.cur"]]
  
  cln <- colnames(dt.cur)
  
  if (length(cln) != 2) stop(paste("Main dataset should contain exactly 2 columns: labels and values."
                                   , "Check your input data to be sure it does not have extra columns (may be empty) left or right to the data"))
  
  # define columns names
  
  tmp <- data.table(input = sort(cln), chck = c("label", "value"))
  tmp <- tmp[, str_detect(input, chck)]
  
  if (length(tmp[tmp == FALSE]) == 0) {
    
    dt.cur <- dt.cur[, sort(cln), with = FALSE]
    
  }
  
  setnames(dt.cur, c("label", "value"))
  
  # data types
  
  cln <- colnames(dt.cur)
  
  for (cl in cln) {
    
    if (dt.cur[, is.factor(eval(as.name(cl)))]) dt.cur[, eval(cl) := as.character(eval(as.name(cl)))]
    
    if (dt.cur[, is.character(eval(as.name(cl)))]){
      
      dt.cur[, eval(cl) := str_replace_all(eval(as.name(cl)), " ", "")]
      dt.cur[, eval(cl) := str_replace_all(eval(as.name(cl)), "\\,", ".")]
      
      dt.cur[, eval(cl) := as.numeric(eval(as.name(cl)))]
      
    }
    
  }
  
  # parameters
  
  dt.par <- dt.ttl[["dt.par"]]
  
  cln <- colnames(dt.par)
  
  for (cl in cln) {
    
    dt.par[, eval(cl) := as.character(eval(as.name(cl)))]
    dt.par[is.na(eval(as.name(cl))), eval(cl) := ""]
    
  }
  
  # extract parameters

  cur.task <- dt.par[name == "" & param == "task", value]
  if (length(cur.task) == 0) cur.task <- "spectrophotometry"
  
  window.borders <- c(NA, NA)
  
  if (length(dt.par[name == "" & param == "left", value]) > 0) window.borders[1] <- dt.par[name == "" & param == "left", value] %>% as.numeric()
  if (length(dt.par[name == "" & param == "right", value]) > 0) window.borders[2] <- dt.par[name == "" & param == "right", value] %>% as.numeric()
  
  dt.par <- dt.par[name != ""]

  list(dt.cur = dt.cur
       , cur.task = cur.task
       , window.borders = window.borders
       , dt.par = dt.par)
  
}









