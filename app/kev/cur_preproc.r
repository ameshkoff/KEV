# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #

require(RcppRoll)

# ------------------------------------------------------------

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
  
  window.borders <- c(dt.cur[, min(label)], dt.cur[, max(label)])
  
  if (length(dt.par[name == "" & param == "left", value]) > 0) window.borders[1] <- dt.par[name == "" & param == "left", value] %>% as.numeric()
  if (length(dt.par[name == "" & param == "right", value]) > 0) window.borders[2] <- dt.par[name == "" & param == "right", value] %>% as.numeric()
  
  dt.par <- dt.par[name != ""]
  dt.par[, value := as.numeric(value)]

  list(dt.cur = dt.cur
       , cur.task = cur.task
       , window.borders = window.borders
       , dt.par = dt.par)
  
}

cur.initial.guess <- function(dt.cur, cur.task, window.borders, dt.par, smooth.delimiter = 30) {
  
  # smooth observations
  
  sm.number <- nrow(dt.cur) %/% smooth.delimiter
  sm.number.half <- sm.number %/% 2
  
  sm.fill <- c(head(dt.cur[, value], sm.number) %>% mean()
               , dt.cur[sm.number.half * smooth.delimiter : sm.number.half * smooth.delimiter + sm.number, value] %>% mean(na.rm = TRUE)
               , tail(dt.cur[, value], sm.number) %>% mean())
  
  sm.fill[is.na(sm.fill)] <- 0
  
  dt.cur <- dt.cur[, .(label, value, value.sm = roll_mean(value, n = sm.number, align = "center", na.rm = FALSE, fill = sm.fill))]
  
  # guess curve parameters if not set
  # for now only gaussian curves, later try to guess a curve and then its parameters - much more complex
  
  if (is.null(dt.par) || nrow(dt.par) == 0) {
    
    # prepare
    
    dt.cur[, sign := value.sm - shift(value.sm)]
    dt.cur[is.na(sign), sign := 0]
    
    dt.cur[, sign := sign(sign)]
    
    tmp.max <- dt.cur[sm.number.half:(nrow(dt.cur) - sm.number.half)][sign < 1 & shift(sign) == 1][order(label)]
    tmp.min <- dt.cur[sm.number.half:(nrow(dt.cur) - sm.number.half)][sign == 1 & shift(sign) < 1][order(label)]
    
    tmp <- tmp.min[, .(label, label.left = label)][tmp.max[, .(label, label.base = label, value)], on = .(label<=label), allow.cartesian = TRUE][
      , .(label.base, label.left, value)]
    tmp <- tmp[order(label.base, -label.left)][, .SD[1], label.base]
    
    tmp <- tmp.min[, .(label, label.right = label)][tmp[, .(label.base, label.left, label = label.base, value)]
                                                    , on = .(label>=label), allow.cartesian = TRUE][
                                                      !is.na(label.base), .(label.base, label.left, label.right, value)]
    tmp <- tmp[order(label.base, label.right)][, .SD[1], label.base]
    
    tmp[is.na(label.right), label.right := dt.cur[, max(label)]]
    
    # expected value
    
    dt.par <- data.table(name = as.character(tmp[, label.base])
                         , design = "gaussian"
                         , param = "expvalue"
                         , value = tmp[, label.base])
    
    # hwhm (half width at half maximum)
    
    tmp[, label.left := label.base - (label.base - label.left) * .35]
    tmp[, label.right := label.base + (label.right - label.base) * .35]
    
    tmp <- dt.cur[, .(label, label.left = label, value.left = value)][tmp, on = .(label == label.left), roll = Inf]
    tmp[, label := NULL]
    tmp <- dt.cur[, .(label, label.right = label, value.right = value)][tmp, on = .(label == label.right), roll = -Inf]
    tmp[, label := NULL]
    
    dt.par <- rbind(dt.par
                    , data.table(name = as.character(tmp[, label.base])
                                 , design = "gaussian"
                                 , param = "hwhm"
                                 , value = tmp[, label.right - label.left]))
    
    # amplitude
    
    dt.par <- rbind(dt.par
                    , data.table(name = as.character(tmp[, label.base])
                                 , design = "gaussian"
                                 , param = "amplitude"
                                 , value = tmp[, value]))
    
  }
  
  dt.par = dt.par
  
}








