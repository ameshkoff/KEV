# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #

require(RcppRoll)


cur.assumptions <- function(dt.cur, cur.task, window.borders, dt.par, smooth.delimiter = 30) {
  
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
    
    # median
    
    dt.par <- data.table(name = as.character(tmp[, label.base])
                         , design = "gauss"
                         , param = "median"
                         , value = tmp[, label.base])
    
    # hwhm (half width at half maximum)
    
    tmp[, label.left := label.base - (label.base - label.left) * .75]
    tmp[, label.right := label.base + (label.right - label.base) * .75]
    
    tmp <- dt.cur[, .(label, label.left = label, value.left = value)][tmp, on = .(label == label.left), roll = Inf]
    tmp[, label := NULL]
    tmp <- dt.cur[, .(label, label.right = label, value.right = value)][tmp, on = .(label == label.right), roll = -Inf]
    tmp[, label := NULL]
    
    dt.par <- rbind(dt.par
                    , data.table(name = as.character(tmp[, label.base])
                                 , design = "gauss"
                                 , param = "hwhm"
                                 , value = tmp[, label.right - label.left]))
    
    # amplitude
    
    dt.par <- rbind(dt.par
                    , data.table(name = as.character(tmp[, label.base])
                                 , design = "gauss"
                                 , param = "amplitude"
                                 , value = tmp[, value]))

  }

  list(dt.cur = dt.cur
       , cur.task = cur.task
       , window.borders = window.borders
       , dt.par = dt.par)
   
}



