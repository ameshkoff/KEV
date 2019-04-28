# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #

require(RcppRoll)

cur.md.loess <- loess(value ~ label, dt.cur, degree = 2)
cur.md.pred <- dt.cur[, .(label, value = predict(cur.md.loess, dt.cur))]

cur.smooth <- dt.cur[, .(label, value = smooth(value))]
cur.runmed <- dt.cur[, .(label, value = runmed(value, k = 9))]

dlm <- 30

sm.number <- nrow(dt.cur) %/% dlm
sm.fill <- c(head(dt.cur[, value], sm.number) %>% mean()
             , dt.cur[as.integer(sm.number * dlm %/% 2) : as.integer(sm.number * dlm %/% 2) + sm.number, value] %>% mean(na.rm = TRUE)
             , tail(dt.cur[, value], sm.number) %>% mean())

sm.fill[is.na(sm.fill)] <- 0

cur.rollmean <- dt.cur[, .(label, value = roll_mean(value, n = sm.number, align = "center", na.rm = FALSE, fill = sm.fill))]


plot(dt.cur, type = "l")
lines(cur.md.pred, col = "red")
lines(cur.smooth, col = "blue")
lines(cur.runmed, col = "green")
lines(cur.rollmean, col = "purple")





cur.assumptions <- function(dt.cur, cur.task, window.borders, dt.par) {
  
  
  
  
  
  
  
 
  list(dt.cur = dt.cur
       , cur.task = cur.task
       , window.borders = window.borders
       , dt.par = dt.par)
   
}