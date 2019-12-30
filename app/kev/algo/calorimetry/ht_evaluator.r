# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2020                                                 #
#                                                            #
# ########################################################## #


# main optmizer -------------------------------------------- #

kev.constant.optimizer <- function(objective.fn = ht.objective.function
                                  , evaluation.fn = ht.enth.evaluator
                                  , start.values
                                  , lower.bound
                                  , upper.bound
                                  , dt.list # data tables wrapped in the list
                                  , algorithm.options = list(algorithm = "direct search"
                                                             , hardstop = 100
                                                             , lrate.init = .5
                                                             , search.density = 1
                                                             , const.threshold = 5e-5
                                                             , eq.threshold = 1e-08
                                                             , eq.thr.type = c("rel", "abs"))
                                  , metrics = "mse"
                                  , mode = c("base", "grid", "debug")
                                  , verbose = TRUE) {
  
  
  
  
}


# direct search optimizer ---------------------------------- #
# !!! create closure into the the body of the main optimizer (wrapper) to reduce data transmission

kev.direct.search <- function() {
  
  
  
}



# objective function --------------------------------------- #

ht.objective.function <- function() {
  
  
  
}


# enthalpies evaluator ------------------------------------- #

ht.enth.evaluator <- function() {
  
  
  
}






