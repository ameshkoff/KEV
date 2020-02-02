# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2020                                                 #
#                                                            #
# ########################################################## #



# -------------------- load libraries ------------------------

library(testthat)

context(kev.context)




# ----------------------- run tests --------------------------

test_that("calorimetry_output_consistence", {

  for (dt.ind in 2:length(dt.test.list)) {
    
    expect_equal(dt.test.list[[1]], dt.test.list[[dt.ind]])
    
  }
  
})


