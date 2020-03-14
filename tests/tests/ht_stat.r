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

test_that("calorimetry_output_metrics", {
  
  if ((str_to_lower(kev.context) %like% "(\\b|\\_)(bad)\\b")) {
    
    expect_lt(rtrn$dt.metrics[str_to_lower(metrics) == "adj.r^2", value], .99)
    
  } else if (str_to_lower(kev.context) %like% "(\\b|\\_)(no(\\_|\\.)opt)\\b") {
    
    expect_lt(rtrn$dt.metrics[str_to_lower(metrics) == "nrmse", value], .1)
    expect_lt(rtrn$dt.metrics[str_to_lower(metrics) == "smape", value], .1)
    
  } else {
    
    expect_gt(rtrn$dt.metrics[str_to_lower(metrics) == "adj.r^2", value], .99)
    
  }

})

test_that("calorimetry_output_validity", {
  
  if (str_to_lower(kev.context) %like% "(\\b|\\_)(bad)\\b" & !(str_to_lower(kev.context) %like% "(\\b|\\_)(no(\\_|\\.)opt)\\b")) {
    
    expect_gt(nrow(rtrn$cnst.dev[Validity != "OK" | is.na(Validity)]), 0)
    
  } else if (!(str_to_lower(kev.context) %like% "(\\b|\\_)(no(\\_|\\.)opt)\\b")) {
    
    expect_equal(nrow(rtrn$cnst.dev[Validity != "OK" | is.na(Validity)]), 0)
    
  }
  
})






