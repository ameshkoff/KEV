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

test_that("calorimetry_output_r.squared", {
  
  if ((str_to_lower(kev.context) %like% "(\\b|\\_)(bad)\\b")) {
    
    expect_lt(rtrn$adj.r.squared, .99)
    
  } else {
    
    expect_gt(rtrn$adj.r.squared, .99)
    
  }

})

test_that("calorimetry_output_validity", {
  
  if (str_to_lower(kev.context) %like% "(\\b|\\_)(bad)\\b" & !(str_to_lower(kev.context) %like% "(\\b|\\_)(no(\\_|\\.)opt)\\b")) {
    
    expect_gt(nrow(rtrn$cnst.dev[validity != "OK" | is.na(validity)]), 0)
    
  } else if (!(str_to_lower(kev.context) %like% "(\\b|\\_)(no(\\_|\\.)opt)\\b")) {
    
    expect_equal(nrow(rtrn$cnst.dev[validity != "OK" | is.na(validity)]), 0)
    
  }
  
})






