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
  
  if (!(str_to_lower(kev.context) %like% "(\\b|\\_)(bad)\\b"))
    expect_gt(rtrn$adj.r.squared, .99)

})






