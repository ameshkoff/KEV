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

test_that("calorimetry_output_is.full.list", {
  
  expect_type(rtrn, "list")
  expect_length(rtrn, 18)
  
})

test_that("calorimetry_output_non.empty.elements", {
  
  expect_gt(nrow(rtrn$dt.eq.conc), 0)
  expect_gt(nrow(rtrn$dt.heat.calc), 0)
  expect_gt(nrow(rtrn$dt.enth.calc), 0)
  expect_gt(nrow(rtrn$dt.metrics), 0)
  expect_gt(nrow(rtrn$dt.coef.input), 0)
  expect_gt(nrow(rtrn$dt.conc.input), 0)
  expect_gt(nrow(rtrn$cnst.input), 0)
  expect_type(rtrn$part.eq.input, "integer")
  expect_gt(nrow(rtrn$dt.heat.input), 0)
  expect_s3_class(rtrn$dt.enth.input, "data.table")
  expect_gt(length(rtrn$cmp.tune.input), 0)
  expect_gt(length(rtrn$calorimeter.type.input), 0)
  expect_gt(length(rtrn$init.vol.input), 0)
  
  if ((str_to_lower(kev.context) %like% "(\\b|\\_)(no(\\_|\\.)opt)\\b")) {
    
    expect_null(rtrn$err.diff)
    expect_null(rtrn$cor.m)
    expect_length(rtrn$cnst.tune, 0L)
    expect_null(rtrn$cnst.dev)
    expect_null(rtrn$lrate.fin)

  } else {
    
    expect_gt(length(rtrn$err.diff), 0)
    expect_gt(nrow(rtrn$cor.m), 0)
    expect_gt(length(rtrn$cnst.tune), 0)
    expect_gt(nrow(rtrn$cnst.dev), 0)
    expect_gt(length(rtrn$lrate.fin), 0)
    
  }
  
})





