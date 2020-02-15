# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2020                                                 #
#                                                            #
# ########################################################## #



#----------------- load libraries & data ----------------------

library(testthat)
library(beepr)

#

source("app/kev/algo/calorimetry/ht_runner.r", chdir = TRUE, local = TRUE)
source("app/kev/algo/calorimetry/ht_save.r", chdir = TRUE, local = TRUE)
source("tests/functions.r")



# ------------------------ calorimetry --------------------------

# load data

options(warn = 2)

dt.test.list <- kev.test.getdata.all(target.dir = "input/calorimetry"
                                     , getdata.fn = ht.test.getdata
                                     , ignore.pattern = "(\\b|\\_)old\\b|DEPR")

options(warn = 0)

# run tests

kev.test.formal(dt.test.list = dt.test.list
                , test.fn = ht.test.formal
                , stop.on.fail = TRUE)

kev.test.consistent(dt.test.list = dt.test.list
                   , test.fn = ht.test.consistent
                   , stop.on.fail = TRUE)

kev.test.stat(dt.test.list = dt.test.list
              , test.fn = ht.test.stat
              , stop.on.fail = FALSE)

# regression

ht.update.test <- TRUE
ht.update.stable <- TRUE

if (ht.update.test) {
  
  kev.clear.dir("tests/data.scripts/test/calorimetry")
  kev.test.regression.write.data(dt.test.list, path = "tests/data.scripts/test", write.fn = ht.save)
  
}

if (ht.update.stable) {
  
  kev.clear.dir("tests/data.scripts/stable/calorimetry")
  kev.copy.dir("output/calorimetry"
               , "tests/data.scripts/stable"
               , ignore.pattern = "(\\b|\\_)old\\b|DEPR")
  
}

# # uncomment to write down main output
# kev.clear.dir("output/calorimetry")
# kev.test.regression.write.data(dt.test.list, path = "output", write.fn = ht.save)

kev.test.regression("tests/data.scripts", TRUE)

beep("fanfare")



