# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2020                                                 #
#                                                            #
# ########################################################## #



#----------------- load libraries & data ----------------------

library(testthat)

#

source("app/kev/algo/calorimetry/ht_runner.r", chdir = TRUE)
source("tests/unit.tests/functions.r")



# ------------------------ calorimetry --------------------------

# load data

options(warn = 2)

dt.test.list <- kev.test.getdata.all(target.dir = "input/calorimetry"
                                     , getdata.fn = ht.test.getdata
                                     , ignore.pattern = "(\\b|\\_)old\\b|DEPR")

options(warn = 0)

# run tests

kev.test.formal(dt.test.list = dt.test.list
                , test.fn = ht.test.formal)

kev.test.consistent(dt.test.list = dt.test.list
                   , test.fn = ht.test.consistent)

kev.test.stat(dt.test.list = dt.test.list
              , test.fn = ht.test.stat
              , stop.on.fail = FALSE)





