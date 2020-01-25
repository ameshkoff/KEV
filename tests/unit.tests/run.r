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

source("tests/unit.tests/functions.r")
source("app/kev/algo/calorimetry/ht_runner.r", chdir = TRUE)



# ------------------------ calorimetry --------------------------

dt.test.list <- kev.test.getdata.all(target.dir = "input/calorimetry"
                                     , getdata.fn = ht.test.getdata
                                     , ignore.pattern = "(\\b|\\_)old\\b|DEPR")

#

kev.test.formal(dt.test.list = dt.test.list
                , test.fn = ht.test.formal)

kev.test.consistent(dt.test.list = dt.test.list
                   , test.fn = ht.test.consistent)






