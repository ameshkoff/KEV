# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2020                                                 #
#                                                            #
# ########################################################## #



# -------------------- load libraries -------------------------


#

source("app/kev/algo/calorimetry/ht_runner.r", chdir = TRUE)

# loop over all data in calorimetry directory


rtrn <- ht.evaluation.runner(mode = "script", sep = "tab", subdir = "calorimetry/ds.1.dsc"
                             , eq.thr.type = "rel", eq.threshold = 1e-08
                             , algorithm = "direct search", ht.mode = "base", method = "basic wls"
                             , search.density = 1, lrate.init = .5, ht.threshold = 5e-7
                             , filename = "data.xlsx"
)

source("tests/unit.tests/tests.r", chdir = FALSE)






