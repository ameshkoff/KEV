# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2020                                                 #
#                                                            #
# ########################################################## #


source("app/kev/shared/functions.r", chdir = TRUE)

xlsx.to.csv(xlsx.file = "input/calorimetry/ds.2.dsc/data.xlsx"
            , sep = ",", delim = "default"
            , target.dir = "", subdir = "default")

xlsx.to.csv(xlsx.file = "input/calorimetry/ds.2.dsc/data.xlsx"
            , sep = ";", delim = "default"
            , target.dir = "", subdir = "default")

xlsx.to.csv(xlsx.file = "input/calorimetry/ds.2.dsc/data.xlsx"
            , sep = "tab", delim = "."
            , target.dir = "", subdir = "default")


