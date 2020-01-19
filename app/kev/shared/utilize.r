# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2020                                                 #
#                                                            #
# ########################################################## #


source("app/kev/shared/functions.r", chdir = TRUE)

# ---------------------- xlsx to csv ----------------------- #

# one file samples

xlsx.to.csv(xlsx.file = "input/calorimetry/ds.2.dsc/data.xlsx"
            , target.dir = ""
            , sep = ",", delim = "default"
            , subdir = "default")

xlsx.to.csv(xlsx.file = "input/calorimetry/ds.2.dsc/data.xlsx"
            , target.dir = ""
            , sep = ";", delim = "default"
            , subdir = "default")

xlsx.to.csv(xlsx.file = "input/calorimetry/ds.2.dsc/data.xlsx"
            , target.dir = ""
            , sep = "tab", delim = "."
            , subdir = "default")

# directories

xlsx.to.csv.dir(target.dir = "input/calorimetry"
                , sep = "all"
                , delim = "default"
                , subdir = "default"
                , ignore.pattern = "DEPR"
                , overwrite = FALSE)


