# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #



source("app/KEV/cur_runner.r", chdir = TRUE)


# run loading & preprocessing --------------------------------

cur.status <- cur.data.runner(mode = "script"
                              , sep = ";"
                              , subdir = "curves/dsc.1.no.assumptions/semicolon"
                              , file = NULL
                              , save.res = TRUE
                              , dt.list = NULL)


# run modelling ----------------------------------------------

cur.status@window.borders[1] <- 210
# cur.status@window.borders[2] <- 440

cur.status <- cur.remove.curves(cur.status, min.expvalue = 200)

cur.status <- cur.model(cur.status)





cur.plot.initial(cur.status)

cur.plot.model(cur.status)

cur.status@metrics$r.squared


# TO DO: save


