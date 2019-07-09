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
                              , subdir = "curves/dsc.1.uvvis.no.assumptions/semicolon"
                              , file = NULL
                              , dt.list = NULL)

cur.plot.effects(cur.status)


# run modelling ----------------------------------------------

cur.status@window.borders[1] <- 210
# cur.status@window.borders[2] <- 440

cur.status <- cur.remove.curves(cur.status, min.expvalue = 200)

cur.status <- cur.model(cur.status, algorithm = "gaussnewton")
cur.status <- cur.model(cur.status, algorithm = "neldermead")


# universal data extraction (independent from step)

cur.status@metrics$r.squared

cur.object.effects(cur.status)
cur.auc(cur.status)

cur.plot.effects(cur.status)

# save

cur.save(cur.status, file = NULL)
cur.save(cur.status, file = "output.xlsx")
cur.save(cur.status, file = "output.zip")



# run loading & preprocessing --------------------------------

cur.status <- cur.data.runner(mode = "script"
                              , sep = ","
                              , subdir = "curves/dsc.2.ir"
                              , file = "data.xlsx"
                              , dt.list = NULL)

cur.plot.effects(cur.status)


# run modelling ----------------------------------------------

# cur.status@window.borders[1] <- 1600
# cur.status@window.borders[2] <- 1700

cur.status <- cur.remove.curves(cur.status, max.expvalue = 1680)

cur.status <- cur.model(cur.status, algorithm = "gaussnewton")
cur.status <- cur.model(cur.status, algorithm = "neldermead")



