# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #



source("app/KEV/eq_runner.r", chdir = TRUE)

eq.evaluation.runner(app = FALSE, sep = ";", subdir = "ds.2p", bs.name = "molecule2", thr.type = c("abs"), threshold = 1e-08, save.res = TRUE)
eq.evaluation.runner(app = FALSE, sep = ";", subdir = "ds.3p", bs.name = "molecule2", thr.type = c("abs"), threshold = 1e-08, save.res = TRUE)
eq.evaluation.runner(app = FALSE, sep = ";", subdir = "ds.5p", bs.name = "molecule4", thr.type = c("abs"), threshold = 1e-08, save.res = TRUE)
eq.evaluation.runner(app = FALSE, sep = ";", subdir = "ds.eq", bs.name = "molecule3", thr.type = c("abs"), threshold = 1e-08, save.res = TRUE)

eq.evaluation.runner(app = FALSE, sep = ",", subdir = "ds.3p.f", bs.name = "L", thr.type = c("rel"), threshold = 1e-08, save.res = FALSE)
eq.evaluation.runner(app = FALSE, sep = ";", subdir = "ds.3p", bs.name = "molecule2", thr.type = c("abs"), threshold = 1e-08, save.res = FALSE)
eq.evaluation.runner(app = FALSE, sep = ";", subdir = "ds.5p", bs.name = "molecule4", thr.type = c("abs"), threshold = 1e-08, save.res = FALSE)
