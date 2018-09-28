# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #



source("app/KEV/eq_runner.r", chdir = TRUE)

eq.evaluation.runner(mode = "script", sep = ";", subdir = "ds.2p", bs.name = "molecule2", thr.type = c("abs"), threshold = 1e-08, save.res = TRUE)
eq.evaluation.runner(mode = "script", sep = ";", subdir = "ds.3p", bs.name = "molecule2", thr.type = c("abs"), threshold = 1e-08, save.res = TRUE)
eq.evaluation.runner(mode = "script", sep = ";", subdir = "ds.5p", bs.name = "molecule4", thr.type = c("abs"), threshold = 1e-08, save.res = TRUE)
eq.evaluation.runner(mode = "script", sep = "tab", subdir = "ds.5p.tab", bs.name = "molecule4", thr.type = c("abs"), threshold = 1e-08, save.res = TRUE)
eq.evaluation.runner(mode = "script", sep = ";", subdir = "ds.eq", bs.name = "molecule3", thr.type = c("abs"), threshold = 1e-08, save.res = TRUE)

eq.evaluation.runner(mode = "script", sep = ",", subdir = "ds.3p.f", bs.name = "L", thr.type = c("rel"), threshold = 1e-08, save.res = FALSE)
eq.evaluation.runner(mode = "script", sep = ";", subdir = "ds.3p", bs.name = "molecule2", thr.type = c("abs"), threshold = 1e-08, save.res = FALSE)
eq.evaluation.runner(mode = "script", sep = ";", subdir = "ds.5p", bs.name = "molecule4", thr.type = c("abs"), threshold = 1e-08, save.res = FALSE)
eq.evaluation.runner(mode = "script", sep = "tab", subdir = "ds.5p.tab", bs.name = "molecule4", thr.type = c("abs"), threshold = 1e-08, save.res = FALSE)
eq.evaluation.runner(mode = "script", sep = ",", subdir = "ds.sp", bs.name = "L", thr.type = c("rel"), threshold = 1e-08, save.res = FALSE)
eq.evaluation.runner(mode = "script", sep = ",", subdir = "ds.3p.2eq", bs.name = "L", thr.type = c("rel"), threshold = 1e-08, save.res = FALSE)



source("app/KEV/ab_runner.r", chdir = TRUE)

ab.evaluation.runner(mode = "script", sep = ",", subdir = "dsl.1"
                     , eq.thr.type = "rel", eq.threshold = 1e-08
                     , cnst.tune = c("SB")
                     , algorithm = "direct search", method = "basic wls"
                     , search.density = 1, lrate.init = .5, ab.threshold = 5e-7
                     , save.res = TRUE)

ab.evaluation.runner(mode = "script", sep = ",", subdir = "dsl.2"
                     , eq.thr.type = "rel", eq.threshold = 1e-08
                     , cnst.tune = c("CuL2")
                     , algorithm = "direct search", method = "basic wls"
                     , search.density = 1, lrate.init = .5, ab.threshold = 5e-7
                     , save.res = TRUE)

ab.evaluation.runner(mode = "script", sep = ",", subdir = "dsl.3"
                     , eq.thr.type = "rel", eq.threshold = 1e-08
                     , cnst.tune = c("HL", "H2L")
                     , algorithm = "direct search", method = "basic wls"
                     , search.density = 1, lrate.init = .5, ab.threshold = 5e-7
                     , save.res = TRUE)









