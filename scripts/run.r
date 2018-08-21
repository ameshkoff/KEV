# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #



source("scripts/eq_concentrations.r", chdir = TRUE)

eq.conc.exec(sep = ";", subdir = "ds.2p", bs.name = "molecule2", thr.type = c("abs"), threshold = 1e-08, verbose = TRUE)
eq.conc.exec(sep = ";", subdir = "ds.3p", bs.name = "molecule2", thr.type = c("abs"), threshold = 1e-08, verbose = TRUE)
eq.conc.exec(sep = ";", subdir = "ds.5p", bs.name = "molecule4", thr.type = c("abs"), threshold = 1e-08, verbose = TRUE)
eq.conc.exec(sep = ";", subdir = "ds.eq", bs.name = "molecule3", thr.type = c("abs"), threshold = 1e-08, verbose = TRUE)