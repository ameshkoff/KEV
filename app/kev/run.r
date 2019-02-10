# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #



source("app/KEV/eq_runner.r", chdir = TRUE)

eq.evaluation.runner(mode = "script", sep = ";", subdir = "concentrations/ds.2p"
                     , bs.name = "molecule2", thr.type = c("abs"), threshold = 1e-08, save.res = TRUE)
eq.evaluation.runner(mode = "script", sep = ";", subdir = "concentrations/ds.3p"
                     , bs.name = "molecule2", thr.type = c("abs"), threshold = 1e-08, save.res = TRUE)
eq.evaluation.runner(mode = "script", sep = ";", subdir = "concentrations/ds.5p"
                     , bs.name = "molecule4", thr.type = c("abs"), threshold = 1e-08, save.res = TRUE)
eq.evaluation.runner(mode = "script", sep = "tab", subdir = "concentrations/ds.5p.tab"
                     , bs.name = "molecule4", thr.type = c("abs"), threshold = 1e-08, save.res = TRUE)

eq.evaluation.runner(mode = "script", sep = ",", subdir = "concentrations/ds.3p.1eq"
                     , bs.name = "L", thr.type = c("rel"), threshold = 1e-08, save.res = FALSE)
eq.evaluation.runner(mode = "script", sep = ";", subdir = "concentrations/ds.3p"
                     , bs.name = "molecule2", thr.type = c("abs"), threshold = 1e-08, save.res = FALSE)
eq.evaluation.runner(mode = "script", sep = ";", subdir = "concentrations/ds.5p"
                     , bs.name = "molecule4", thr.type = c("abs"), threshold = 1e-08, save.res = FALSE)
eq.evaluation.runner(mode = "script", sep = "tab", subdir = "concentrations/ds.5p.tab"
                     , bs.name = "molecule4", thr.type = c("abs"), threshold = 1e-08, save.res = FALSE)
eq.evaluation.runner(mode = "script", sep = ",", subdir = "concentrations/ds.3p.2eq"
                     , bs.name = "L", thr.type = c("rel"), threshold = 1e-08, save.res = FALSE)


#

source("app/KEV/ab_runner.r", chdir = TRUE)

ab.evaluation.runner(mode = "script", sep = ",", subdir = "spectrophotometry/dsl.1"
                     , eq.thr.type = "rel", eq.threshold = 1e-08
                     , cnst.tune = c("SB")
                     , algorithm = "direct search", ab.mode = "base", method = "basic wls"
                     , search.density = 1, lrate.init = .5, ab.threshold = 5e-7
                     , save.res = TRUE)

ab.evaluation.runner(mode = "script", sep = ",", subdir = "spectrophotometry/dsl.2"
                     , eq.thr.type = "rel", eq.threshold = 1e-08
                     , cnst.tune = c("CuL2")
                     , algorithm = "direct search", ab.mode = "base", method = "basic wls"
                     , search.density = 1, lrate.init = .5, ab.threshold = 5e-7
                     , save.res = TRUE)

ab.evaluation.runner(mode = "script", sep = ",", subdir = "spectrophotometry/dsl.3"
                     , eq.thr.type = "rel", eq.threshold = 1e-08
                     , cnst.tune = c("HL", "H2L")
                     , algorithm = "direct search", ab.mode = "base", method = "basic wls"
                     , search.density = 1, lrate.init = .5, ab.threshold = 5e-7
                     , save.res = TRUE)

#

ab.evaluation.runner(mode = "script", sep = ",", subdir = "spectrophotometry/dsl.1"
                     , eq.thr.type = "rel", eq.threshold = 1e-08
                     , cnst.tune = c("SB")
                     , algorithm = "direct search", ab.mode = "base", method = "basic wls"
                     , search.density = 1, lrate.init = .5, ab.threshold = 1e-7
                     , save.res = FALSE)

ab.evaluation.runner(mode = "script", sep = ",", subdir = "spectrophotometry/dsl.2"
                     , eq.thr.type = "rel", eq.threshold = 1e-08
                     , cnst.tune = c("CuL2")
                     , algorithm = "direct search", ab.mode = "base", method = "basic wls"
                     , search.density = 1, lrate.init = .5, ab.threshold = 5e-7
                     , save.res = FALSE)

ab.evaluation.runner(mode = "script", sep = ",", subdir = "spectrophotometry/dsl.3"
                     , eq.thr.type = "rel", eq.threshold = 1e-08
                     , cnst.tune = c("HL", "H2L")
                     , algorithm = "direct search", ab.mode = "base", method = "basic wls"
                     , search.density = 1, lrate.init = .5, ab.threshold = 5e-7
                     , save.res = FALSE, wl.tune = c(200, 300))

ab.evaluation.runner(mode = "script", sep = ",", subdir = "spectrophotometry/dsl.3.no.ext"
                     , eq.thr.type = "rel", eq.threshold = 1e-08
                     , cnst.tune = c("HL", "H2L")
                     , algorithm = "direct search", ab.mode = "base", method = "basic wls"
                     , search.density = 1, lrate.init = .5, ab.threshold = 5e-7
                     , save.res = FALSE, wl.tune = c(200, 300))

ab.evaluation.runner(mode = "script", sep = ";", subdir = "spectrophotometry/dsl.4/full.spectra"
                     , eq.thr.type = "rel", eq.threshold = 1e-08
                     # , cnst.tune = "SB"
                     , algorithm = "direct search", ab.mode = "base", method = "basic wls"
                     , search.density = 1, lrate.init = .5, ab.threshold = 5e-7
                     , save.res = FALSE
                     # , wl.tune = c(306, 387)
                     )

ab.evaluation.runner(mode = "script", sep = ";", subdir = "spectrophotometry/dsl.7"
                     , eq.thr.type = "rel", eq.threshold = 1e-08
                     # , cnst.tune = "ZnL"
                     , algorithm = "direct search", ab.mode = "base", method = "basic wls"
                     , search.density = 1, lrate.init = .5, ab.threshold = 5e-7
                     , save.res = FALSE, wl.tune = c(299, 329, 414))

#

ab.evaluation.runner(mode = "script", sep = ",", subdir = "spectrophotometry/dsl.1"
                     , eq.thr.type = "rel", eq.threshold = 1e-08
                     , cnst.tune = c("SB")
                     , algorithm = "basic search", ab.mode = "grid", method = "basic wls"
                     , search.density = 1, lrate.init = .0005, ab.threshold = 1e-7
                     , save.res = FALSE)


ab.evaluation.runner(mode = "script", sep = ";", subdir = "spectrophotometry/dsl.9/csv.semicolon"
                     , eq.thr.type = "rel", eq.threshold = 1e-08
                     , algorithm = "direct search", ab.mode = "base", method = "basic wls"
                     , search.density = 1, lrate.init = .5, ab.threshold = 5e-7
                     , save.res = FALSE
)

ab.evaluation.runner(mode = "script", sep = "tab", subdir = "spectrophotometry/dsl.9/txt.tab"
                     , eq.thr.type = "rel", eq.threshold = 1e-08
                     , algorithm = "direct search", ab.mode = "base", method = "basic wls"
                     , search.density = 1, lrate.init = .5, ab.threshold = 5e-7
                     , save.res = FALSE
)

#

ab.evaluation.runner(mode = "script", sep = ";", subdir = "spectrophotometry/dsl.4/full.spectra"
                     , eq.thr.type = "rel", eq.threshold = 1e-08
                     , cnst.tune = c("SB")
                     , algorithm = "direct search", ab.mode = "base", method = "basic wls"
                     , search.density = 1, lrate.init = .5, ab.threshold = 5e-7
                     , save.res = FALSE, wl.tune = c(306, 387))



source("app/kev/sp_runner.r", chdir = TRUE)

sp.evaluation.runner(mode = "script", sep = "tab", subdir = "molar.extinction.coefficients/dsl.4", save.res = TRUE)


#

source("app/KEV/emf_runner.r", chdir = TRUE)

emf.evaluation.runner(mode = "script", sep = ";", subdir = "emf/dsp.1/csv.semicolon"
                     , eq.thr.type = "rel", eq.threshold = 1e-08
                     # , cnst.tune = c("HL")
                     , algorithm = "direct search", emf.mode = "base", method = "basic wls"
                     , search.density = 1, lrate.init = .5, emf.threshold = 5e-7
                     , save.res = TRUE)

emf.evaluation.runner(mode = "script", sep = ",", subdir = "emf/dsp.2"
                      , eq.thr.type = "rel", eq.threshold = 1e-08
                      # , cnst.tune = c("NiL", "NiL2")
                      , algorithm = "direct search", emf.mode = "base", method = "basic wls"
                      , search.density = 1, lrate.init = .5, emf.threshold = 5e-7
                      , save.res = TRUE)





emf.evaluation.runner(mode = "script", sep = ",", subdir = "emf/dsp.1/comma"
                      , eq.thr.type = "rel", eq.threshold = 1e-08
                      , cnst.tune = c("HL")
                      , algorithm = "direct search", emf.mode = "base", method = "basic wls"
                      , search.density = 1, lrate.init = .5, emf.threshold = 5e-7
                      , save.res = FALSE)

emf.evaluation.runner(mode = "script", sep = ",", subdir = "emf/dsp.2"
                      , eq.thr.type = "rel", eq.threshold = 1e-08
                      , cnst.tune = c("NiL", "NiL2")
                      , algorithm = "direct search", emf.mode = "base", method = "basic wls"
                      , search.density = 1, lrate.init = .5, emf.threshold = 5e-4
                      , save.res = FALSE)

emf.evaluation.runner(mode = "script", sep = ";", subdir = "emf/dsp.3"
                      , eq.thr.type = "rel", eq.threshold = 1e-08
                      , algorithm = "direct search", emf.mode = "base", method = "basic wls"
                      , search.density = 1, lrate.init = .5, emf.threshold = 5e-4
                      , save.res = FALSE)






