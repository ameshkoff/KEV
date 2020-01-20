# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #



source("app/kev/algo/concentrations/eq_runner.r", chdir = TRUE)

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
eq.evaluation.runner(mode = "script", sep = ";", subdir = "concentrations/ds.3p.2eq"
                     , bs.name = "L", thr.type = c("rel"), threshold = 1e-08, save.res = FALSE)

eq.evaluation.runner(mode = "script", sep = ";", subdir = "concentrations/ds.2p"
                     , bs.name = "molecule2", thr.type = c("abs"), threshold = 1e-08, save.res = FALSE)
eq.evaluation.runner(mode = "script", sep = ";", subdir = "concentrations/ds.2p.1eq/xlsx"
                     , bs.name = "L", thr.type = c("abs"), threshold = 1e-08, save.res = FALSE, filename = "data.xlsx")

eq.evaluation.runner(mode = "script", sep = ",", subdir = "concentrations/ds.3p.long/csv.comma"
                     , bs.name = "PO4", thr.type = c("abs"), threshold = 1e-08, save.res = FALSE)
eq.evaluation.runner(mode = "script", sep = ",", subdir = "concentrations/ds.3p.long"
                     , bs.name = "PO4", thr.type = c("abs"), threshold = 1e-08, save.res = FALSE, filename = "example_eq_conc_calc.xlsx")

eq.evaluation.runner(mode = "script", sep = ",", subdir = "concentrations/ds.3p.series/csv.comma"
                     , bs.name = "M", thr.type = c("abs"), threshold = 1e-08, save.res = FALSE)



#

source("app/kev/algo/spectrophotometry/ab_runner.r", chdir = TRUE)

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

ab.evaluation.runner(mode = "script", sep = ";", subdir = "spectrophotometry/dsl.12/csv.semicolon"
                     , eq.thr.type = "rel", eq.threshold = 1e-08
                     , algorithm = "direct search", ab.mode = "base", method = "basic wls"
                     , search.density = 1, lrate.init = .5, ab.threshold = 5e-7
                     , save.res = TRUE)

ab.evaluation.runner(mode = "script", sep = ";", subdir = "spectrophotometry/dsl.8"
                     , eq.thr.type = "rel", eq.threshold = 1e-08
                     , algorithm = "direct search", ab.mode = "base", method = "basic wls"
                     , search.density = 1, lrate.init = .5, ab.threshold = 1e-7
                     , save.res = TRUE
)

ab.evaluation.runner(mode = "script", sep = ",", subdir = "spectrophotometry/dsl.9/csv.comma"
                     , eq.thr.type = "rel", eq.threshold = 1e-08
                     , algorithm = "direct search", ab.mode = "base", method = "basic wls"
                     , search.density = 1, lrate.init = .5, ab.threshold = 1e-7
                     , save.res = TRUE
)

ab.evaluation.runner(mode = "script", sep = ";", subdir = "spectrophotometry/dsl.9/csv.semicolon"
                     , eq.thr.type = "rel", eq.threshold = 1e-08
                     , algorithm = "direct search", ab.mode = "base", method = "basic wls"
                     , search.density = 1, lrate.init = .5, ab.threshold = 1e-7
                     , save.res = TRUE
)

ab.evaluation.runner(mode = "script", sep = "tab", subdir = "spectrophotometry/dsl.9/txt.tab"
                     , eq.thr.type = "rel", eq.threshold = 1e-08
                     , algorithm = "direct search", ab.mode = "base", method = "basic wls"
                     , search.density = 1, lrate.init = .5, ab.threshold = 1e-7
                     , save.res = TRUE
)

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


ab.evaluation.runner(mode = "script", sep = ",", subdir = "spectrophotometry/dsl.9/csv.comma"
                     , eq.thr.type = "rel", eq.threshold = 1e-08
                     , algorithm = "direct search", ab.mode = "base", method = "basic wls"
                     , search.density = 1, lrate.init = .5, ab.threshold = 5e-7
                     , save.res = FALSE
)

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



source("app/kev/algo/molar.extinction.coefficients/sp_runner.r", chdir = TRUE)

sp.evaluation.runner(mode = "script", sep = "tab", subdir = "molar.extinction.coefficients/dsl.4", save.res = TRUE)


#

source("app/kev/algo/emf/emf_runner.r", chdir = TRUE)

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

#

emf.evaluation.runner(mode = "script", sep = ",", subdir = "emf/dsp.1/csv.comma"
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

#

source("app/kev/algo/nmr/nm_runner.r", chdir = TRUE)

nm.evaluation.runner(mode = "script", sep = ",", subdir = "nmr/dsn.1"
                     , eq.thr.type = "rel", eq.threshold = 1e-08
                     , algorithm = "direct search", nm.mode = "base", method = "basic wls"
                     , search.density = 1, lrate.init = .5, nm.threshold = 5e-7
                     , save.res = TRUE)

nm.evaluation.runner(mode = "script", sep = ";", subdir = "nmr/dsn.2"
                     , eq.thr.type = "rel", eq.threshold = 1e-08
                     , algorithm = "direct search", nm.mode = "base", method = "basic wls"
                     , search.density = 1, lrate.init = .5, nm.threshold = 5e-7
                     , save.res = TRUE)

nm.evaluation.runner(mode = "script", sep = "tab", subdir = "nmr/dsn.3"
                     , eq.thr.type = "rel", eq.threshold = 1e-08
                     , algorithm = "direct search", nm.mode = "base", method = "basic wls"
                     , search.density = 1, lrate.init = .5, nm.threshold = 5e-7
                     , save.res = TRUE)

nm.evaluation.runner(mode = "script", sep = ",", subdir = "nmr/dsn.4/comma"
                     , eq.thr.type = "rel", eq.threshold = 1e-08
                     , algorithm = "direct search", nm.mode = "base", method = "basic wls"
                     , search.density = 1, lrate.init = .5, nm.threshold = 5e-7
                     , save.res = TRUE)

nm.evaluation.runner(mode = "script", sep = ";", subdir = "nmr/dsn.4/semicolon"
                     , eq.thr.type = "rel", eq.threshold = 1e-08
                     , algorithm = "direct search", nm.mode = "base", method = "basic wls"
                     , search.density = 1, lrate.init = .5, nm.threshold = 5e-7
                     , save.res = TRUE)

nm.evaluation.runner(mode = "script", sep = "tab", subdir = "nmr/dsn.4/tab"
                     , eq.thr.type = "rel", eq.threshold = 1e-08
                     , algorithm = "direct search", nm.mode = "base", method = "basic wls"
                     , search.density = 1, lrate.init = .5, nm.threshold = 5e-7
                     , save.res = TRUE)


#

nm.evaluation.runner(mode = "script", sep = "tab", subdir = "nmr/dsn.3"
                     , eq.thr.type = "rel", eq.threshold = 1e-08
                     , algorithm = "direct search", nm.mode = "base", method = "basic wls"
                     , search.density = 1, lrate.init = .5, nm.threshold = 1e-7
                     , save.res = FALSE)


#

source("app/kev/algo/calorimetry/ht_runner.r", chdir = TRUE)

rtrn <- ht.evaluation.runner(mode = "script", sep = "tab", subdir = "calorimetry/ds.4.overfilled"
                             , eq.thr.type = "rel", eq.threshold = 1e-08
                             , algorithm = "direct search", ht.mode = "base", method = "basic wls"
                             , search.density = 1, lrate.init = .5, ht.threshold = 5e-7
                             , filename = "data.xlsx"
                             )

rtrn












