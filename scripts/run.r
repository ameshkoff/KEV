source("scripts/eq_concentrations.r", chdir = TRUE)

eq.conc.exec(sep = ";", subdir = "ds.2p", bs.name = "molecule2", verbose = TRUE)
eq.conc.exec(sep = ";", subdir = "ds.3p", bs.name = "molecule2", verbose = TRUE)
eq.conc.exec(sep = ";", subdir = "ds.5p", bs.name = "molecule4", verbose = TRUE)
eq.conc.exec(sep = ";", subdir = "ds.eq", bs.name = "molecule3", verbose = TRUE)