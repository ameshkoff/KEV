# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #



# ---------------------- load libraries ----------------------

# I/O
# data structure
library(data.table)
# computation
library(MASS)
library(Matrix)
library(Hmisc)
# strings
library(stringi)
library(stringr)



# runner -------------------------------------------- #

sp.evaluation.runner <- function(mode = c("api", "script", "app")
                                 , sep = "tab"
                                 , subdir = ""
                                 , dt.list = NULL
                                 , save.res = TRUE) {
  
  # source code ------------- #
  
  dir.start <- ""
  
  if (mode %in% c("script", "api")) {
    
    dir.start <- "app/spectrophotometry/algo/"
    
  } else {
    
    dir.start <- "algo/"
    
  }
  
  source(paste0(dir.start, "molar.extinction.coefficients/sp_data.r"), chdir = TRUE)
  source(paste0(dir.start, "molar.extinction.coefficients/sp_preproc.r"), chdir = TRUE)
  source(paste0(dir.start, "molar.extinction.coefficients/sp_evaluator.r"), chdir = TRUE)
  source(paste0(dir.start, "molar.extinction.coefficients/sp_postproc.r"), chdir = TRUE)
  source(paste0(dir.start, "molar.extinction.coefficients/sp_save.r"), chdir = TRUE)
  
  
  # load data ---------------- #
  
  if (mode[1] == "script") {
    
    dt.sp <- sp.scripts.load(sep, subdir)
    
  } else if (mode[1] %in% c("app", "api")) {
    
    dt.sp <- dt.list
    
  }
  
  
  # preproc data --------------- #
  
  dt.sp <- sp.preproc(dt.sp)
  
  
  # run evaluator --------------- #
  
  system.time(dt.ttl <- spectra.mol.ext.evaluator(dt.sp))
  
  
  # postprocessing ---------------- #
  
  dt.mol.full <- sp.postproc(dt.ttl)
  
  
  # save -------------------------- #
  
  if (mode == "script" & save.res) {
    
    sp.save(subdir, sep, dt.mol.full)
    
  }
  
  # return
  
  dt.mol.full

}










