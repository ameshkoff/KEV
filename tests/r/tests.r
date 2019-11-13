# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #



# ---------------------- load libraries ----------------------

# I/O
library(openxlsx)
# data structure
library(data.table)
# computation
library(Hmisc)
# strings
library(stringi)
library(stringr)
# code
library(crayon)


# ----------------------- functions --------------------------

tst.test.worker <- function(fl, sh, dt.stable, dt.test, verbose) {
  
  # minor error warning
  wrn <- NULL
  
  # ad hoc fixes for different data sets
  
  if (str_detect(fl, "^curves/") && str_detect(sh, "^output_params*$") && length(colnames(dt.stable)[colnames(dt.stable) == "name"]) > 0) {
    
    dt.stable[, value := as.numeric(value)]
    dt.test[, value := as.numeric(value)]
    
    dt.stable <- dt.stable[, !c("name"), with = FALSE]
    dt.test <- dt.test[, !c("name"), with = FALSE]
    
  }
  
  if (str_detect(fl, "^curves/") && str_detect(sh, "^output_area_under_curve$") && length(colnames(dt.stable)[colnames(dt.stable) == "name"]) > 0) {
    
    dt.stable <- dt.stable[, !c("name"), with = FALSE]
    dt.test <- dt.test[, !c("name"), with = FALSE]
    
  }
  
  if (str_detect(sh, "^constants*_evaluated$")) {
    
    dt.stable[, Constant := as.numeric(Constant)]
    dt.stable[, St.Deviation := as.numeric(St.Deviation)]
    
    dt.test[, Constant := as.numeric(Constant)]
    dt.test[, St.Deviation := as.numeric(St.Deviation)]
    
  }
  
  # test itself
  
  if (str_detect(fl, "^curves/")) {
    
    res <- all.equal(dt.test, dt.stable, check.attributes = FALSE, tolerance = 1e-5)
    
  } else {
    
    res <- all.equal(dt.test, dt.stable, check.attributes = FALSE)  
    
  }
  
  # test postproc
  
  if (is.logical(res)) {
    
    if (verbose) print(paste(fl, sh, "OK", sep = " : "))
    
  } else if (str_detect(sh, "^constants*_evaluated$") && res %like% "\\bSt.Deviation\\b.*Mean relative difference"
             && as.numeric(str_extract(res, "[0-9e\\-\\+\\.]+$")) < 1e-4) {
    
    wrn <- paste("WARNING", fl, sh, res, sep = " : ")
    
    if (verbose) paste0(wrn, "\n") %>% crayon::red() %>% cat()
    
  } else {
    
    stop(paste(fl, sh, res, sep = " : "))
    
  }
  
  # return warning (if exists)
  
  wrn
  
}

tst.test.xlsx <- function(fl, verbose = FALSE) {
  
  fl.stable.cur <- paste0("tests/data.gui/stable/", fl)
  fl.test.cur <- paste0("tests/data.gui/test/", fl)
  
  fl.sheets <- getSheetNames(fl.stable.cur)
  
  tst.warnings <- c()
  
  for (sh in fl.sheets) {
    
    dt.stable <- read.xlsx(fl.stable.cur, sheet = sh, detectDates = FALSE) %>% as.data.table(keep.rownames = FALSE)
    dt.test <- read.xlsx(fl.test.cur, sheet = sh, detectDates = FALSE) %>% as.data.table(keep.rownames = FALSE)
    
    tst.warnings <- c(tst.warnings, tst.test.worker(fl, sh, dt.stable, dt.test, verbose))
    
  }
  
  return(tst.warnings)
  
}

tst.test.gui <- function(verbose = FALSE) {
  
  # test files structure
  
  fl.stable <- list.files(path = "tests/data.gui/stable", recursive = TRUE)
  fl.test <- list.files(path = "tests/data.gui/test", recursive = TRUE)
  
  fl.missed <- setdiff(fl.stable, fl.test)
  if (length(fl.missed)) stop(paste("Missed stable files :", fl.missed))

  # test files (GUI outputs)
  
  tst.warnings <- c()
  
  for (fl in fl.stable) {
    
    if (fl %like% "\\.zip$") {
      
      
      
      
    } else if (fl %like% "\\.xlsx$") {
      
      tst.warnings <- c(tst.warnings, tst.test.xlsx(fl, verbose))
      
    } else {
      
      stop(paste("Unknown file type :", fl))
      
    }
    
  }
  
  # print results
  
  paste("Tests succeded. Minor errors :", length(tst.warnings), "\n") %>% red %>% cat()
  if (length(tst.warnings) > 0) print(tst.warnings)
  
  return(0)  
  
}

# -------------------------- run -----------------------------

tst.test.gui(FALSE)








