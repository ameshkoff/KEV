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


# ---------------------- test files structure -----------------

# read files structure

fl.stable <- list.files(path = "tests/data.gui/stable", recursive = TRUE)
fl.test <- list.files(path = "tests/data.gui/test", recursive = TRUE)

fl.missed <- setdiff(fl.stable, fl.test)
fl.missed <- fl.missed[!(fl.missed == "canary")]

# check if some files of the stable set are missed in the test one

if (length(fl.missed)) stop(paste("Missed stable files :", fl.missed))


# ------------------------- test files ------------------------

# functions

tst.test.xlsx <- function(fl) {
  
  fl.stable.cur <- paste0("tests/data.gui/stable/", fl)
  fl.test.cur <- paste0("tests/data.gui/test/", fl)
  
  fl.sheets <- getSheetNames(fl.stable.cur)
  
  for (sh in fl.sheets) {
    
    dt.stable <- read.xlsx(fl.stable.cur, sheet = sh, detectDates = FALSE) %>% as.data.table(keep.rownames = FALSE)
    dt.test <- read.xlsx(fl.test.cur, sheet = sh, detectDates = FALSE) %>% as.data.table(keep.rownames = FALSE)
    
    res <- all.equal(dt.test, dt.stable, check.attributes = FALSE)
    
    if (str_detect(fl, "^curves/") & str_detect(sh, "^output_(params*|area_under_curve)$")) {
      
      dt.stable <- dt.stable[, !c("name"), with = FALSE]
      dt.test <- dt.stable[, !c("name"), with = FALSE]

    }
    
    if (str_detect(fl, "^curves/")) res <- all.equal(dt.test, dt.stable, check.attributes = FALSE, tolerance = 1e-5)
    
    if (is.logical(res)) {
      
      print(paste(fl, sh, "OK", sep = " : "))
      
    } else {
      
      stop(paste(fl, sh, res, sep = " : "))
      
    }
    
  }
  
  return(0)
  
}


# run

fl.stable <- fl.stable[fl.stable != "canary"]

fl.stable <- fl.stable[!(fl.stable %like% "\\bdsl\\.5\\b")]

for (fl in fl.stable) {
  
  if (fl %like% "\\.zip$") {
    
    
    
    
  } else if (fl %like% "\\.xlsx$") {
    
    tst.test.xlsx(fl)
    
  } else {
    
    stop(paste("Unknown file type :", fl))
    
  }
  
  
}









