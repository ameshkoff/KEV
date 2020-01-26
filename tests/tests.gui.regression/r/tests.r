# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
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
  
  if (str_detect(fl, "^curves/") && str_detect(sh, "^output_params*(\\.(txt|csv))*$") && length(colnames(dt.stable)[colnames(dt.stable) == "name"]) > 0) {
    
    dt.stable[, value := as.numeric(value)]
    dt.test[, value := as.numeric(value)]
    
    dt.stable <- dt.stable[, !c("name"), with = FALSE]
    dt.test <- dt.test[, !c("name"), with = FALSE]
    
  }
  
  if (str_detect(fl, "^curves/") && str_detect(sh, "^output_area_under_curve(\\.(txt|csv))*$") && length(colnames(dt.stable)[colnames(dt.stable) == "name"]) > 0) {
    
    dt.stable <- dt.stable[, !c("name"), with = FALSE]
    dt.test <- dt.test[, !c("name"), with = FALSE]
    
  }
  
  if (str_detect(sh, "^constants*_evaluated(\\.(txt|csv))*$")) {
    
    dt.stable[, Constant := as.numeric(Constant)]
    dt.stable[, St.Deviation := as.numeric(St.Deviation)]
    
    dt.test[, Constant := as.numeric(Constant)]
    dt.test[, St.Deviation := as.numeric(St.Deviation)]
    
  }
  
  # test itself
  
  if (str_detect(fl, "^curves/")) {
    
    res <- all.equal(dt.test, dt.stable, check.attributes = FALSE, tolerance = 1e-4)
    
  } else {
    
    res <- all.equal(dt.test, dt.stable, check.attributes = FALSE)  
    
  }
  
  # test postproc
  
  if (is.logical(res)) {
    
    if (verbose) print(paste(fl, sh, "OK", sep = " : "))
    
  } else if (str_detect(sh, "^constants*_evaluated(\\.(txt|csv))*$") && res %like% "\\bSt.Deviation\\b.*Mean relative difference"
             && as.numeric(str_extract(res, "[0-9e\\-\\+\\.]+$")) < 1e-4) {
    
    wrn <- paste("WARNING", fl, sh, res, sep = " : ")
    
    if (verbose) paste0(wrn, "\n") %>% crayon::red() %>% cat()
    
  } else {
    
    stop(paste(fl, sh, res, sep = " : "))
    
  }
  
  # return warning (if exists)
  
  wrn
  
}

tst.test.zip <- function(fl, verbose = FALSE) {
  
  fl.stable.cur <- paste0("tests/tests.gui.regression/data/stable/", fl)
  fl.test.cur <- paste0("tests/tests.gui.regression/data/test/", fl)
  
  fl.stable.cur.dir <- str_remove(fl.stable.cur, "\\.zip$")
  fl.test.cur.dir <- str_remove(fl.test.cur, "\\.zip$")
  
  unzip(fl.stable.cur, exdir = fl.stable.cur.dir)
  unzip(fl.test.cur, exdir = fl.test.cur.dir)

  fl.sheets <- list.files(fl.stable.cur.dir, full.names = FALSE)
  
  tst.warnings <- c()
  
  for (sh in fl.sheets) {
    
    if (fl.stable.cur.dir %like% "(\\b|\\_)comma[0-9]*\\b") {
      
      dt.stable <- read.csv(paste0(fl.stable.cur.dir, "/", sh), stringsAsFactors = FALSE, check.names = FALSE) %>% as.data.table(keep.rownames = FALSE)
      dt.test <- read.csv(paste0(fl.test.cur.dir, "/", sh), stringsAsFactors = FALSE, check.names = FALSE) %>% as.data.table(keep.rownames = FALSE)
      
    } else if (fl.stable.cur.dir %like% "(\\b|\\_)semicolon[0-9]*\\b") {
      
      dt.stable <- read.csv2(paste0(fl.stable.cur.dir, "/", sh), stringsAsFactors = FALSE, check.names = FALSE) %>% as.data.table(keep.rownames = FALSE)
      dt.test <- read.csv2(paste0(fl.test.cur.dir, "/", sh), stringsAsFactors = FALSE, check.names = FALSE) %>% as.data.table(keep.rownames = FALSE)
      
    } else if (fl.stable.cur.dir %like% "(\\b|\\_)tab[0-9]*\\b") {
      
      dt.stable <- read.delim(paste0(fl.stable.cur.dir, "/", sh), stringsAsFactors = FALSE, check.names = FALSE) %>% as.data.table(keep.rownames = FALSE)
      dt.test <- read.delim(paste0(fl.test.cur.dir, "/", sh), stringsAsFactors = FALSE, check.names = FALSE) %>% as.data.table(keep.rownames = FALSE)
      
    } else {
      
      unlink(c(fl.stable.cur.dir, fl.test.cur.dir), recursive = TRUE)
      stop(paste("File format not defined", fl.stable.cur.dir, sh, sep = " : "))
      
    }

    tst.warnings <- c(tst.warnings, tst.test.worker(fl, sh, dt.stable, dt.test, verbose))
    
  }
  
  unlink(c(fl.stable.cur.dir, fl.test.cur.dir), recursive = TRUE)
  return(tst.warnings)
  
}

tst.test.xlsx <- function(fl, verbose = FALSE) {
  
  fl.stable.cur <- paste0("tests/tests.gui.regression/data/stable/", fl)
  fl.test.cur <- paste0("tests/tests.gui.regression/data/test/", fl)
  
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
  
  fl.stable <- list.files(path = "tests/tests.gui.regression/data/stable", recursive = TRUE)
  fl.test <- list.files(path = "tests/tests.gui.regression/data/test", recursive = TRUE)
  
  fl.missed <- setdiff(fl.stable, fl.test)
  if (length(fl.missed)) stop(paste("Missed stable files :", fl.missed))

  # test files (GUI outputs)
  
  tst.warnings <- c()
  
  for (fl in fl.stable) {
    
    if (fl %like% "\\.zip$") {
      
      tst.warnings <- c(tst.warnings, tst.test.zip(fl, verbose))
      
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

tst.test.gui(TRUE)








