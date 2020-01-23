# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2020                                                 #
#                                                            #
# ########################################################## #



#----------------- load libraries & data ----------------------

library(testthat)

#

source("app/kev/algo/calorimetry/ht_runner.r", chdir = TRUE)
test.dict <- read.delim("tests/unit.tests/dict.csv", stringsAsFactors = FALSE) %>% as.data.table()


# ------------------------ functions --------------------------

kev.test.run <- function(target.dir
                         , getdata.fn = function(){1}
                         , test.formal.fn = function(){1}
                         , ignore.pattern = "") {

  fls <- list.files(target.dir, pattern = "\\.xlsx$", full.names = TRUE, recursive = TRUE)
  fls <- fls[!(fls %like% "^\\~\\.lock\\.|^\\~\\$")]
  fls <- str_remove(fls, "^(.*/kev/)*input/")
  
  drs <- list.dirs(target.dir, full.names = TRUE, recursive = TRUE)
  drs <- drs[basename(drs) %in% test.dict[, dir]]
  drs <- str_remove(drs, "^(.*/kev/)*input/")
  
  if (ignore.pattern != "") {
    
    fls <- fls[!(fls %like% ignore.pattern)]
    drs <- drs[!(drs %like% ignore.pattern)]
    
  }
  
  for (dr in drs) {
    
    rtrn <<- getdata.fn(dr
                   , sep = test.dict[dir == basename(dr), sep]
                   , filename = NULL)
    dt.test.list[[dr]] <<- rtrn
    kev.context <<- dr

    test.formal.fn()
    
    rtrn <<- NULL
    kev.context <<- NULL
    
  }
  
  0

}

ht.test.getdata <- function(dr, sep, filename) {
  
  rtrn <- ht.evaluation.runner(mode = "script"
                               , sep = sep
                               , subdir = dr
                               , eq.thr.type = "rel"
                               , eq.threshold = 1e-08
                               , algorithm = "direct search"
                               , ht.mode = "base"
                               , method = "basic wls"
                               , search.density = 1
                               , lrate.init = .5
                               , ht.threshold = 5e-7
                               , filename = filename)
                               
  rtrn

}

ht.test.formal <- function() { test_file("tests/unit.tests/tests/ht_tests.r") }




# ------------------------ calorimetry --------------------------

dt.test.list <- list()

kev.test.run(target.dir = "input/calorimetry"
             , getdata.fn = ht.test.getdata
             , test.formal.fn = ht.test.formal
             , ignore.pattern = "")









