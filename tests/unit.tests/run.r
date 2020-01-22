# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2020                                                 #
#                                                            #
# ########################################################## #



#----------------- load libraries & data ----------------------

source("app/kev/algo/calorimetry/ht_runner.r", chdir = TRUE)
test.dict <- read.delim("tests/unit.tests/dict.csv", stringsAsFactors = FALSE) %>% as.data.table()


# ------------------------ functions --------------------------

kev.test.run <- function(target.dir
                         , run.fn = function(){1}
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
    
    rtrn <<- run.fn(dr
                   , sep = test.dict[dir == basename(dr), sep]
                   , filename = NULL)
    print(dr)
    test_file("tests/unit.tests/tests/tests.r")
    rtrn <<- NULL
    
  }
  
  0

}

test.ht.run <- function(dr, sep, filename) {
  
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


# ------------------------ calorimetry --------------------------

kev.test.run(target.dir = "input/calorimetry"
             , run.fn = test.ht.run
             , ignore.pattern = "")








# test_file("tests/unit.tests/tests/tests.r")






