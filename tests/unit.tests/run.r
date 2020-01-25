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

# get output data to test

kev.test.getdata.all <- function(target.dir
                                 , getdata.fn = function(){1}
                                 , ignore.pattern = "") {

  # list files and dirs to load
  
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
  
  dt.test.list <- list()
  
  # load plain files
  
  for (dr in drs) {
    
    dt.test.list[[dr]] <- getdata.fn(dr
                                     , sep = test.dict[dir == basename(dr), sep]
                                     , filename = NULL)
    print(paste(dr, "loaded"))

  }

  # load xlsx
  
  for (fl in fls) {
    
    dt.test.list[[fl]] <- getdata.fn(dirname(fl)
                                     , sep = test.dict[dir == basename(fl), sep]
                                     , filename = basename(fl))
    print(paste(fl, "loaded"))
    
  }
  
  # return
  
  dt.test.list

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

# formal tests

kev.test.formal <- function(dt.test.list = list(data.table(fake = character(0)))
                           , test.formal.fn = function(){1}) {
  
  kev.test.env <- new.env(parent = parent.frame())
  
  for (dt.name in names(dt.test.list)) {
    
    assign("rtrn"
           , dt.test.list[[dt.name]]
           , envir = kev.test.env)
    assign("kev.context", dt.name, envir = kev.test.env)
    
    test.formal.fn(kev.test.env)
    
    assign("rtrn", NULL, envir = kev.test.env)
    assign("kev.context", NULL, envir = kev.test.env)

  }
  
}

ht.test.formal <- function(env) { test_file("tests/unit.tests/tests/ht_formal.r", env = env) }




# ------------------------ calorimetry --------------------------

dt.test.list <- kev.test.getdata.all(target.dir = "input/calorimetry"
                                     , getdata.fn = ht.test.getdata
                                     , ignore.pattern = "(\\b|\\_)old\\b|DEPR")

kev.test.formal(dt.test.list = dt.test.list
                , test.formal.fn = ht.test.formal)







