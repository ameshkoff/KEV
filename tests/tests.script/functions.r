# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2020                                                 #
#                                                            #
# ########################################################## #



# --------------------- load libraries -----------------------

library(testthat)

test.dict <- read.delim("tests/tests.script/dict.csv", stringsAsFactors = FALSE) %>% as.data.table()




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
                            , test.fn = function(){1}) {
  
  kev.test.env <- new.env(parent = parent.frame())
  
  for (dt.name in names(dt.test.list)) {
    
    assign("rtrn"
           , dt.test.list[[dt.name]]
           , envir = kev.test.env)
    assign("kev.context", paste("Formal :", dt.name), envir = kev.test.env)
    
    test.fn(kev.test.env)
    
    assign("rtrn", NULL, envir = kev.test.env)
    assign("kev.context", NULL, envir = kev.test.env)
    
  }
  
}

ht.test.formal <- function(env) {
  
  cat("\n")
  test_file("tests/tests.script/tests/ht_formal.r", env = env, reporter = c("progress", "fail"))
  
}


# statistics tests

kev.test.stat <- function(dt.test.list = list(data.table(fake = character(0)))
                          , test.fn = function(){1}
                          , stop.on.fail = TRUE) {
  
  kev.test.env <- new.env(parent = parent.frame())
  
  for (dt.name in names(dt.test.list)) {
    
    assign("rtrn"
           , dt.test.list[[dt.name]]
           , envir = kev.test.env)
    assign("kev.context", paste("Stat :", dt.name), envir = kev.test.env)
    
    test.fn(kev.test.env, stop.on.fail)
    
    assign("rtrn", NULL, envir = kev.test.env)
    assign("kev.context", NULL, envir = kev.test.env)
    
  }
  
}

ht.test.stat <- function(env, stop.on.fail) {
  
  cat("\n")
  
  if (stop.on.fail) {
    test_file("tests/tests.script/tests/ht_stat.r", env = env, reporter = c("progress", "fail"))
  } else {
    test_file("tests/tests.script/tests/ht_stat.r", env = env, reporter = "progress")
  }
  
  
}



# consistency tests

kev.test.consistent <- function(dt.test.list = list(data.table(fake = character(0)))
                                , test.fn = function(){1}) {
  
  dt.test.list <- kev.test.consistent.data(dt.test.list)
  
  dt.names <- data.table(dt.name = names(dt.test.list))
  dt.names[, dt.group := dirname(dt.name)]
  
  dt.names.unique <- dt.names[, dt.group] %>% unique()
  
  kev.test.env <- new.env(parent = parent.frame())
  
  for (dt.nm in dt.names.unique) {
    
    assign("dt.test.list"
           , dt.test.list[names(dt.test.list) %in% dt.names[dt.group == dt.nm, dt.name]]
           , envir = kev.test.env)
    assign("kev.context", paste("Consistence :", dt.nm), envir = kev.test.env)
    
    test.fn(kev.test.env)
    
    assign("dt.test.list", NULL, envir = kev.test.env)
    assign("kev.context", NULL, envir = kev.test.env)
    
  }
  
}

kev.test.consistent.data <- function(dt.test.list) {
  
  # convert to numeric if possible and remove vector names
  
  dt.test.list <- lapply(dt.test.list, function(x) lapply(x, function(x) {
    
    if (is.vector(x)) {
      
      x <- unname(x)
      
      if (is.character(x)) {
        
        x <- str_replace(x, "\\,", ".")
        x <- str_replace(x, " ", "")
        
        if (length(x[!is.na(as.numeric(x))]) == length(x[!is.na(x)])) x <- as.numeric(x)
        
      }
      
    } else if (is.data.table(x)) {
      
      cln <- colnames(x)
      
      for (cl in cln) {
        
        if (is.character(x[, eval(as.name(cl))])) {
          
          x[, eval(cl) := str_replace(eval(as.name(cl)), "\\,", ".")]
          x[, eval(cl) := str_replace(eval(as.name(cl)), " ", "")]
          
          if (nrow(x[!is.na(eval(as.name(cl)))]) == nrow(x[!is.na(as.numeric(eval(as.name(cl))))])) x[, eval(cl) := as.numeric(eval(as.name(cl)))]
          
        }
        
      }
      
    }
    
    x
    
  }))
  
  dt.test.list
  
}

ht.test.consistent <- function(env) {
  
  cat("\n")
  test_file("tests/tests.script/tests/ht_consistent.r", env = env, reporter = c("progress", "fail"))
  
}



# regression tests

kev.test.regression.write.data <- function(dt.test.list = list(data.table(fake = character(0))), path = "") {
  
  source("app/kev/algo/calorimetry/ht_save.r", chdir = TRUE, local = TRUE)
  
  lapply(names(dt.test.list), function(nm) {
    
    path.dt <- nm
    sep.v <- basename(nm)
    filename <- NULL
    
    if (!(sep.v %like% "^(csv|txt)")) {
      
      filename <- sep.v
      path.dt <- dirname(nm)
      sep.v <- "csv.comma"
      
    }
    
    sep.v <- test.dict[dir == sep.v, sep]
    
    ht.save(dt.ttl = dt.test.list[[nm]]
              , path = str_replace_all(paste(path, path.dt, sep = "/"), "\\/\\/", "/")
              , sep = sep.v
              , filename = filename)

    print(paste(nm, "saved"))
    
  })
  
  0
  
}

kev.test.regression.read.data <- function(path = "") {}

kev.test.regression <- function() {}




