# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2020                                                 #
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


# --------------------- load libraries -----------------------

library(testthat)

test.dict <- read.delim("tests/dict.csv", stringsAsFactors = FALSE) %>% as.data.table()




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
  test_file("tests/tests/ht_formal.r", env = env, reporter = c("progress", "fail"))
  
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
    test_file("tests/tests/ht_stat.r", env = env, reporter = c("progress", "fail"))
  } else {
    test_file("tests/tests/ht_stat.r", env = env, reporter = "progress")
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
  test_file("tests/tests/ht_consistent.r", env = env, reporter = c("progress", "fail"))
  
}


# regression tests

kev.test.regression.test <- function(fl, sh, dt.stable, dt.test, verbose) {
  
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

kev.test.loop.zip <- function(fl
                             , data.path = "tests/data.gui"
                             , verbose = FALSE) {

  fl.stable.cur <- str_replace_all(paste(data.path, "stable", fl, sep = "/"), "\\/\\/", "/")
  fl.test.cur <- str_replace_all(paste(data.path, "test", fl, sep = "/"), "\\/\\/", "/")
  
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
    
    tst.warnings <- c(tst.warnings, kev.test.regression.test(fl, sh, dt.stable, dt.test, verbose))
    
  }
  
  unlink(c(fl.stable.cur.dir, fl.test.cur.dir), recursive = TRUE)
  return(tst.warnings)

}

kev.test.loop.dir <- function(fl
                              , data.path = "tests/data.gui"
                              , verbose = FALSE) {
  
  fl.stable.cur <- str_replace_all(paste(data.path, "stable", fl, sep = "/"), "\\/\\/", "/")
  fl.test.cur <- str_replace_all(paste(data.path, "test", fl, sep = "/"), "\\/\\/", "/")
  
  fl.sheets <- list.files(fl.stable.cur, full.names = FALSE)
  
  tst.warnings <- c()
  
  for (sh in fl.sheets) {
    
    if (fl.stable.cur %like% "(\\b|\\_)comma[0-9]*\\b") {
      
      dt.stable <- read.csv(paste0(fl.stable.cur, "/", sh), stringsAsFactors = FALSE, check.names = FALSE) %>% as.data.table(keep.rownames = FALSE)
      dt.test <- read.csv(paste0(fl.test.cur, "/", sh), stringsAsFactors = FALSE, check.names = FALSE) %>% as.data.table(keep.rownames = FALSE)
      
    } else if (fl.stable.cur %like% "(\\b|\\_)semicolon[0-9]*\\b") {
      
      dt.stable <- read.csv2(paste0(fl.stable.cur, "/", sh), stringsAsFactors = FALSE, check.names = FALSE) %>% as.data.table(keep.rownames = FALSE)
      dt.test <- read.csv2(paste0(fl.test.cur, "/", sh), stringsAsFactors = FALSE, check.names = FALSE) %>% as.data.table(keep.rownames = FALSE)
      
    } else if (fl.stable.cur %like% "(\\b|\\_)tab[0-9]*\\b") {
      
      dt.stable <- read.delim(paste0(fl.stable.cur, "/", sh), stringsAsFactors = FALSE, check.names = FALSE) %>% as.data.table(keep.rownames = FALSE)
      dt.test <- read.delim(paste0(fl.test.cur, "/", sh), stringsAsFactors = FALSE, check.names = FALSE) %>% as.data.table(keep.rownames = FALSE)
      
    } else {
      
      unlink(c(fl.stable.cur, fl.test.cur), recursive = TRUE)
      stop(paste("File format not defined", fl.stable.cur, sh, sep = " : "))
      
    }
    
    tst.warnings <- c(tst.warnings, kev.test.regression.test(fl, sh, dt.stable, dt.test, verbose))
    
  }
  
  return(tst.warnings)
  
}

kev.test.loop.xlsx <- function(fl
                              , data.path = "tests/data.gui"
                              , verbose = FALSE) {
  
  fl.stable.cur <- str_replace_all(paste(data.path, "stable", fl, sep = "/"), "\\/\\/", "/")
  fl.test.cur <- str_replace_all(paste(data.path, "test", fl, sep = "/"), "\\/\\/", "/")

  fl.sheets <- getSheetNames(fl.stable.cur)
  
  tst.warnings <- c()
  
  for (sh in fl.sheets) {
    
    dt.stable <- read.xlsx(fl.stable.cur, sheet = sh, detectDates = FALSE) %>% as.data.table(keep.rownames = FALSE)
    dt.test <- read.xlsx(fl.test.cur, sheet = sh, detectDates = FALSE) %>% as.data.table(keep.rownames = FALSE)
    
    tst.warnings <- c(tst.warnings, kev.test.regression.test(fl, sh, dt.stable, dt.test, verbose))
    
  }
  
  return(tst.warnings)
  
}

kev.test.regression <- function(data.path = "tests/data.gui"
                        , verbose = FALSE) {
  
  # test files structure
  
  fl.stable <- list.files(path = str_replace_all(paste(data.path, "stable", sep = "/"), "\\/\\/", "/"), recursive = TRUE)
  fl.test <- list.files(path = str_replace_all(paste(data.path, "stable", sep = "/"), "\\/\\/", "/"), recursive = TRUE)
  
  fl.stable[fl.stable %like% "\\.(csv|txt)$"] <- paste0(dirname(fl.stable[fl.stable %like% "\\.(csv|txt)$"]), "/")
  fl.test[fl.test %like% "\\.(csv|txt)$"] <- paste0(dirname(fl.test[fl.test %like% "\\.(csv|txt)$"]), "/")
  
  fl.stable <- unique(fl.stable)
  fl.test <- unique(fl.test)
  
  fl.missed <- setdiff(fl.stable, fl.test)
  if (length(fl.missed)) stop(paste("Missed stable files :", fl.missed))
  
  # test files (GUI outputs)
  
  tst.warnings <- c()
  
  for (fl in fl.stable) {
    
    if (fl %like% "\\.zip$") {
      
      tst.warnings <- c(tst.warnings, kev.test.loop.zip(fl, data.path, verbose))
      
    } else if (fl %like% "\\.xlsx$") {
      
      tst.warnings <- c(tst.warnings, kev.test.loop.xlsx(fl, data.path, verbose))
      
    } else if (fl %like% "\\/$") {
      
      tst.warnings <- c(tst.warnings, kev.test.loop.dir(fl, data.path, verbose))
      
    } else {
      
      stop(paste("Unknown file type :", fl))
      
    }
    
  }
  
  # print results
  
  paste("Tests succeeded. Minor errors :", length(tst.warnings), "\n") %>% red %>% cat()
  if (length(tst.warnings) > 0) print(tst.warnings)
  
  return(0)  
  
}


kev.test.regression.write.data <- function(dt.test.list = list(data.table(fake = character(0)))
                                           , path = ""
                                           , write.fn = function(){1}) {
  
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
    
    write.fn(dt.ttl = dt.test.list[[nm]]
              , path = str_replace_all(paste(path, path.dt, sep = "/"), "\\/\\/", "/")
              , sep = sep.v
              , filename = filename)

    print(paste(nm, "saved"))
    
  })
  
  invisible(0)
  
}

# kev.test.regression <- function() {}






