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
library(MASS)
library(Matrix)
library(Hmisc)
# strings
library(stringi)
library(stringr)


# initialize and update helper variables

# load data --------------------------------- #

ht.scripts.load.csv <- function(sep, subdir, tbl) {

  fls <- list.files(subdir)
    
  dt.heat.fl <- paste0(subdir, fls[fls %like% "^(input\\_)*heats*(\\.csv|\\.txt)*"][1])
  dt.enth.fl <- paste0(subdir, fls[fls %like% "^(input\\_)*enth*alp(y|ie)s*(\\.csv|\\.txt)*"][1])
  setup.fl <- paste0(subdir, fls[fls %like% "^(input\\_)*(targets*|constants*\\_names*|setup)(\\.csv|\\.txt)*"][1])
  
  if (sep == ";") {
    
    tbl[["dt.heat"]] <- as.data.table(read.csv2(dt.heat.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
                                    , keep.rownames = FALSE)
    if (file.size(dt.enth.fl) > 0)
      tbl[["dt.enth"]] <- as.data.table(read.csv2(dt.enth.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
                                       , keep.rownames = FALSE)
    tbl[["setup"]] <- as.data.table(read.delim(setup.fl, stringsAsFactors = FALSE, colClasses = "character"
                                             , check.names = FALSE, sep = sep, dec = ",", header = FALSE)
                                  , keep.rownames = FALSE)
    
    
  } else if (sep == ",") {
    
    tbl[["dt.heat"]] <- as.data.table(read.csv(dt.heat.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
                                    , keep.rownames = FALSE)
    if (file.size(dt.enth.fl) > 0)
      tbl[["dt.enth"]] <- as.data.table(read.csv(dt.enth.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
                                       , keep.rownames = FALSE)
    tbl[["setup"]] <- as.data.table(read.delim(setup.fl, stringsAsFactors = FALSE, colClasses = "character"
                                               , check.names = FALSE, sep = sep, dec = ".", header = FALSE)
                                    , keep.rownames = FALSE)
    
  } else if (sep == "tab") {
    
    tbl[["dt.heat"]] <- as.data.table(read.delim(dt.heat.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
                                    , keep.rownames = FALSE)
    if (file.size(dt.enth.fl) > 0)
      tbl[["dt.enth"]] <- as.data.table(read.delim(dt.enth.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
                                       , keep.rownames = FALSE)
    tbl[["setup"]] <- as.data.table(read.delim(setup.fl, stringsAsFactors = FALSE, colClasses = "character"
                                               , check.names = FALSE, sep = "\t", dec = ".", header = FALSE)
                                    , keep.rownames = FALSE)
    
  }
  
  # remove BOM mark if needed
  
  for (tb in tbl) {
    
    if (is.data.table(tb)) {
      
      cln <- colnames(tb)
      setnames(tb, cln, str_replace(cln, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0xbb), as.raw(0xbf)))), ""))
      
    }
    
  }
  
  tbl
  
}

ht.scripts.load.xlsx <- function(sep, subdir, filename, tbl) {
  
  fls <- list.files(subdir)
  
  # check if file exists
  
  if(length(fls[fls == filename]) > 0) {
    
    filename <- paste0(subdir, filename)
    
  } else {
    
    stop(paste("XLSX file", paste0(subdir, filename), "not found"))
    
  }
  
  # load data from XLSX, one dataset per sheet
  
  shts <- getSheetNames(filename)
  
  tbl[["dt.heat"]] <- read.xlsx(filename, sheet = sort(shts[shts %like% "^(input\\_)*heats*"])[1])
  tbl[["dt.enth"]] <- read.xlsx(filename, sheet = sort(shts[shts %like% "^(input\\_)*enth*alp(y|ie)s*"])[1])
  tbl[["setup"]] <- read.xlsx(filename, sheet = sort(shts[shts %like% "^(input\\_)*(targets*|constants*\\_names*|setup)"])[1], colNames = FALSE)
  
  for (i in 1:length(tbl))
    tbl[[i]] <- as.data.table(tbl[[i]])
  
  # return
  
  tbl
  
}

ht.load.extract.setup <- function(tbl = list(dt = data.table())) {
  
  if (is.data.table(tbl[["setup"]]) && nrow(tbl[["setup"]]) > 0) {
    
    cln <- colnames(tbl[["setup"]])
    
    if (length(cln[cln == "V1"]) > 0) {
      
      tbl[["setup"]][1, V1 := str_replace(V1, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0x2e), as.raw(0xbf)))), "")]
      setnames(tbl[["setup"]], "V1", "X1")
      
    }
    
    tbl[["setup"]][, X1 := str_to_lower(str_trim(X1))]
    
    if (nrow(tbl[["setup"]][X1 %like% "^constants*$"]) > 0) {
      
      tbl[["cnst.tune"]] <- tbl[["setup"]][X1 %like% "^constants*$"][, !"X1", with = FALSE]
      tbl[["cnst.tune"]] <- unlist(tbl[["cnst.tune"]])
      tbl[["cnst.tune"]] <- tbl[["cnst.tune"]][!is.na(tbl[["cnst.tune"]]) & tbl[["cnst.tune"]] != ""]
      
      tbl[["cnst.tune"]] <- unlist(tbl[["cnst.tune"]])
      
    }
    
    if (nrow(tbl[["setup"]][X1 %like% "^(components*|cell|component *\\(cell\\))$"]) > 0) {
      
      tbl[["cmp.tune"]] <- tbl[["setup"]][X1 %like% "^(components*|cell|component *\\(cell\\))$"][, !"X1", with = FALSE]
      tbl[["cmp.tune"]] <- unlist(tbl[["cmp.tune"]])
      tbl[["cmp.tune"]] <- tbl[["cmp.tune"]][!is.na(tbl[["cmp.tune"]]) & tbl[["cmp.tune"]] != ""]
      
      tbl[["cmp.tune"]] <- unlist(tbl[["cmp.tune"]])[1]
      
    }
    
    if (nrow(tbl[["setup"]][X1 %like% "^calorimeter$"]) > 0) {
      
      tbl[["calorimeter.type"]] <- tbl[["setup"]][X1 %like% "^calorimeter*$"][, !"X1", with = FALSE]
      tbl[["calorimeter.type"]] <- unlist(tbl[["calorimeter.type"]])
      tbl[["calorimeter.type"]] <- tbl[["calorimeter.type"]][!is.na(tbl[["calorimeter.type"]]) & tbl[["calorimeter.type"]] != ""]
      
      tbl[["calorimeter.type"]] <- unlist(tbl[["calorimeter.type"]])[1]
      
    }
    
    if (nrow(tbl[["setup"]][X1 %like% "^(initial|active*).*volumes*$"]) > 0) {
      
      tbl[["init.vol"]] <- tbl[["setup"]][X1 %like% "^(initial|active*).*volumes*$"][, !"X1", with = FALSE]
      tbl[["init.vol"]] <- unlist(tbl[["init.vol"]])
      tbl[["init.vol"]] <- tbl[["init.vol"]][!is.na(tbl[["init.vol"]]) & tbl[["init.vol"]] != ""]
      
      tbl[["init.vol"]] <- unlist(tbl[["init.vol"]])[1]
      
    }

  }

  # return
  
  tbl[!(names(tbl) %in% c("setup"))]
  
}

ht.scripts.load <- function(sep = ";", subdir = "", filename = NULL) {
  
  tbl <- list("dt.heat" = NA, "dt.enth" = NA, "setup" = NA)
  
  if (subdir != "")
    subdir <- paste0("/", subdir, "/")
  
  subdir <- paste0("input", subdir)
  
  if (is.null(filename)) {
    
    tbl <- ht.scripts.load.csv(sep, subdir, tbl)
    
  } else {
    
    tbl <- ht.scripts.load.xlsx(sep, subdir, filename, tbl)
    
  }
  
  # extract data from targets / setup sheet
  
  tbl <- ht.load.extract.setup(tbl)
  
  # return
  
  tbl
  
}



