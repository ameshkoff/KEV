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
library(MASS)
library(Matrix)
library(Hmisc)
# strings
library(stringi)
library(stringr)


# initialize and update helper variables

# load data --------------------------------- #

eq.scripts.load.csv <- function(sep, subdir, tbl) {
  
  fls <- list.files(subdir)
  
  cnst.fl <- paste0(subdir, fls[fls %like% "^(input_|output_)*(k_constants*_log10|constants*)(\\_evaluated)*(\\.csv|\\.txt)*$"][1])
  dt.coef.fl <- paste0(subdir, fls[fls %like% "^(input\\_)*stoich(iometric)*\\_coefficients(\\.csv|\\.txt)*"][1])
  dt.conc.fl <- paste0(subdir, fls[fls %like% "^(input\\_)*concentrations(\\.csv|\\.txt)*"][1])
  
  
  if (sep == ";") {
    
    tbl[["cnst"]] <- as.data.table(read.csv2(cnst.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
                                   , keep.rownames = FALSE)
    tbl[["dt.coef"]] <- as.data.table(read.csv2(dt.coef.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
                                      , keep.rownames = FALSE)
    tbl[["dt.conc"]] <- as.data.table(read.csv2(dt.conc.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE
                                                , skip = 1), keep.rownames = FALSE)
    tbl[["part.eq"]] <- as.data.table(read.csv2(dt.conc.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE
                                                , header = FALSE , nrows = 1), keep.rownames = FALSE)
    tbl[["part.eq"]][1, V1 := str_replace(V1, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0xbb), as.raw(0xbf)))), "")]
    
  } else if (sep == ",") {
    
    tbl[["cnst"]] <- as.data.table(read.csv(cnst.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
                                   , keep.rownames = FALSE)
    tbl[["dt.coef"]] <- as.data.table(read.csv(dt.coef.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
                                      , keep.rownames = FALSE)
    tbl[["dt.conc"]] <- as.data.table(read.csv(dt.conc.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE
                                               , skip = 1), keep.rownames = FALSE)
    tbl[["part.eq"]] <- as.data.table(read.csv(dt.conc.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE
                                               , header = FALSE , nrows = 1), keep.rownames = FALSE)
    tbl[["part.eq"]][1, V1 := str_replace(V1, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0xbb), as.raw(0xbf)))), "")]
    
  } else if (sep == "tab") {
    
    tbl[["cnst"]] <- as.data.table(read.delim(cnst.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
                                   , keep.rownames = FALSE)
    tbl[["dt.coef"]] <- as.data.table(read.delim(dt.coef.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
                                      , keep.rownames = FALSE)
    tbl[["dt.conc"]] <- as.data.table(read.delim(dt.conc.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE
                                                 , skip = 1), keep.rownames = FALSE)
    tbl[["part.eq"]] <- as.data.table(read.delim(dt.conc.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE
                                                 , header = FALSE , nrows = 1), keep.rownames = FALSE)
    tbl[["part.eq"]][1, V1 := str_replace(V1, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0xbb), as.raw(0xbf)))), "")]
    
  }
  
  # remove BOM mark if needed
  
  for (tb in tbl) {
    
    cln <- colnames(tb)
    setnames(tb, cln, str_replace(cln, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0xbb), as.raw(0xbf)))), ""))
    
  }
  
  tbl
  
}

eq.scripts.load.xlsx <- function(sep, subdir, filename, tbl) {
  
  fls <- list.files(subdir)

  # check if file exists
    
  if(length(fls[fls == filename]) > 0) {
    
    filename <- paste0(subdir, filename)
    
  } else {
    
    stop(paste("XLSX file", paste0(subdir, filename), "not found"))
    
  }

  # load data from XLSX, one dataset per sheet
  
  shts <- getSheetNames(filename)
  
  tbl[["cnst"]] <- read.xlsx(filename, sheet = sort(shts[shts %like% "^(input_|output_)*(k_constants*_log10|constants*)(\\_evaluated)*"])[1])
  tbl[["dt.coef"]] <- read.xlsx(filename, sheet = sort(shts[shts %like% "^(input_|output_)*stoich(iometric)*_coefficients*"])[1])
  tbl[["dt.conc"]] <- read.xlsx(filename, sheet = sort(shts[shts %like% "^(input_|output_)*concentrations*"])[1], startRow = 2)
  tbl[["part.eq"]] <- read.xlsx(filename, sheet = sort(shts[shts %like% "^(input_|output_)*concentrations*"])[1], colNames = FALSE, rows = 1)

  for (i in 1:length(tbl))
    tbl[[i]] <- as.data.table(tbl[[i]])

  # return
  
  tbl
  
}

eq.scripts.load <- function(sep = ";", subdir = "", filename = NULL) {
  
  tbl <- list("cnst" = NA, "dt.coef" = NA, "dt.conc" = NA, "part.eq" = NA)
  
  if (subdir != "")
    subdir <- paste0("/", subdir, "/")
  
  subdir <- paste0("input", subdir)
  
  if (is.null(filename)) {
    
    tbl <- eq.scripts.load.csv(sep, subdir, tbl)
    
  } else {
    
    tbl <- eq.scripts.load.xlsx(sep, subdir, filename, tbl)
    
  }
  
  tbl
  
}



