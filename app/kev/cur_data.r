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
library(MASS)
library(Matrix)
library(Hmisc)
# strings
library(stringi)
library(stringr)


# initialize and update helper variables

# load data --------------------------------- #

cur.scripts.load.plain <- function(sep, subdir) {
  
  tbl <- list("dt.cur" = NA, "dt.par" = NA)
  
  if (subdir != "")
    subdir <- paste0("/", subdir, "/")
  
  subdir <- paste0("input", subdir)
  fls <- list.files(subdir)
  
  dt.cur.fl <- paste0(subdir, fls[fls %like% "^(input\\_)*data(\\.csv|\\.txt)*"][1])
  dt.par.fl <- paste0(subdir, fls[fls %like% "^(input\\_)*(param(eter)*s*)(\\.csv|\\.txt)*"][1])
  
  if (sep == ";") {
    
    tbl[["dt.cur"]] <- as.data.table(read.csv2(dt.cur.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
                                     , keep.rownames = FALSE)
    if (file.size(dt.par.fl) > 0)
      tbl[["dt.par"]] <- as.data.table(read.csv2(dt.par.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
                                       , keep.rownames = FALSE)
    
  } else if (sep == ",") {
    
    tbl[["dt.cur"]] <- as.data.table(read.csv(dt.cur.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
                                     , keep.rownames = FALSE)
    if (file.size(dt.par.fl) > 0)
      tbl[["dt.par"]] <- as.data.table(read.csv(dt.par.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
                                       , keep.rownames = FALSE)
    
  } else if (sep == "tab") {
    
    tbl[["dt.cur"]] <- as.data.table(read.delim(dt.cur.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
                                     , keep.rownames = FALSE)
    if (file.size(dt.par.fl) > 0)
      tbl[["dt.par"]] <- as.data.table(read.delim(dt.par.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
                                       , keep.rownames = FALSE)
    
  }
  
  # remove BOM mark if needed
  
  for (tb in tbl) {
    
    if (is.data.table(tb)) {
      
      cln <- colnames(tb)
      setnames(tb, cln, str_replace(cln, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0xbb), as.raw(0xbf)))), ""))
      
    }
    
  }
  
  # remove params table if is empty
  
  if (is.data.table(tbl[["dt.par"]]) && nrow(tbl[["dt.par"]]) == 0) {
    
    tbl[["dt.par"]] <- NA
    
  }
  
  tbl
  
}

cur.scripts.load.xlsx <- function(subdir, file) {
  
  tbl <- list("dt.cur" = NA, "dt.par" = NA)
  
  if (subdir != "")
    subdir <- paste0("/", subdir)
  
  # check file
  
  fl <- paste0("input", subdir, "/", file)
  if (!file.exists(fl)) stop("Input XLSX file provided does not exist")
  
  # read sheets
  
  shts <- getSheetNames(fl)
  
  sht <- shts[shts %like% "^(input\\_)*data$"]
  sht <- sort(sht)
  
  tbl[["dt.cur"]] <- try(as.data.table(read.xlsx(fl, sheet = sht[1])), silent = TRUE)
  
  if (!is.data.table(tbl[["dt.cur"]]) | (is.data.table(tbl[["dt.cur"]]) && nrow(tbl[["dt.cur"]]) == 0)) {
    
    stop("Input XLSX file does not contain the curve sheet or the curve sheet is not valid. It should be named `input_data` or `data`, without quotes")
    
  }
  
  sht <- shts[shts %like% "^(input\\_)*param(eter)*s*$"]
  sht <- sort(sht)
  
  tbl[["dt.par"]] <- try(as.data.table(read.xlsx(fl, sheet = sht[1])), silent = TRUE)

  # remove params table if is empty
  
  if (!is.data.table(tbl[["dt.par"]]) | (is.data.table(tbl[["dt.par"]]) && nrow(tbl[["dt.par"]]) == 0)) {
    
    tbl[["dt.par"]] <- NA
    
  }
  
  tbl
  
}

cur.scripts.load <- function(sep = ";", subdir = "", file = NULL) {
  
  # select behavior
  
  if (is.null(file)) {
    
    tbl <- cur.scripts.load.plain(sep = sep, subdir = subdir)
    
  } else if (file %like% "\\.xlsx$") {
    
    tbl <- cur.scripts.load.xlsx(subdir = subdir, file = file)
    
  } else {
    
    stop("File provided is not a valid XLSX file. Provide a valid file or pass file = NULL argument if you prefer the plain text input")
    
  }
  
  tbl
  
}



