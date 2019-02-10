# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #



# ---------------------- load libraries ----------------------

# I/O
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

ab.scripts.load <- function(sep = ";", subdir = "") {
  
  tbl <- list("dt.ab" = NA, "dt.mol" = NA)
  
  if (subdir != "")
    subdir <- paste0("/", subdir, "/")
  
  subdir <- paste0("input", subdir)
  fls <- list.files(subdir)
  
  dt.ab.fl <- paste0(subdir, fls[fls %like% "^(input\\_)*absorbance(\\.csv|\\.txt)*"][1])
  dt.mol.fl <- paste0(subdir, fls[fls %like% "^(input\\_)*mol(ar)*\\_ext(inction)*\\_coefficients(\\.csv|\\.txt)*"][1])
  
  if (sep == ";") {
    
    tbl[["dt.ab"]] <- as.data.table(read.csv2(dt.ab.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
                                      , keep.rownames = FALSE)
    if (file.size(dt.mol.fl) > 0)
      tbl[["dt.mol"]] <- as.data.table(read.csv2(dt.mol.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
                                     , keep.rownames = FALSE)

  } else if (sep == ",") {
    
    tbl[["dt.ab"]] <- as.data.table(read.csv(dt.ab.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
                                      , keep.rownames = FALSE)
    if (file.size(dt.mol.fl) > 0)
      tbl[["dt.mol"]] <- as.data.table(read.csv(dt.mol.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
                                       , keep.rownames = FALSE)

  } else if (sep == "tab") {
    
    tbl[["dt.ab"]] <- as.data.table(read.delim(dt.ab.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
                                      , keep.rownames = FALSE)
    if (file.size(dt.mol.fl) > 0)
      tbl[["dt.mol"]] <- as.data.table(read.delim(dt.mol.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
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



