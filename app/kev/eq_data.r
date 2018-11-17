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

eq.scripts.load <- function(sep = ";", subdir = "") {
  
  tbl <- list("cnst" = NA, "dt.coef" = NA, "dt.conc" = NA, "part.eq" = NA)
  
  if (subdir != "")
    subdir <- paste0("/", subdir, "/")
  
  subdir <- paste0("input", subdir)
  fls <- list.files(subdir)
  
  cnst.fl <- paste0(subdir, fls[fls %like% "^(input\\_)*k\\_constants\\_log10(\\.csv|\\.txt)*"][1])
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



