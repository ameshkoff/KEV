# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
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

nm.scripts.load <- function(sep = ";", subdir = "") {
  
  tbl <- list("dt.nm" = NA, "dt.ind" = NA)
  
  if (subdir != "")
    subdir <- paste0("/", subdir, "/")
  
  subdir <- paste0("input", subdir)
  fls <- list.files(subdir)
  
  dt.nm.fl <- paste0(subdir, fls[fls %like% "^(input\\_)*chem(ical)*\\_shifts*(\\.csv|\\.txt)*"][1])
  dt.ind.fl <- paste0(subdir, fls[fls %like% "^(input\\_)*ind(ividual)*\\_shifts*(\\.csv|\\.txt)*"][1])
  trg.fl <- paste0(subdir, fls[fls %like% "^(input\\_)*(targets*|constants*\\_names*)(\\.csv|\\.txt)*"][1])
  
  if (sep == ";") {
    
    tbl[["dt.nm"]] <- as.data.table(read.csv2(dt.nm.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
                                    , keep.rownames = FALSE)
    if (file.size(dt.ind.fl) > 0)
      tbl[["dt.ind"]] <- as.data.table(read.csv2(dt.ind.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
                                       , keep.rownames = FALSE)
    if (file.size(trg.fl) > 0)
      trg <- as.data.table(read.delim(trg.fl, stringsAsFactors = FALSE, colClasses = "character"
                                               , check.names = FALSE, sep = sep, dec = ",", header = FALSE)
                                    , keep.rownames = FALSE)
    
  } else if (sep == ",") {
    
    tbl[["dt.nm"]] <- as.data.table(read.csv(dt.nm.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
                                    , keep.rownames = FALSE)
    if (file.size(dt.ind.fl) > 0)
      tbl[["dt.ind"]] <- as.data.table(read.csv(dt.ind.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
                                       , keep.rownames = FALSE)
    if (file.size(trg.fl) > 0)
      trg <- as.data.table(read.delim(trg.fl, stringsAsFactors = FALSE, colClasses = "character"
                                               , check.names = FALSE, sep = sep, dec = ".", header = FALSE)
                                    , keep.rownames = FALSE)

  } else if (sep == "tab") {
    
    tbl[["dt.nm"]] <- as.data.table(read.delim(dt.nm.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
                                    , keep.rownames = FALSE)
    if (file.size(dt.ind.fl) > 0)
      tbl[["dt.ind"]] <- as.data.table(read.delim(dt.ind.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
                                       , keep.rownames = FALSE)
    if (file.size(trg.fl) > 0)
      trg <- as.data.table(read.delim(trg.fl, stringsAsFactors = FALSE, colClasses = "character"
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
  
  # special treatment of targets file
  
  if (!is.null(trg)) {
    
    trg[1, V1 := str_replace(V1, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0xbb), as.raw(0xbf)))), "")]
    
    if (length(unlist(trg[V1 %like% "^constants*$"])) > 0) {
      
      tbl[["cnst.tune"]] <- unlist(trg[V1 %like% "^constants*$"])
      tbl[["cnst.tune"]] <- tbl[["cnst.tune"]][!is.na(tbl[["cnst.tune"]]) & !(tbl[["cnst.tune"]] %like% "^constants*$")]
      
    }
    
  }

  tbl
  
}



