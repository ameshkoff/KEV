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

emf.scripts.load <- function(sep = ";", subdir = "") {
  
  tbl <- list("dt.emf" = NA)
  
  if (subdir != "")
    subdir <- paste0("/", subdir, "/")
  
  subdir <- paste0("input", subdir)
  fls <- list.files(subdir)
  
  dt.emf.fl <- paste0(subdir, fls[fls %like% "^(input\\_)*emf(\\.csv|\\.txt)*"][1])
  trg.fl <- paste0(subdir, fls[fls %like% "^(input\\_)*targets(\\.csv|\\.txt)*"][1])
  
  if (sep == ";") {
    
    tbl[["dt.emf"]] <- as.data.table(read.csv2(dt.emf.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
                                    , keep.rownames = FALSE)
    trg <- as.data.table(read.delim(trg.fl, stringsAsFactors = FALSE, colClasses = "character"
                                    , check.names = FALSE, sep = sep, dec = ",", header = FALSE)
                                       , keep.rownames = FALSE)
    
  } else if (sep == ",") {
    
    tbl[["dt.emf"]] <- as.data.table(read.csv(dt.emf.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
                                    , keep.rownames = FALSE)
    trg <- as.data.table(read.delim(trg.fl, stringsAsFactors = FALSE, colClasses = "character"
                                    , check.names = FALSE, sep = sep, dec = ".", header = FALSE)
                         , keep.rownames = FALSE)
    
  } else if (sep == "tab") {
    
    tbl[["dt.emf"]] <- as.data.table(read.delim(dt.emf.fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
                                    , keep.rownames = FALSE)
    trg <- as.data.table(read.delim(trg.fl, stringsAsFactors = FALSE, colClasses = "character"
                                    , check.names = FALSE, sep = "\t", dec = ".", header = FALSE)
                         , keep.rownames = FALSE)
    
  }

  # remove BOM mark if needed
  
  cln <- colnames(tbl[["dt.emf"]])
  setnames(tbl[["dt.emf"]], cln, str_replace(cln, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0xbb), as.raw(0xbf)))), ""))
  
  cln <- colnames(trg)
  setnames(trg, cln, str_replace(cln, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0xbb), as.raw(0xbf)))), ""))
  
  # split trg into separate data sets
  # not in preproc to keep consistent wuth the GUI input
  
  if (length(unlist(trg[V1 == "constant"])) > 0) {
    
    tbl[["cnst.tune"]] <- unlist(trg[V1 == "constant"])
    tbl[["cnst.tune"]] <- tbl[["cnst.tune"]][!is.na(tbl[["cnst.tune"]]) & tbl[["cnst.tune"]] != "constant"]
    
  }
  
  if (nrow(trg[V1 %in% c("standard.potential", "slope")]) == 2)
    tbl[["dt.params"]] <- trg[V1 %in% c("standard.potential", "slope")]

  tbl
  
}



