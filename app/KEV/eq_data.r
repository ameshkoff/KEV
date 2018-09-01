# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #



# ---------------------- load libraries ----------------------

# I/O
library(readr)
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
  
  if (sep == ";") {
    
    tbl[["cnst"]] <- as.data.table(read.csv2(paste0("input", subdir, "k_constants_log10.csv"), stringsAsFactors = FALSE, colClasses = "character")
                                 , keep.rownames = FALSE)
    tbl[["dt.coef"]] <- as.data.table(read.csv2(paste0("input", subdir, "stoich_coefficients.csv"), stringsAsFactors = FALSE, colClasses = "character")
                                 , keep.rownames = FALSE)
    tbl[["dt.conc"]] <- as.data.table(read.csv2(paste0("input", subdir, "concentrations.csv"), stringsAsFactors = FALSE, colClasses = "character"
                                                 , skip = 1), keep.rownames = FALSE)
    tbl[["part.eq"]] <- as.data.table(read.csv2(paste0("input", subdir, "concentrations.csv"), stringsAsFactors = FALSE, colClasses = "character"
                                                , header = FALSE , nrows = 1), keep.rownames = FALSE)
    
  } else if (sep == ",") {
    
    tbl[["cnst"]] <- as.data.table(read.csv(paste0("input", subdir, "k_constants_log10.csv"), stringsAsFactors = FALSE, colClasses = "character")
                                   , keep.rownames = FALSE)
    tbl[["dt.coef"]] <- as.data.table(read.csv(paste0("input", subdir, "stoich_coefficients.csv"), stringsAsFactors = FALSE, colClasses = "character")
                                      , keep.rownames = FALSE)
    tbl[["dt.conc"]] <- as.data.table(read.csv(paste0("input", subdir, "concentrations.csv"), stringsAsFactors = FALSE, colClasses = "character"
                                                , skip = 1), keep.rownames = FALSE)
    tbl[["part.eq"]] <- as.data.table(read.csv(paste0("input", subdir, "concentrations.csv"), stringsAsFactors = FALSE, colClasses = "character"
                                                , header = FALSE , nrows = 1), keep.rownames = FALSE)
    
  }
  
  tbl
  
}



