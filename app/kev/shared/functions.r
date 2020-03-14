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

# ------------------------------------------------------------

# https://stackoverflow.com/a/27347397

CJ.dt = function(X, Y) {
  
  stopifnot(is.data.table(X), is.data.table(Y))
  k = NULL
  
  X = X[, c(k = 1, .SD)]
  setkey(X, k)
  
  Y = Y[, c(k = 1, .SD)]
  setkey(Y, NULL)
  
  X[Y, allow.cartesian = TRUE][, k := NULL][]
  
}

#

xlsx.to.csv <- function(xlsx.file
                        , target.dir = ""
                        , sep = c(",", ";", "tab")
                        , delim = c("default", ".", ",")
                        , subdir = c("default", "")
                        , overwrite = TRUE
                        , verbose = FALSE) {
  
  sep <- sep[1]
  delim <- delim[1]
  subdir <- subdir[1]
  
  if (is.character(xlsx.file)) {
    
    if (target.dir == "") {

      target.dir <- xlsx.file
      target.dir <- dirname(target.dir)
      
    }
    
    xlsx.file <- loadWorkbook(file = xlsx.file)

  }

  if (sep == "tab") sep <- "\t"
  
  if (subdir == "default") {
    
    if (sep == ",") subdir <- "csv.comma"
    if (sep == ";") subdir <- "csv.semicolon"
    if (sep == "\t") subdir <- "txt.tab"
    
  }

  if (subdir != "") {
    
    target.dir <- paste0(target.dir, "/", subdir)
    dir.create(target.dir, showWarnings = FALSE)
    
  }
  
  target.dir <- str_replace_all(target.dir, "\\\\", "/")
  target.dir <- paste0(target.dir, "/")
  target.dir <- str_replace_all(target.dir, "\\/\\/", "/")

  if (delim == "default" && sep == ",") delim <- "."
  if (delim == "default" && sep == ";") delim <- ","
  if (delim == "default") delim <- "."
  
  ext <- ".csv"
  if (sep == "tab") ext <- ".txt"
  
  for (sh in sheets(xlsx.file)) {
    
    dt <- readWorkbook(xlsxFile = xlsx.file, sheet = sh, fillMergedCells = TRUE)
    fl <- paste0(target.dir, sh, ext)
    
    if (!file.exists(fl) || overwrite) {
      
      write.table(dt, file = fl, sep = sep, dec = delim, row.names = FALSE)
      if (verbose) print(fl)
      
    }
    
  }
  
  0

}

xlsx.to.csv.dir <- function(target.dir
                            , sep = c("all", ",", ";", "tab")
                            , delim = c("default", ".", ",")
                            , subdir = "default"
                            , ignore.pattern = ""
                            , overwrite = TRUE) {
  
  fls <- list.files(target.dir, pattern = "\\.xlsx$", full.names = TRUE, recursive = TRUE)
  fls <- fls[!(fls %like% "^\\~\\.lock\\.|^\\~\\$")]
  
  if (ignore.pattern != "") fls <- fls[!(fls %like% ignore.pattern)]
  
  fls <- data.table(file = fls, dir = dirname(fls))
  
  # get duplicates
  
  fls[, ln := .N, dir]
  fls[ln > 1, dir := paste0(dir, "/", str_remove(basename(file), "\\.xlsx"))]
  
  lapply(fls[, dir], dir.create, showWarnings = FALSE)
  
  # populate
  
  if (sep[1] == "all") {
    
    fls <- CJ.dt(fls, data.table(sep = c(",", ";", "\t")))
    
  } else {
    
    fls[, sep := sep[1]]
    
  }
  
  # run
  
  for (i in 1:nrow(fls)) {
    
    xlsx.to.csv(xlsx.file = fls[i, file]
                , target.dir = fls[i, dir]
                , sep = fls[i, sep]
                , delim = delim[1]
                , subdir = subdir
                , overwrite = overwrite
                , verbose = TRUE)
    
  }
  
  0
  
}






