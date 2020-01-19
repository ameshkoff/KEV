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

xlsx.to.csv <- function(xlsx.file
                        , sep = c(",", ";", "tab")
                        , delim = c("default", ".", ",")
                        , target.dir = ""
                        , subdir = "default") {
  
  if (is.character(xlsx.file)) {
    
    target.dir <- xlsx.file
    target.dir <- str_extract(target.dir, "^.*(\\/|\\\\)")
    
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
  
  ext <- ".csv"
  if (sep == "tab") ext <- ".txt"
  
  for (sh in sheets(xlsx.file)) {
    
    dt <- readWorkbook(xlsxFile = xlsx.file, sheet = sh, fillMergedCells = TRUE)
    
    write.table(dt, file = paste0(target.dir, sh, ext), sep = sep, dec = delim, row.names = FALSE)
    
  }
  
  0

}







