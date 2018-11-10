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

sp.scripts.load <- function(sep = ";", subdir = "") {
  
  
  if (subdir != "")
    subdir <- paste0("/", subdir, "/")
  
  subdir <- paste0("input", subdir)
  
  fls <- list.files(subdir)
  fls <- fls[fls %like% "^(spectra\\_|calibr.*spectr)" & !(fls %like% "\\.xls(x|b|m)*$")]
  
  tbl <- vector("list", length(fls))
  names(tbl) <- fls
  
  for (i in fls) {
    
    fl <- paste0(subdir, i)
    
    con <- file(fl, "r")
    pt.name <- readLines(con, n = 1)
    close(con)
    
    # define where to get particle name and whether to skip first row of the file
    
    skp <- 1
    
    if (pt.name %like% "wave.*length")
      skp <- 0
    
    if (skp == 0 || pt.name == "")
      pt.name <- str_extract(i, "\\_[A-Za-z0-9]+(\\.(txt|csv))*$") %>% str_replace_all("\\_|\\.(txt|csv)$", "")
    
    # load
    
    if (sep == ";") {
      
      tbl[[i]] <- as.data.table(read.csv2(fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE, skip = skp)
                                      , keep.rownames = FALSE)

    } else if (sep == ",") {
      
      tbl[[i]] <- as.data.table(read.csv(fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE, skip = skp)
                                      , keep.rownames = FALSE)

    } else if (sep == "tab") {
      
      tbl[[i]] <- as.data.table(read.delim(fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE, skip = skp)
                                      , keep.rownames = FALSE)

    }
    
    # table name
    names(tbl)[which(names(tbl) == i)] <- pt.name
    
  }

  tbl
  
}



