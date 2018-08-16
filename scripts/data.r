# ########################################################## #
#                                                            #
# Name: 
# Author: AMeshkov
# Date: 2018                                                 #
#                                                            #
# ########################################################## #



# ---------------------- load libraries ----------------------

# I/O
library(readr)
library(openxlsx)
# data structure
library(data.table)
library(Matrix)
library(Hmisc)
# strings
library(stringi)
library(stringr)





# ------------------------ load data -------------------------

# load data

cnst <- as.data.table(read.csv2("data.raw/k_constants_log10.csv"), keep.rownames = FALSE)
dt.coef <- as.data.table(read.csv2("data.raw/stech_coefficients.csv"), keep.rownames = FALSE)
dt.conc <- as.data.table(read.csv2("data.raw/concentrations.csv"), keep.rownames = FALSE)


# ---------------------- preprocessing -----------------------

# create variables --------------------

cnst.nm <- nrow(cnst)
part.nm <- ncol(dt.coef)
reac.nm <- nrow(dt.coef) + part.nm

# complete coefficients matrix -------

cln <- colnames(dt.coef)

dt.coef <- rbind(as.data.table(diag(part.nm)), dt.coef, use.names = FALSE)
setnames(dt.coef, cln)

# create names

dt.coef[, name := ""]

cln <- colnames(dt.coef)
cln <- cln[cln != "name"]

for (i in cln) {
  
  dt.coef[eval(as.name(i)) > 0, name := paste0(name, " + ", i)]
  dt.coef[eval(as.name(i)) < 0, name := paste0(name, " - ", i)]
  
}

dt.coef[, name := str_replace(name, "^ *\\+ *", "")]


# restore constants ------------------

cnst <- (10 ^ cnst)[, 1]
cnst <- c(rep(1, part.nm), cnst)
cnst <- log(cnst)



# 

for (i in 1:reac.nm) {
  
  for (j in 1:part.nm) {
  
    dt.coef[i, j, with = FALSE] * log()
    
  }

}






