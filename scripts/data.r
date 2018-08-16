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
dt.coef[name %like% "\\+", name := paste(name, 1:.N, sep = "_"), name]


# restore constants ------------------

cnst <- (10 ^ cnst)[, 1]
cnst <- c(rep(1, part.nm), cnst)
cnst <- log(cnst)


# create result set

cln <- colnames(dt.conc)
cln <- cln[!(cln %like% "is.general")]

dt.conc.res <- dt.conc[, cln, with = FALSE]

cln <- dt.coef[(name %like% "\\+"), name]

for (i in cln) {
  
  dt.conc.res[, eval(i) := 0]
  
}

#

dt.coef.m <- as.matrix(dt.coef[, !c("name"), with = FALSE])

cln <- colnames(dt.conc)
cln <- cln[!(cln %like% "is.general")]

dt.conc.m <- as.matrix(dt.conc[, cln, with = FALSE])


# base concentrations equation

conc.base.res <- t(dt.coef.m) %*% exp(cnst + dt.coef.m %*% log(dt.conc.m[1, ]))

# product concentrations equation

conc.prod.res <- exp(cnst + dt.coef.m %*% log(dt.conc.m[1, ]))

# jacobian matrix

jc <- t(dt.coef.m) %*% (dt.coef.m * as.vector(conc.prod.res))
# t(conc.prod.res) %*% dt.coef.m %*% t(dt.coef.m)

# error vector

err.v <- t(dt.coef.m) %*% conc.prod.res - dt.conc.m[1, ]
err.v <- conc.base.res - dt.conc.m[1, ]

# step

conc.base.res - solve(jc) %*% err.v





# for (i in 1:reac.nm) {
#   
#   ff <- dt.coef.m[i, 1] * exp(cnst[i] + dt.coef.m[i, ] %*% dt.conc.m[1, ])
#   print(as.vector(ff))
#   
# }
# 
# for (i in 1:part.nm) {
#   
#   ff <- dt.coef.m[, i] %*% exp(cnst + dt.coef.m %*% dt.conc.m[1, ])
#   print(as.vector(ff))
#   
# }



# dt.coef.m[, 1] %*% exp(cnst + dt.coef.m %*% dt.conc.m[1, ]) + dt.coef.m[, 2] %*% exp(cnst + dt.coef.m %*% dt.conc.m[1, ])


# dt.conc.m %*% t(dt.coef.m)











# 

for (i in 1:reac.nm) {
  
  for (j in 1:part.nm) {
  
    dt.coef[i, j, with = FALSE] * log()
    
  }

}






