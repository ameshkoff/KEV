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

# stechiometric coefficients to matrix

dt.coef.m <- as.matrix(dt.coef[, !c("name"), with = FALSE])

cln <- colnames(dt.conc)
cln <- cln[!(cln %like% "is.general")]

# base concentrations to matrix

dt.conc.m <- as.matrix(dt.conc[, cln, with = FALSE])
dt.conc.m.iter <- copy(dt.conc.m[1, ])


for (iter in 1:1000) {
  
  # base concentrations equation
  
  conc.base.res <- t(dt.coef.m) %*% exp(cnst + dt.coef.m %*% log(dt.conc.m.iter))
  
  # product concentrations equation
  
  conc.prod.res <- exp(cnst + dt.coef.m %*% log(dt.conc.m.iter))
  
  # jacobian matrix
  
  jc <- t(dt.coef.m) %*% (dt.coef.m * as.vector(conc.prod.res))
  
  # error vector
  
  err.v <- t(dt.coef.m) %*% conc.prod.res - dt.conc.m[1, ]
  # err.v <- conc.base.res - dt.conc.m[1, ]
  
  # step
  
  tmp <- exp(log(dt.conc.m.iter) - 1 * solve(jc) %*% err.v)

  precis <- mean(abs(log(dt.conc.m.iter) - log(tmp)))
  
  dt.conc.m.iter <- tmp
  
  if (precis < 1e-08) {
    
    cat(iter, dt.conc.m.iter)
    break
    
  }
  
  if (iter %% 100 == 0) print(iter)
  
  iter <- iter + 1
  
}






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











# rs <- matrix(data = 0, nrow = part.nm, ncol = part.nm)
# 
# for (l in 1:part.nm) {
#   
#   for (j in 1:part.nm) {
#     
#     for (i in 1:reac.nm)
#     
#       rs[l,j] <- rs[l,j] + as.vector(dt.coef.m[i, l] * dt.coef.m[i, j] * conc.prod.res[i, ])
#       
#   }
#   
# }







