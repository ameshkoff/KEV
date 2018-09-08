# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #


# conditional fractions ------------------------------ #

eq.cond.fractions <- function(dt.res, bs.name, dt.coef, dt.coef.m, dt.conc.m) {
  
  cln <- colnames(dt.res)
  cln <- cln[cln %like% bs.name]
  
  dt.frac <- t(round(100 * t(as.matrix(dt.res[, cln, with = FALSE]) %*%
                               diag(dt.coef.m[dt.coef[, eval(as.name(bs.name))] != 0, bs.name])) %*%
                       diag(1 / dt.conc.m[, bs.name]), 2))
  
  dt.frac <- data.table(t(dt.frac))
  dt.frac <- data.table(rn = colnames(dt.res[, cln, with = FALSE]), dt.frac)
  
  tmp <- data.table(rn = paste0("-lg(C(", bs.name, "))"), t(data.table(round(-log10(dt.res[, eval(as.name(paste0(bs.name, "_1")))]), 2))))
  
  dt.frac <- rbind(tmp, dt.frac)
  
  cln <- colnames(dt.frac)
  setnames(dt.frac, cln, str_replace(cln, "^V", "S_"))
  
  dt.frac  
  
}


# calculated total concentrations ------------------------ #

eq.tot.conc.calc <- function(dt.res, cnst.m, dt.coef.m, part.nm) {
  
  t(exp(as.vector(cnst.m) + dt.coef.m %*% log(t(dt.res[, 1:part.nm])))) %*% dt.coef.m
  
}


# residuals ---------------------------------------------- #

eq.residuals <- function(dt.conc.m, dt.conc.calc, part.eq) {
  
  rtrn <- (dt.conc.calc - dt.conc.m) / dt.conc.m
  rtrn[, part.eq] <- 0
  
  rtrn
  
}




