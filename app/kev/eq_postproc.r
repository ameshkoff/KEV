# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #


# conditional fractions ------------------------------ #

eq.cond.fractions <- function(dt.res, bs.name, dt.coef, dt.coef.m, dt.conc.m, pc.name) {
  
  cln <- colnames(dt.res)
  cln <- cln[cln %like% bs.name]
  
  dt.frac <- t(round(100 * t(as.matrix(dt.res[, cln, with = FALSE]) %*%
                               diag(dt.coef.m[dt.coef[, eval(as.name(bs.name))] != 0, bs.name])) %*%
                       as.matrix(diag(1 / dt.conc.m[, bs.name], nrow = length(dt.conc.m[, bs.name]))), 2))
  
  dt.frac <- data.table(t(dt.frac))
  dt.frac <- data.table(rn = colnames(dt.res[, cln, with = FALSE]), dt.frac)
  
  if (!is.null(pc.name) && bs.name != pc.name) {
    
    cln <- colnames(dt.res)
    cln <- cln[cln %in% c(pc.name, paste0(pc.name, "_1"))]
    
    tmp <- data.table(rn = paste0("p(", pc.name, ")"), t(data.table(round(-log10(dt.res[, cln, with = FALSE]), 2))))
    
  } else {
    
    cln <- colnames(dt.res)
    cln <- cln[cln %in% c(bs.name, paste0(bs.name, "_1"))]
    
    tmp <- data.table(rn = paste0("p(", bs.name, ")"), t(data.table(round(-log10(dt.res[, cln, with = FALSE]), 2))))
    
  }
  
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




