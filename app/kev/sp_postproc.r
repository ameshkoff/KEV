# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #


sp.merge.postproc <- function(dt.x, dt.y) {
  
  merge(dt.x
        , dt.y
        , by = "wavelength", all = TRUE)
  
}



# postproc ------------------------------ #

sp.postproc <- function(dt.ttl) {
  
  for (i in names(dt.ttl)) {
    
    dt.ttl[[i]] <- dt.ttl[[i]][, .(wavelength, mol.ext.coef, adj.r.sq)]
    setnames(dt.ttl[[i]], c("wavelength", i, paste0(i, "_adj.r.squared")))
  }
  
  dt.mol.full <- Reduce(sp.merge.postproc, dt.ttl)
  
  dt.mol.full
  
}
