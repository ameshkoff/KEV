# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #



# evaluators ---------------------------------------------- #

spectra.mol.ext.evaluator <- function(dt.ttl) {
  
  for (i in names(dt.ttl)) {
    
    dt <- dt.ttl[[i]]
    
    wl <- colnames(dt)
    wl <- wl[!(wl %like% "concentr")]
    
    dt.res <- data.table(wavelength = wl)
    
    # run linear models in a loop
    
    for (w in wl) {
      
      frml <- paste0("`", w, "` ~ 1 + concentration")
      frml <- as.formula(frml)
      
      # evaluate
      
      md <- lm(formula = frml, data = dt)
      sm <- as.data.table(summary(md)$coefficients, keep.rownames = TRUE)
      
      # save results
      
      dt.res[wl == w, `:=`(mol.ext.coef = sm[rn == "concentration", Estimate]
                           , p.value = sm[rn == "concentration", `Pr(>|t|)`]
                           , adj.r.sq = summary(md)$adj.r.squared)]
      
      
    }
    
    # set negative values to 0
    
    dt.res[mol.ext.coef < 0, mol.ext.coef := 0]
    
    dt.ttl[[i]] <- dt.res
        
  }
  
  dt.ttl

}












