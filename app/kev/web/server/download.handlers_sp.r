# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #



# extinction coefficients downoad ---------------- #

output$sp.dt.mol.full.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "molar_extinction_coefficients.csv"
    
  },
  
  content = function(file) {
    
    if (sp.sep() == ";") {
      write.csv2(sp.dt.mol.full.data(), file, row.names = FALSE)
    } else if (sp.sep() == ",") {
      write.csv(sp.dt.mol.full.data(), file, row.names = FALSE)
    } else if (sp.sep() == "tab") {
      write.table(sp.dt.mol.full.data(), file, row.names = FALSE, sep = "\t")
    }
    
  }
  
)
# ----

output$sp.dt.mol.full.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "molar_extinction_coefficients.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(sp.dt.mol.full.data(), file)
    
  }
  
)
# ----


