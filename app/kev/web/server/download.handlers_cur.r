# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #



# curve fitting ---------------- #

output$kev.cur.data.zip <- downloadHandler(
  # ----
  filename = function() {
    
    "kev.curves.fitting.zip"
    
  },
  
  content = function(file) {
    
    
    # temporary directory to avoid permission issues
    
    curdir <- getwd()
    tmpdir <- tempdir()
    setwd(tmpdir)
    print(tempdir())
    
    cur.save(values$cur.status, file)
    
    setwd(curdir)
    
  }
  
)
# ----

output$kev.cur.data.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "kev.curves.fitting.xlsx"
    
  },
  
  content = function(file) {
    
    cur.save(values$cur.status, file)
    
  }
  
)
# ----

output$kev.cur.data.bottom.zip <- downloadHandler(
  # ----
  filename = function() {
    
    "kev.curves.fitting.zip"
    
  },
  
  content = function(file) {
    
    # temporary directory to avoid permission issues
    
    curdir <- getwd()
    tmpdir <- tempdir()
    setwd(tmpdir)
    print(tempdir())
    
    cur.save(values$cur.status, file)
    
    setwd(curdir)
    
  }
  
)
# ----

output$kev.cur.data.bottom.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "kev.curves.fitting.xlsx"
    
  },
  
  content = function(file) {
    
    cur.save(values$cur.status, file)
    
  }
  
)
# ----

