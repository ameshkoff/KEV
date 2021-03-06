# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2020                                                 #
#                                                            #
# ########################################################## #


# common input

output$ht.dt.coef.csv <- download_dt.coef.csv("ht")
output$ht.dt.coef.xlsx <- download_dt.coef.xlsx("ht")

output$ht.cnst.csv <- download_cnst.csv("ht")
output$ht.cnst.xlsx <- download_cnst.xlsx("ht")

output$ht.dt.conc.csv <- download_dt.conc.csv("ht")
output$ht.dt.conc.xlsx <- download_dt.conc.xlsx("ht")

# specific input

output$ht.dt.heat.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "input_heats.csv"
    
  },
  
  content = function(file) {
    
    if (ht.sep() == ";") {
      write.csv2(ht.dt.heat.data(), file, row.names = FALSE)
    } else if (ht.sep() == ",") {
      write.csv(ht.dt.heat.data(), file, row.names = FALSE)
    }else if (ht.sep() == "tab") {
      write.table(ht.dt.heat.data(), file, row.names = FALSE, sep = "\t")
    }
    
  }
  
)
# ----

output$ht.dt.heat.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "input_heats.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(ht.dt.heat.data(), file)
    
  }
  
)
# ----

output$ht.dt.enth.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "input_enthalpies.csv"
    
  },
  
  content = function(file) {
    
    if (ht.sep() == ";") {
      write.csv2(ht.dt.enth.data(), file, row.names = FALSE)
    } else if (ht.sep() == ",") {
      write.csv(ht.dt.enth.data(), file, row.names = FALSE)
    } else if (ht.sep() == "tab") {
      write.table(ht.dt.enth.data(), file, row.names = FALSE, sep = "\t")
    }
    
  }
  
)
# ----

output$ht.dt.enth.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "input_enthalpies.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(ht.dt.enth.data(), file)
    
  }
  
)
# ----

# standard output

output$ht.dt.res.csv <- download_dt.res.csv("ht")
output$ht.dt.res.xlsx <- download_dt.res.xlsx("ht")

output$ht.cnst.dev.csv <- download_cnst.dev.csv("ht")
output$ht.cnst.dev.xlsx <- download_cnst.dev.xlsx("ht")

output$ht.cor.m.csv <- download_cor.m.csv("ht")
output$ht.cor.m.xlsx <- download_cor.m.xlsx("ht")

output$ht.dt.metrics.csv <- download_dt.metrics.csv("ht")
output$ht.dt.metrics.xlsx <- download_dt.metrics.xlsx("ht")

# specific output

output$ht.dt.heat.calc.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "heats_calculated.csv"
    
  },
  
  content = function(file) {
    
    if (ht.sep() == ";") {
      write.csv2(ht.dt.heat.calc.data(), file, row.names = FALSE)
    } else if (ht.sep() == ",") {
      write.csv(ht.dt.heat.calc.data(), file, row.names = FALSE)
    } else if (ht.sep() == "tab") {
      write.table(ht.dt.heat.calc.data(), file, row.names = FALSE, sep = "\t")
    }
    
  }
  
)
# ----

output$ht.dt.heat.calc.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "heats_calculated.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(ht.dt.heat.calc.data(), file)
    
  }
  
)
# ----

output$ht.dt.enth.calc.csv <- downloadHandler(
  # ----
  filename = function() {
    
    "enthalpies_calculated.csv"
    
  },
  
  content = function(file) {
    
    if (ht.sep() == ";") {
      write.csv2(ht.dt.enth.calc.data(), file, row.names = FALSE)
    } else if (ht.sep() == ",") {
      write.csv(ht.dt.enth.calc.data(), file, row.names = FALSE)
    } else if (ht.sep() == "tab") {
      write.table(ht.dt.enth.calc.data(), file, row.names = FALSE, sep = "\t")
    }
    
  }
  
)
# ----

output$ht.dt.enth.calc.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "enthalpies_calculated.xlsx"
    
  },
  
  content = function(file) {
    
    write.xlsx(ht.dt.enth.calc.data(), file)
    
  }
  
)
# ----

output$kev.ht.data.zip <- downloadHandler(
  # ----
  filename = function() {
    
    "kev.ht.constants.data.zip"
    
  },
  
  content = function(file) {
    
    dt.dict <- fread("algo/calorimetry/dt.dict.csv")
    
    dt.ttl <- list()
    
    for (i in nrow(dt.dict):1) {
      
      fn.name <- paste0("ht.", str_remove(dt.dict[i, dt], "\\.input$"), ".data()")
      
      dt <- NULL
      try(dt <- eval(expr = parse(text = fn.name)), silent = TRUE)
      
      if (!is.null(dt)) dt.ttl[[dt.dict[i, dt]]] <- dt
      
    }
    
    # temporary directory to avoid permission issues
    
    curdir <- getwd()
    tmpdir <- tempdir()
    setwd(tmpdir)
    print(tempdir())
    
    dt.dict <- ht.save(dt.ttl, path = "", sep = ht.sep(), filename = NULL)
    
    if (ht.sep() %in% c("tab", "\t")) {
      
      dt.dict[, file := paste0(file, ".txt")]
      
    } else {
      
      dt.dict[, file := paste0(file, ".csv")]
      
    }
    
    # create zip
    
    utils::zip(file, dt.dict[, file])
    
    # remove garbage from the disc
    
    for (fl in dt.dict[, file]) {
      
      if (file.exists(fl)) file.remove(fl)
      
    }
    
    setwd(curdir)
    
  }
  
)
# ----

output$kev.ht.data.xlsx <- downloadHandler(
  # ----
  filename = function() {
    
    "kev.ht.constants.data.xlsx"
    
  },
  
  content = function(file) {
    
    dt.dict <- fread("algo/calorimetry/dt.dict.csv")
    
    dt.ttl <- list()
    
    for (i in nrow(dt.dict):1) {
      
      fn.name <- paste0("ht.", str_remove(dt.dict[i, dt], "\\.input$"), ".data()")
      
      dt <- NULL
      try(dt <- eval(expr = parse(text = fn.name)), silent = TRUE)
      
      if (!is.null(dt)) dt.ttl[[dt.dict[i, dt]]] <- dt
      
    }
    
    dt.dict <- ht.save(dt.ttl, path = "", sep = ht.sep(), filename = file)
    
  }
  
)
# ----



