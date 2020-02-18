# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #



# extinction coefficients -------------------------------------------------


# technical

sp.sep <- reactive({
  
  switch(input$sp.sep,
         comma = ",",
         semicolon = ";",
         tab = "tab")
  
})

# data --------------------- #

# input data

observeEvent(input$file.sp.bulk.input, {
  
  sep <- sp.sep()
  
  in.file.bulk <- as.data.table(input$file.sp.bulk.input)
  
  in.file.xlsx <- NULL
  in.file.xlsx <- in.file.bulk[name %like% "\\.xlsx$"]
  
  if (nrow(in.file.xlsx) > 0) {
    
    in.file.xlsx <- as.data.frame(in.file.xlsx[1])
    
  } else {
    
    in.file.xlsx <- NULL
    
  }
  
  if (is.null(in.file.xlsx)) {
    
    tbl <- vector("list", nrow(in.file.bulk))
    names(tbl) <- in.file.bulk[, name]
    
    for (fl in in.file.bulk[, datapath]) {
      
      i <- in.file.bulk[datapath == fl, name]
      
      con <- file(fl, "r")
      pt.name <- readLines(con, n = 1)
      close(con)
      
      # define where to get component name and whether to skip first row of the file
      
      skp <- 1
      
      if (pt.name %like% "wave.*length")
        skp <- 0
      
      if (skp == 0 || pt.name == "")
        pt.name <- str_extract(i, "\\_[A-Za-z0-9]+(\\.(txt|csv))*$") %>% str_replace_all("\\_|\\.(txt|csv)$", "")
      
      # load
      
      if (sep == ";") {
        
        tbl[[i]] <- as.data.table(read.csv2(fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE, skip = skp)
                                  , keep.rownames = FALSE)
        
      } else if (sep == ",") {
        
        tbl[[i]] <- as.data.table(read.csv(fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE, skip = skp)
                                  , keep.rownames = FALSE)
        
      } else if (sep == "tab") {
        
        tbl[[i]] <- as.data.table(read.delim(fl, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE, skip = skp)
                                  , keep.rownames = FALSE)
        
      }
      
      # table name
      names(tbl)[which(names(tbl) == i)] <- pt.name
      
    }
    
  } else {
    
    shts <- getSheetNames(in.file.xlsx$datapath[1])
    
    tbl <- vector("list", length(shts))
    names(tbl) <- shts
    
    for (sh in shts) {
      
      pt.name <- try(read.xlsx(in.file.xlsx$datapath[1], sheet = sh, startRow = 1, rows = 1), silent = TRUE)
      pt.name <- try(colnames(pt.name)[1], silent = TRUE)
      
      if (is.null(pt.name) || is.na(pt.name))
        pt.name <- ""
      
      # define where to get component name and whether to skip first row of the file
      
      strt <- 2
      
      if (pt.name %like% "wave.*length")
        strt <- 1
      
      if (strt == 1 || pt.name == "")
        pt.name <- str_extract(i, "\\_[A-Za-z0-9]+(\\.(txt|csv))*$") %>% str_replace_all("\\_|\\.(txt|csv)$", "")
      
      # load
      
      tbl[[sh]] <- try(read.xlsx(in.file.xlsx$datapath[1], sheet = sh, startRow = strt), silent = TRUE)
      tbl[[sh]] <- as.data.table(tbl[[sh]])
      
      # table name
      names(tbl)[which(names(tbl) == sh)] <- pt.name
      
    }
    
  }
  
  values[["dt.mol.raw"]] <- tbl
  
})

# execute

sp.eval.data <- reactive({
  
  dt.ttl <- values[["dt.mol.raw"]]
  
  withProgress(message = "Computation... It may take some time", value = 0, {
    
    incProgress(.2)
    
    # run
    
    res <- sp.evaluation.runner(mode = "app"
                                , sep = sp.sep()
                                , dt.list = dt.ttl
                                , save.res = FALSE)
    
    incProgress(.8)
    
  })
  
  res
  
})

# output data

sp.dt.mol.full.data <- reactive({
  
  sp.eval.data()
  
})


# rendering ---------------- #

output$sp.dt.mol.full <- renderRHandsontable({
  
  dt <- sp.dt.mol.full.data()
  
  if (!is.null(dt)) {
    
    col_highlight <- which(colnames(dt) %like% "squared") - 1
    renderer <- "
        function (instance, td, row, col, prop, value, cellProperties) {

          Handsontable.renderers.NumericRenderer.apply(this, arguments);
          
          if (instance.params) {
            hcols = instance.params.col_highlight
            hcols = hcols instanceof Array ? hcols : [hcols]
          }
          
          if (instance.params && hcols.includes(col) && value < 0.95) {
            td.style.background = 'pink';
          }
          
        }" 
    
    if (nrow(dt) > 25) {
      
      rhandsontable(dt, col_highlight = col_highlight, stretchH = "all", height = 550) %>%
        hot_cols(renderer = renderer, format = "0.00000") %>%
        hot_col("wavelength", format = "0.0")
      
    } else {
      
      rhandsontable(dt, col_highlight = col_highlight, stretchH = "all", height = NULL) %>%
        hot_cols(renderer = renderer, format = "0.00000") %>%
        hot_col("wavelength", format = "0.0")
      
    }
    
  }
  
})




