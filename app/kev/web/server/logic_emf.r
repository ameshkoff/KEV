# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #



# emf -------------------------------------------------------


# technical

emf.sep <- reactive({
  
  switch(input$emf.sep,
         comma = ",",
         semicolon = ";",
         tab = "tab")
  
})

observeEvent(input$file.emf.bulk.input, {
  
  # stoichiometric coefficients
  
  if (nrow(as.data.table(input$file.emf.bulk.input)[name %like% "^(input\\_)*stoich(iometric)*\\_coefficients(\\.csv|\\.txt)*"]) > 0){
    input.source$emf.dt.coef.bulk <- TRUE
  }
  
  if (nrow(as.data.table(input$file.emf.bulk.input)[name %like% "\\.xlsx$"]) > 0){
    
    shts <- getSheetNames(input$file.emf.bulk.input$datapath)
    
    if (length(shts[shts %like% "stoich(iometric)*_coefficients"]))
      input.source$emf.dt.coef.bulk <- TRUE
    
  }
  
  # concentrations
  
  if (nrow(as.data.table(input$file.emf.bulk.input)[name %like% "^(input\\_)*concentrations(\\.csv|\\.txt)*"]) > 0){
    input.source$emf.dt.conc.bulk <- TRUE
  }
  
  if (nrow(as.data.table(input$file.emf.bulk.input)[name %like% "\\.xlsx$"]) > 0){
    
    shts <- getSheetNames(input$file.emf.bulk.input$datapath)
    
    if (length(shts[shts %like% "^(input_|output_)*concentrations"]))
      input.source$emf.dt.conc.bulk <- TRUE
    
  }
  
  # constants
  
  if (nrow(as.data.table(input$file.emf.bulk.input)[name %like% "^(input\\_)*k\\_constants\\_log10(\\.csv|\\.txt)*"]) > 0){
    input.source$emf.cnst.bulk <- TRUE
  }
  
  if (nrow(as.data.table(input$file.emf.bulk.input)[name %like% "\\.xlsx$"]) > 0){
    
    shts <- getSheetNames(input$file.emf.bulk.input$datapath)
    
    if (length(shts[shts %like% "^((input_)*k_constants_log10|constants_evaluated)"]))
      input.source$emf.cnst.bulk <- TRUE
    
  }
  
  # emf
  
  if (nrow(as.data.table(input$file.emf.bulk.input)[name %like% "^(input\\_)*emf(\\.csv|\\.txt)*"]) > 0){
    input.source$dt.emf.bulk <- TRUE
  }
  
  if (nrow(as.data.table(input$file.emf.bulk.input)[name %like% "\\.xlsx$"]) > 0){
    
    shts <- getSheetNames(input$file.emf.bulk.input$datapath)
    
    if (length(shts[shts %like% "emf"]))
      input.source$dt.emf.bulk <- TRUE
    
  }
  
  # emf params coefficients
  
  if (nrow(as.data.table(input$file.emf.bulk.input)[name %like% "^(input\\_)*targets*(\\.csv|\\.txt)*$"]) > 0)
    input.source$emf.dt.params.bulk <- TRUE
  
  if (nrow(as.data.table(input$file.emf.bulk.input)[name %like% "\\.xlsx$"]) > 0){
    
    shts <- getSheetNames(input$file.emf.bulk.input$datapath)
    
    if (length(shts[shts %like% "^targets*"]))
      input.source$emf.dt.params.bulk <- TRUE
    
    
  }
  
  
  
}, priority = 1000)

observeEvent(input$file.emf.dt.coef, {
  
  input.source$emf.dt.coef.bulk <- FALSE
  
}, priority = 1000)

observeEvent(input$file.emf.dt.conc, {
  
  input.source$emf.dt.conc.bulk <- FALSE
  
}, priority = 1000)

observeEvent(input$file.emf.cnst, {
  
  input.source$emf.cnst.bulk <- FALSE
  
}, priority = 1000)

observeEvent(input$file.dt.emf, {
  
  input.source$dt.emf.bulk <- FALSE
  
}, priority = 1000)

observeEvent(input$file.emf.dt.params, {
  
  input.source$emf.dt.params.bulk <- FALSE
  
}, priority = 1000)



# data --------------------- #

# input data

emf.part.names.data <- reactive({
  
  tmp <- input$emf.part.names
  
  tmp <- str_split(tmp, pattern = ",")[[1]]
  tmp <- str_trim(tmp)
  
  tmp
  
})

emf.dt.coef.data <- server_dt.coef.data("emf")

emf.dt.conc.data <- server_dt.conc.data("emf")

emf.part.eq.data <- server_part.eq.data("emf")

emf.cnst.data <- server_cnst.data("emf")

dt.emf.data <- reactive({
  
  if (!is.null(input$dt.emf)) {
    
    dt.emf <- hot_to_r(input$dt.emf)
    
  } else {
    
    if (is.null(values[["dt.emf"]])) {
      
      dt.emf <- matrix(rep(100, 10), 2)
      dt.emf[(nrow(dt.emf) / 2 + 1):nrow(dt.emf), 1:ncol(dt.emf)] <- .01
      
      dt.emf <- data.table(data = c(rep("observation", nrow(dt.emf) / 2), rep("deviation", nrow(dt.emf) / 2))
                           , particle = rep("molecule1", 2)
                           , dt.emf)
      
      cln <- colnames(dt.emf)
      cln <- cln[cln %like% "^V[0-9]"]
      
      setnames(dt.emf, cln, paste0("S", 1:length(cln)))
      
    } else {
      
      dt.emf <- values[["dt.emf"]]
      
    }
    
  }
  
  dt.emf <- as.data.table(dt.emf)
  
  values[["dt.emf"]] <- dt.emf
  
  dt.emf
  
})

plot.dt.emf.data <- eventReactive(input$emf.conc.exec.btn, {
  
  # get data
  
  dt.calc <- copy(emf.eval.data()$dt.emf.calc)
  
  dt.obs <- dt.emf.data()[data %like% "^observ"]
  dt.obs[, data := "Observed"]
  
  dt.obs[, particle := NULL]
  dt.calc[, particle := NULL]
  
  # unify column names
  
  cln <- colnames(dt.calc)
  setnames(dt.obs, c("data", cln))
  
  dt.calc[, data := "Calculated"]
  
  # melt
  
  dt.calc <- melt(dt.calc, id.vars = c("data"), variable.name = "solution", value.name = "EMF")
  dt.obs <- melt(dt.obs, id.vars = c("data"), variable.name = "solution", value.name = "EMF")
  
  # convert observed EMF to numerics if not
  
  dt.obs[, EMF := as.character(EMF)]
  dt.obs[, EMF := str_replace_all(EMF, " ", "")]
  dt.obs[, EMF := str_replace_all(EMF, "\\,", "\\.")]
  dt.obs[, EMF := as.numeric(EMF)]
  
  # bind
  
  dt <- rbind(dt.obs, dt.calc, use.names = TRUE, fill = TRUE)
  
  # return
  
  dt
  
})


emf.dt.params.data <- reactive({
  
  if (!is.null(input$emf.dt.params)) {
    
    emf.dt.params <- hot_to_r(input$emf.dt.params)
    
  } else {
    
    if (is.null(values[["emf.dt.params"]])) {
      
      emf.dt.params <- data.table(Parameter = c("standard.potential", "slope"), Value = c(388, 59))
      
    } else {
      
      emf.dt.params <- values[["emf.dt.params"]]
      
    }
    
  }
  
  emf.dt.params <- as.data.table(emf.dt.params)
  
  values[["emf.dt.params"]] <- emf.dt.params
  
  emf.dt.params
  
})

# data returns data
emf.cnst.tune.data <- reactive({
  
  if (!is.null(input$emf.cnst.tune)) {
    
    cnst.tune <- input$emf.cnst.tune
    cnst.tune <- str_split(cnst.tune, "\\, *")
    cnst.tune <- unlist(cnst.tune)
    
  } else {
    
    if (is.null(values[["emf.cnst.tune"]])) {
      
      cnst.tune <- "molecule1"
      
    } else {
      
      cnst.tune <- values[["emf.cnst.tune"]]
      
    }
    
  }
  
  values[["emf.cnst.tune"]] <- cnst.tune
  
  cnst.tune
  
})

# load only updates textinput
emf.cnst.tune.load <- reactive({
  
  in.file <- input$file.emf.dt.params
  in.file.bulk <- input$file.emf.bulk.input
  in.file.xlsx <- NULL
  
  # bulk input
  
  if (input.source$emf.dt.params.bulk) {
    
    in.file <- as.data.table(input$file.emf.bulk.input)[name %like% "^(input\\_)*targets*(\\.csv|\\.txt)*$"][1]
    in.file <- as.data.frame(in.file)
    
    in.file.xlsx <- as.data.table(input$file.emf.bulk.input)[name %like% "\\.xlsx$"]
    
    if (nrow(in.file.xlsx) > 0) {
      
      in.file.xlsx <- as.data.frame(in.file.xlsx[1])
      
    } else {
      
      in.file.xlsx <- NULL
      
    }
    
    if (!is.null(in.file.xlsx))
      in.file <- NULL
    
  }
  
  # choose source
  
  if (!is.null(in.file)) {
    
    if (emf.sep() == ";") {
      cnst.tune <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", header = FALSE), silent = TRUE)
    } else if (emf.sep() == ",") {
      cnst.tune <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", header = FALSE), silent = TRUE)
    } else if (emf.sep() == "tab") {
      cnst.tune <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", header = FALSE), silent = TRUE)
    }
    
    setDT(cnst.tune)
    cnst.tune[1, V1 := str_replace(V1, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0x2e), as.raw(0xbf)))), "")]
    setnames(cnst.tune, "V1", "X1")
    
    
  } else if (!is.null(in.file.xlsx)) {
    
    sht <- getSheetNames(in.file.xlsx$datapath[1])
    sht <- sht[sht %like% "^(constants*_names*|targets*)"]
    
    cnst.tune <- try(read.xlsx(in.file.xlsx$datapath, sheet = sht, colNames = FALSE), silent = TRUE)
    
    
  } else {
    
    cnst.tune <- values[["emf.cnst.tune"]]
    
  }
  
  # new format
  
  setDT(cnst.tune)
  
  if (nrow(cnst.tune[X1 == "constant"]) > 0) {
    
    cnst.tune <- cnst.tune[X1 == "constant"][, !"X1", with = FALSE]
    cnst.tune <- unlist(cnst.tune)
    cnst.tune <- cnst.tune[!is.na(cnst.tune) & cnst.tune != ""]
    
  }
  
  cnst.tune <- unlist(cnst.tune)
  
  values[["emf.cnst.tune"]] <- cnst.tune
  updateTextInput(session, "emf.cnst.tune", value = paste(cnst.tune, collapse = ", "))
  
})

# to save results
emf.target.data <- reactive({
  
  target <- list(constant = emf.cnst.tune.data()
                 , standard.potential = as.character(emf.dt.params.data()[Parameter == "standard.potential", Value])
                 , slope = as.character(emf.dt.params.data()[Parameter == "slope", Value]))
  target <- setDT(lapply(target, "length<-", max(lengths(target))))[]
  
  target[is.na(constant), constant := ""]
  target[is.na(standard.potential), standard.potential := ""]
  target[is.na(slope), slope := ""]
  
  target <- as.data.table(t(target), keep.rownames = TRUE)
  
  cln <- target[rn == "constant"] %>% unlist
  target <- target[2:nrow(target)]
  
  setnames(target, cln)
  
  target
  
  
})


# execute

emf.eval.data <- reactive({
  
  withProgress(message = "Computation... It may take some time", value = 0, {
    
    incProgress(.1)
    
    particles <- c(colnames(emf.dt.coef.data()), emf.dt.coef.data()[, name])
    
    validate(
      
      need(length(particles %in% cnst.tune.data()) > 0, "Input correct component names for constants evaluation")
      
    )
    
    incProgress(.3)
    
    # run
    
    res <- emf.evaluation.runner(mode = "app"
                                 , sep = emf.sep()
                                 , eq.thr.type = "rel"
                                 , eq.threshold = 1e-08
                                 , cnst.tune = emf.cnst.tune.data()
                                 , algorithm = "direct search"
                                 , emf.mode = "base"
                                 , method = "basic wls"
                                 , search.density = as.numeric(input$emf.search.density)
                                 , lrate.init = .5
                                 , emf.threshold = as.numeric(input$emf.threshold)
                                 , dt.list = list(dt.coef = emf.dt.coef.data()
                                                  , cnst = emf.cnst.data()
                                                  , dt.conc = emf.dt.conc.data()
                                                  , part.eq = emf.part.eq.data()
                                                  , dt.emf = dt.emf.data()
                                                  , dt.params = emf.dt.params.data())
                                 , save.res = FALSE)
    
    incProgress(.6)
    
  })
  
  res
  
})


# output data

emf.dt.res.data <- eventReactive(input$emf.conc.exec.btn, {
  
  emf.eval.data()$dt.eq.conc
  
})

dt.emf.abs.data <- eventReactive(input$emf.conc.exec.btn, {
  
  dt <- emf.eval.data()$dt.emf.calc
  dt.err <- emf.eval.data()$emf.res.abs
  
  dt.comb <- data.table(data = c(rep("observation", nrow(dt)), rep("error", nrow(dt.err)))
                        , rbind(dt, dt.err, use.names = TRUE))
  
  dt.comb
  
})

dt.emf.rel.data <- eventReactive(input$emf.conc.exec.btn, {
  
  dt <- emf.eval.data()$dt.emf.calc
  dt.err <- emf.eval.data()$emf.res.rel
  
  dt.comb <- data.table(data = c(rep("observation", nrow(dt)), rep("error", nrow(dt.err)))
                        , rbind(dt, dt.err, use.names = TRUE))
  
  dt.comb
  
})

emf.cnst.dev.data <- eventReactive(input$emf.conc.exec.btn, {
  
  cnst.dev <- emf.eval.data()$cnst.dev
  cnst.dev <- as.data.table(cnst.dev)
  
  setnames(cnst.dev, c("Component", "Constant", "St.Deviation", "Validity"))
  
})

emf.cor.m.data <- eventReactive(input$emf.conc.exec.btn, {
  
  cor.m <- emf.eval.data()$cor.m
  
  cor.m
  
})

emf.adj.r.squared.data <- eventReactive(input$emf.conc.exec.btn, {
  
  emf.adj.r.squared <- emf.eval.data()$adj.r.squared
  emf.adj.r.squared <- data.table(`Adj. R^2` = emf.adj.r.squared)
  
  emf.adj.r.squared
  
})


# rendering ---------------- #

output$emf.dt.coef <- server_render_dt.coef("emf")

output$emf.dt.conc <- server_render_dt.conc("emf")

output$emf.part.eq <- server_render_part.eq("emf")

output$emf.cnst <- server_render_cnst("emf")

output$dt.emf <- renderRHandsontable({
  
  in.file <- input$file.dt.emf
  in.file.bulk <- input$file.emf.bulk.input
  in.file.xlsx <- NULL
  
  # bulk input
  
  if (input.source$dt.emf.bulk) {
    
    in.file <- as.data.table(input$file.emf.bulk.input)[name %like% "^(input\\_)*emf(\\.csv|\\.txt)*$"][1]
    in.file <- as.data.frame(in.file)
    
    in.file.xlsx <- as.data.table(input$file.emf.bulk.input)[name %like% "\\.xlsx$"]
    
    if (nrow(in.file.xlsx) > 0) {
      
      in.file.xlsx <- as.data.frame(in.file.xlsx[1])
      
    } else {
      
      in.file.xlsx <- NULL
      
    }
    
    if (!is.null(in.file.xlsx))
      in.file <- NULL
    
  }
  
  # choose source
  
  if (!is.null(in.file)) {
    
    if (emf.sep() == ";") {
      dt.emf <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
    } else if (emf.sep() == ",") {
      dt.emf <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
    } else if (emf.sep() == "tab") {
      dt.emf <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
    }
    
    setDT(dt.emf)
    
    cln <- colnames(dt.emf)
    setnames(dt.emf, cln, str_replace(cln, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0x2e), as.raw(0xbf)))), ""))
    
    validate(
      
      need(is.data.frame(dt.emf), "Your file doesn't look like an EMF file")
      
    )
    
    
  } else if (!is.null(in.file.xlsx)) {
    
    shts <- getSheetNames(in.file.xlsx$datapath)
    
    shts <- shts[shts %like% "(input_|output_)*emf"]
    shts <- sort(shts, decreasing = TRUE)
    
    dt.emf <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1]), silent = TRUE)
    
    validate(
      
      need(is.data.frame(dt.emf), "Your file doesn't look like an EMF file")
      
    )
    
  } else {
    
    dt.emf <- dt.emf.data()
    
  }
  
  if (!is.null(dt.emf)) {
    
    if (nrow(dt.emf) > 25) {
      
      rhandsontable(dt.emf, stretchH = "all", useTypes = FALSE, height = 600) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
      
    } else {
      
      rhandsontable(dt.emf, stretchH = "all", useTypes = FALSE, height = NULL) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
      
    }
    
  }
  
  
  
})

output$emf.dt.params <- renderRHandsontable({
  
  in.file <- input$file.emf.dt.params
  in.file.bulk <- input$file.emf.bulk.input
  in.file.xlsx <- NULL
  
  try(emf.cnst.tune.load(), silent = TRUE)
  
  # bulk input
  
  if (input.source$emf.dt.params.bulk) {
    
    in.file <- as.data.table(input$file.emf.bulk.input)[name %like% "^(input\\_)*targets*(\\.csv|\\.txt)*$"][1]
    in.file <- as.data.frame(in.file)
    
    in.file.xlsx <- as.data.table(input$file.emf.bulk.input)[name %like% "\\.xlsx$"]
    
    if (nrow(in.file.xlsx) > 0) {
      
      in.file.xlsx <- as.data.frame(in.file.xlsx[1])
      
    } else {
      
      in.file.xlsx <- NULL
      
    }
    
    if (!is.null(in.file.xlsx))
      in.file <- NULL
    
  }
  
  # choose source
  
  if (!is.null(in.file)) {
    
    if (emf.sep() == ";") {
      
      emf.dt.params <- try(as.data.table(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character"
                                                    , check.names = FALSE, sep = emf.sep(), dec = ",", header = FALSE)
                                         , keep.rownames = FALSE), silent = TRUE)
      
    } else if (emf.sep() == ",") {
      
      emf.dt.params <- try(as.data.table(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character"
                                                    , check.names = FALSE, sep = emf.sep(), dec = ".", header = FALSE)
                                         , keep.rownames = FALSE), silent = TRUE)
      
    } else if (emf.sep() == "tab") {
      
      
      emf.dt.params <- try(as.data.table(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character"
                                                    , check.names = FALSE, sep = "\t", dec = ".", header = FALSE)
                                         , keep.rownames = FALSE), silent = TRUE)
      
    }
    
    setDT(emf.dt.params)
    
    cln <- colnames(emf.dt.params)
    setnames(emf.dt.params, cln, str_replace(cln, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0x2e), as.raw(0xbf)))), ""))
    
    emf.dt.params <- emf.dt.params[V1 %in% c("standard.potential", "slope")]
    emf.dt.params <- emf.dt.params[, 1:2, with = FALSE]
    
    setnames(emf.dt.params, c("Parameter", "Value"))
    
  } else if (!is.null(in.file.xlsx)) {
    
    shts <- getSheetNames(in.file.xlsx$datapath)
    
    shts <- shts[shts %like% "^(input_|output_)*targets*$"]
    shts <- sort(shts)
    
    emf.dt.params <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1], colNames = FALSE), silent = TRUE)
    
    setDT(emf.dt.params)
    
    emf.dt.params <- emf.dt.params[X1 %in% c("standard.potential", "slope")]
    emf.dt.params <- emf.dt.params[, 1:2, with = FALSE]
    
    setnames(emf.dt.params, c("Parameter", "Value"))
    
  } else {
    
    emf.dt.params <- emf.dt.params.data()
    
  }
  
  if (!is.null(emf.dt.params)) {
    
    if (nrow(emf.dt.params) > 25) {
      
      rhandsontable(emf.dt.params, stretchH = "all", useTypes = FALSE, height = 500) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
      
    } else {
      
      rhandsontable(emf.dt.params, stretchH = "all", useTypes = FALSE, height = NULL) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
      
    }
    
  }
  
})

output$emf.dt.res <- server_render_dt.res("emf")

output$dt.emf.abs <- renderRHandsontable({
  
  dt.emf.abs <- dt.emf.abs.data()
  
  if (!is.null(dt.emf.abs)) {
    
    row_highlight <- dt.emf.abs[data == "observation", which = TRUE] - 1
    
    renderer <- "
      function (instance, td, row, col, prop, value, cellProperties) {
      
        Handsontable.renderers.TextRenderer.apply(this, arguments);
      
      }" 
    
    if (nrow(dt.emf.abs) > 25) {
      
      rhandsontable(dt.emf.abs, stretchH = "all", row_highlight = row_highlight, height = 550) %>%
        hot_cols(renderer = renderer)
      
    } else {
      
      rhandsontable(dt.emf.abs, stretchH = "all", row_highlight = row_highlight, height = NULL) %>%
        hot_cols(renderer = renderer)
      
    }
    
  }
  
})

output$dt.emf.rel <- renderRHandsontable({
  
  dt.emf.rel <- dt.emf.rel.data()
  
  if (!is.null(dt.emf.rel)) {
    
    row_highlight <- dt.emf.rel[data == "observation", which = TRUE] - 1
    
    renderer <- "
      function (instance, td, row, col, prop, value, cellProperties) {
      
        Handsontable.renderers.TextRenderer.apply(this, arguments);
      
      }" 
    
    if (nrow(dt.emf.rel) > 25) {
      
      rhandsontable(dt.emf.rel, stretchH = "all", row_highlight = row_highlight, height = 550) %>%
        hot_cols(renderer = renderer)
      
    } else {
      
      rhandsontable(dt.emf.rel, stretchH = "all", row_highlight = row_highlight, height = NULL) %>%
        hot_cols(renderer = renderer)
      
    }
    
  }
  
})

output$emf.cnst.dev <- renderRHandsontable({
  
  cnst.dev <- emf.cnst.dev.data()
  
  if (!is.null(cnst.dev))
    
    row_highlight <- cnst.dev[Validity != "OK", which = TRUE] - 1
  
  renderer <- "
    function (instance, td, row, col, prop, value, cellProperties) {
    
    Handsontable.renderers.TextRenderer.apply(this, arguments);
    
    if (instance.params) {
    hrows = instance.params.row_highlight
    hrows = hrows instanceof Array ? hrows : [hrows]
    }
    
    if (instance.params && hrows.includes(row)) {
    td.style.background = 'pink';
    }
    
    }" 
  
  
  rhandsontable(cnst.dev, stretchH = FALSE, row_highlight = row_highlight, useTypes = TRUE) %>%
    hot_cols(renderer = renderer)
  
})

output$emf.cor.m <- renderRHandsontable({
  
  cor.m <- emf.cor.m.data()
  
  if (!is.null(cor.m))
    
    rhandsontable(cor.m, stretchH = FALSE, useTypes = FALSE) %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  
})

output$emf.adj.r.squared <- renderRHandsontable({
  
  emf.adj.r.squared <- emf.adj.r.squared.data()
  
  if (!is.null(emf.adj.r.squared))
    
    rhandsontable(emf.adj.r.squared, stretchH = FALSE, useTypes = FALSE) %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  
})

output$plot.dt.emf <- renderPlotly({
  
  dt <- plot.dt.emf.data()
  
  g <- ggplot(data = dt) +
    geom_point(aes(x = solution, y = EMF, group = data, color = data), size = 1) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Solution", y = "EMF")
  
  g <- ggplotly(g)
  g[["x"]][["layout"]][["annotations"]][[1]][["y"]] <- -0.15
  g <- g %>% plotly::layout(margin = list(b = 100, t = 50))
  
  # g$x$data[[1]]$hoverinfo <- "none"
  
  g
  
})






