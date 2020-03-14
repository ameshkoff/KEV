# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2020                                                 #
#                                                            #
# ########################################################## #



# calorimetry -------------------------------------------------------


# technical

ht.sep <- reactive({
  
  switch(input$ht.sep,
         comma = ",",
         semicolon = ";",
         tab = "tab")
  
})

observeEvent(input$file.ht.bulk.input, {
  
  # stoichiometric coefficients
  
  if (nrow(as.data.table(input$file.ht.bulk.input)[name %like% "^(input\\_)*stoich(iometric)*\\_coefficients*(\\.csv|\\.txt)*"]) > 0){
    input.source$ht.dt.coef.bulk <- TRUE
  }
  
  if (nrow(as.data.table(input$file.ht.bulk.input)[name %like% "\\.xlsx$"]) > 0){
    
    shts <- getSheetNames(input$file.ht.bulk.input$datapath)
    
    if (length(shts[shts %like% "stoich(iometric)*_coefficients*"]))
      input.source$ht.dt.coef.bulk <- TRUE
    
  }
  
  # concentrations
  
  if (nrow(as.data.table(input$file.ht.bulk.input)[name %like% "^(input\\_)*concentrations*(\\.csv|\\.txt)*"]) > 0){
    input.source$ht.dt.conc.bulk <- TRUE
  }
  
  if (nrow(as.data.table(input$file.ht.bulk.input)[name %like% "\\.xlsx$"]) > 0){
    
    shts <- getSheetNames(input$file.ht.bulk.input$datapath)
    
    if (length(shts[shts %like% "^(input_|output_)*concentrations*"]))
      input.source$ht.dt.conc.bulk <- TRUE
    
  }
  
  # constants
  
  if (nrow(as.data.table(input$file.ht.bulk.input)[name %like% "^(input\\_)*k\\_constants*\\_log10|constants*_evaluated(\\.csv|\\.txt)*"]) > 0){
    input.source$ht.cnst.bulk <- TRUE
  }
  
  if (nrow(as.data.table(input$file.ht.bulk.input)[name %like% "\\.xlsx$"]) > 0){
    
    shts <- getSheetNames(input$file.ht.bulk.input$datapath)
    
    if (length(shts[shts %like% "^((input_)*k_constants*_log10|constants*_evaluated)"]))
      input.source$ht.cnst.bulk <- TRUE
    
  }
  
  # heats
  
  if (nrow(as.data.table(input$file.ht.bulk.input)[name %like% "^(input\\_)*heats*(\\.csv|\\.txt)*"]) > 0){
    input.source$ht.dt.heat.bulk <- TRUE
  }
  
  if (nrow(as.data.table(input$file.ht.bulk.input)[name %like% "\\.xlsx$"]) > 0){
    
    shts <- getSheetNames(input$file.ht.bulk.input$datapath)
    
    if (length(shts[shts %like% "^(input\\_)*heats*"]))
      input.source$ht.dt.heat.bulk <- TRUE
    
  }
  
  # enthalpies
  
  if (nrow(as.data.table(input$file.ht.bulk.input)[name %like% "^(input\\_)*enth*alp(y|ie)s*(\\.csv|\\.txt)*"]) > 0){
    input.source$ht.dt.enth.bulk <- TRUE
  }
  
  if (nrow(as.data.table(input$file.ht.bulk.input)[name %like% "\\.xlsx$"]) > 0){
    
    shts <- getSheetNames(input$file.ht.bulk.input$datapath)
    
    if (length(shts[shts %like% "^(input\\_)*enth*alp(y|ie)s*"])){
      
      input.source$ht.dt.enth.bulk <- TRUE

    }
    
  }
  
  
  
}, priority = 1000)

observeEvent(input$file.ht.dt.coef, {
  
  input.source$ht.dt.coef.bulk <- FALSE
  
}, priority = 1000)

observeEvent(input$file.ht.dt.conc, {
  
  input.source$ht.dt.conc.bulk <- FALSE
  
}, priority = 1000)

observeEvent(input$file.ht.cnst, {
  
  input.source$ht.cnst.bulk <- FALSE
  
}, priority = 1000)

observeEvent(input$file.ht.dt.heat, {
  
  input.source$ht.dt.heat.bulk <- FALSE
  
}, priority = 1000)

observeEvent(input$file.ht.dt.enth, {
  
  input.source$ht.dt.enth.bulk <- FALSE

}, priority = 1000)



# data --------------------- #

# input data

ht.part.names.data <- reactive({
  
  tmp <- input$ht.part.names
  
  tmp <- str_split(tmp, pattern = ",")[[1]]
  tmp <- str_trim(tmp)
  
  tmp
  
})

ht.dt.coef.data <- server_dt.coef.data("ht")

ht.dt.conc.data <- server_dt.conc.data("ht")

ht.part.eq.data <- server_part.eq.data("ht")

ht.cnst.data <- server_cnst.data("ht")

ht.dt.heat.data <- reactive({
  
  if (!is.null(input$ht.dt.heat)) {
    
    ht.dt.heat <- hot_to_r(input$ht.dt.heat)
    
  } else {
    
    if (is.null(values[["ht.dt.heat"]])) {
      
      ht.dt.heat <- data.table(data = 1:4
                          , volumes = seq(10, 10.75, .25)
                          , observation = rnorm(4, sd = .1)
                          , dilution = rnorm(4, sd = .01)
                          , deviation = 1e-6)
      
    } else {
      
      ht.dt.heat <- values[["ht.dt.heat"]]
      
    }
    
  }
  
  ht.dt.heat <- as.data.table(ht.dt.heat)
  
  values[["ht.dt.heat"]] <- ht.dt.heat
  
  ht.dt.heat
  
})

ht.dt.enth.data <- reactive({
  
  if (!is.null(input$ht.dt.enth)) {
    
    ht.dt.enth <- hot_to_r(input$ht.dt.enth)
    
  } else {
    
    if (is.null(values[["ht.dt.enth"]])) {
      
      ht.dt.enth <- data.table(reaction = "molecule1"
                               , value = 0)
      
    } else {
      
      ht.dt.enth <- values[["ht.dt.enth"]]
      
    }
    
  }
  
  ht.dt.enth <- as.data.table(ht.dt.enth)
  
  values[["ht.dt.enth"]] <- ht.dt.enth
  
  ht.dt.enth
  
})

# setup data

ht.cnst.tune.data <- reactive({
  
  if (!is.null(input$ht.cnst.tune)) {
    
    cnst.tune <- input$ht.cnst.tune
    cnst.tune <- str_split(cnst.tune, "\\, *")
    cnst.tune <- unlist(cnst.tune)
    
  } else {
    
    if (is.null(values[["ht.cnst.tune"]])) {
      
      cnst.tune <- "molecule1"
      
    } else {
      
      cnst.tune <- values[["ht.cnst.tune"]]
      
    }
    
  }
  
  values[["ht.cnst.tune"]] <- cnst.tune
  
  cnst.tune
  
})

ht.cmp.tune.data <- reactive({
  
  if (!is.null(input$ht.cmp.tune)) {
    
    cmp.tune <- input$ht.cmp.tune
    cmp.tune <- str_split(cmp.tune, "\\, *")
    cmp.tune <- unlist(cmp.tune)
    
  } else {
    
    if (is.null(values[["ht.cmp.tune"]])) {
      
      cmp.tune <- "molecule1"
      
    } else {
      
      cmp.tune <- values[["ht.cmp.tune"]]
      
    }
    
  }
  
  values[["ht.cmp.tune"]] <- cmp.tune
  
  cmp.tune
  
})

ht.calorimeter.type.data <- reactive({
  
  if (!is.null(input$ht.calorimeter.type)) {
    
    calorimeter.type <- input$ht.calorimeter.type

  } else {
    
    if (is.null(values[["ht.calorimeter.type"]])) {
      
      calorimeter.type <- "DSC"
      
    } else {
      
      calorimeter.type <- values[["ht.calorimeter.type"]]
      
    }
    
  }
  
  values[["ht.calorimeter.type"]] <- calorimeter.type
  
  calorimeter.type
  
})

ht.init.vol.data <- reactive({
  
  if (!is.null(input$ht.init.vol)) {
    
    init.vol <- input$ht.init.vol

  } else {
    
    if (is.null(values[["ht.init.vol"]])) {
      
      init.vol <- 1
      
    } else {
      
      init.vol <- values[["ht.init.vol"]]
      
    }
    
  }
  
  values[["ht.init.vol"]] <- init.vol
  
  init.vol
  
})

# update setup input fields

ht.setup.load <- reactive({
  
  in.file.bulk <- input$file.ht.bulk.input
  in.file.xlsx <- NULL
  in.file <- NULL
  
  # bulk input
  
  if (nrow(as.data.table(input$file.ht.bulk.input)[name %like% "^(constants*_names*|targets*|setup)(\\.csv|\\.txt)*"]) > 0){
    
    in.file <- as.data.table(input$file.ht.bulk.input)[name %like% "^(constants*_names*|targets*|setup)(\\.csv|\\.txt)*"][1]
    in.file <- as.data.frame(in.file)
    
  }
  
  in.file.xlsx <- as.data.table(input$file.ht.bulk.input)[name %like% "\\.xlsx$"]
  
  if (nrow(in.file.xlsx) > 0) {
    
    in.file.xlsx <- as.data.frame(in.file.xlsx[1])
    
  } else {
    
    in.file.xlsx <- NULL
    
  }
  
  if (!is.null(in.file.xlsx))
    in.file <- NULL
  
  if (!is.null(in.file)) {
    
    if (ht.sep() == ";") {
      dt.setup <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", header = FALSE), silent = TRUE)
    } else if (ht.sep() == ",") {
      dt.setup <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", header = FALSE), silent = TRUE)
    } else if (ht.sep() == "tab") {
      dt.setup <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", header = FALSE), silent = TRUE)
    }
    
    dt.setup <- ht.load.extract.setup(list(setup = as.data.table(dt.setup)))

    cnst.tune <- as.character(dt.setup[["cnst.tune"]])
    cmp.tune <- as.character(dt.setup[["cmp.tune"]])
    calorimeter.type <- as.character(dt.setup[["calorimeter.type"]])
    init.vol <- as.numeric(dt.setup[["init.vol"]])
    
  } else if (!is.null(in.file.xlsx)) {
    
    sht <- getSheetNames(in.file.xlsx$datapath[1])
    sht <- sht[sht %like% "^(constants*_names*|targets*|setup)"]
    
    dt.setup <- try(read.xlsx(in.file.xlsx$datapath, sheet = sht, colNames = FALSE), silent = TRUE)
    
    dt.setup <- ht.load.extract.setup(list(setup = as.data.table(dt.setup)))

    cnst.tune <- as.character(dt.setup[["cnst.tune"]])
    cmp.tune <- as.character(dt.setup[["cmp.tune"]])
    calorimeter.type <- as.character(dt.setup[["calorimeter.type"]])
    init.vol <- as.numeric(dt.setup[["init.vol"]])
    
  } else {
    
    cnst.tune <- values[["cnst.tune"]]
    cmp.tune <- values[["cmp.tune"]]
    calorimeter.type <- values[["calorimeter.type"]]
    init.vol <- values[["init.vol"]]
    
  }
  
  values[["ht.cnst.tune"]] <- cnst.tune 
  values[["ht.cmp.tune"]] <- cmp.tune
  values[["ht.init.vol"]] <- init.vol 
  
  calorimeter.type <- str_to_upper(calorimeter.type)
  if (calorimeter.type != "DSC") calorimeter.type <- str_to_title(calorimeter.type)
  values[["ht.calorimeter.type"]] <- calorimeter.type
  
  updateTextInput(session, "ht.cnst.tune", value = paste(cnst.tune, collapse = ", "))
  updateTextInput(session, "ht.cmp.tune", value = cmp.tune)
  updateSelectInput(session, "ht.calorimeter.type", selected = calorimeter.type)
  updateNumericInput(session, "ht.init.vol", value = init.vol)
  
})


# execute

ht.eval.data <- reactive({
  
  withProgress(message = "Computation... It may take some time", value = 0, {
    
    incProgress(.1)
    
    particles <- c(colnames(ht.dt.coef.data()), ht.dt.coef.data()[, name])
    
    validate(
      
      need(length(particles %in% ht.cnst.tune.data()) > 0, "Input correct component names for constants evaluation")
      
    )
    
    ht.dt.heat <- ht.dt.heat.data()
    dt.conc <- ht.dt.conc.data()
    
    heat.length <- nrow(dt.conc)
    
    if (length(colnames(dt.conc)[str_to_lower(colnames(dt.conc)) == "series"]) > 0) {
      
      heat.length <- heat.length - length(unique(unlist(dt.conc[, colnames(dt.conc)[str_to_lower(colnames(dt.conc)) == "series"], with = FALSE])))
      
    } else {
      
      heat.length <- heat.length - 1
      
    }
    # browser()
    validate(
      
      need(identical(nrow(ht.dt.heat), as.integer(heat.length))
           , "Number of rows in Heats should equal to number of rows in Concentrations - number of Observation series")
      
    )
    
    # check if no enthalpies are known
    
    ht.dt.enth <- ht.dt.enth.data()
    
    if (nrow(ht.dt.enth) <= 1)
      ht.dt.enth <- NA
    
    #
    
    cnst.tune <- ht.cnst.tune.data()
    if (length(cnst.tune) == 1 && cnst.tune == "") cnst.tune <- NULL
    
    incProgress(.3)
    
    # run
    
    res <- ht.evaluation.runner(mode = "app"
                                , sep = ht.sep()
                                , eq.thr.type = "rel"
                                , eq.threshold = 1e-08
                                , cnst.tune = cnst.tune
                                , algorithm = "direct search"
                                , ht.mode = "base"
                                , method = "basic wls"
                                , search.density = as.numeric(input$ht.search.density)
                                , lrate.init = .5
                                , ht.threshold = as.numeric(input$ht.threshold)
                                , dt.list = list(dt.coef = ht.dt.coef.data()
                                                 , cnst = ht.cnst.data()
                                                 , dt.conc = dt.conc
                                                 , part.eq = ht.part.eq.data()
                                                 , dt.heat = ht.dt.heat
                                                 , dt.enth = ht.dt.enth
                                                 , cmp.tune = ht.cmp.tune.data()
                                                 , calorimeter.type = ht.calorimeter.type.data()
                                                 , init.vol = ht.init.vol.data())
                                , save.res = FALSE)
    
    incProgress(.6)
    
  })
  
  res
  
})


# output data

ht.dt.res.data <- eventReactive(input$ht.conc.exec.btn, {
  
  ht.eval.data()$dt.eq.conc
  
})

ht.dt.heat.calc.data <- eventReactive(input$ht.conc.exec.btn, {
  
  ht.eval.data()$dt.heat.calc

})

plot.ht.dt.heat.data <- eventReactive(input$ht.conc.exec.btn, {
  
  # get data
  
  dt <- ht.eval.data()$dt.heat.calc[, .(Data = data, Observed = heats, Calculated = heats.calculated)]
  
  # return
  
  dt
  
})

ht.cnst.dev.data <- eventReactive(input$ht.conc.exec.btn, {
  
  cnst.dev <- ht.eval.data()$cnst.dev

  cnst.dev
    
})

ht.cor.m.data <- eventReactive(input$ht.conc.exec.btn, {
  
  cor.m <- ht.eval.data()$cor.m
  
  cor.m
  
})

ht.dt.enth.calc.data <- eventReactive(input$ht.conc.exec.btn, {
  
  dt <- ht.eval.data()$dt.enth.calc

  dt
  
})

ht.dt.metrics.data <- eventReactive(input$ht.conc.exec.btn, {
  
  ht.dt.metrics <- ht.eval.data()$dt.metrics

  ht.dt.metrics
  
})


# rendering ---------------- #

output$ht.dt.coef <- server_render_dt.coef("ht")

output$ht.dt.conc <- server_render_dt.conc("ht")

output$ht.part.eq <- server_render_part.eq("ht")

output$ht.cnst <- server_render_cnst("ht")

output$ht.dt.heat <- renderRHandsontable({
  
  in.file <- input$file.ht.dt.heat
  in.file.bulk <- input$file.ht.bulk.input
  in.file.xlsx <- NULL
  
  # bulk input
  
  if (input.source$ht.dt.heat.bulk) {
    
    in.file <- as.data.table(input$file.ht.bulk.input)[name %like% "^(input\\_)*heats*(\\.csv|\\.txt)*"][1]
    in.file <- as.data.frame(in.file)
    
    in.file.xlsx <- as.data.table(input$file.ht.bulk.input)[name %like% "\\.xlsx$"]
    
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
    
    if (ht.sep() == ";") {
      ht.dt.heat <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
    } else if (ht.sep() == ",") {
      ht.dt.heat <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
    } else if (ht.sep() == "tab") {
      ht.dt.heat <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
    }
    
    setDT(ht.dt.heat)
    
    cln <- colnames(ht.dt.heat)
    setnames(ht.dt.heat, cln, str_replace(cln, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0x2e), as.raw(0xbf)))), ""))
    
    validate(
      
      need(is.data.frame(ht.dt.heat), "Your file doesn't look like a heats file")
      
    )
    
    
  } else if (!is.null(in.file.xlsx)) {
    
    shts <- getSheetNames(in.file.xlsx$datapath)
    
    shts <- shts[shts %like% "^(input\\_)*heats*"]
    shts <- sort(shts, decreasing = TRUE)
    
    ht.dt.heat <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1]), silent = TRUE)
    
    validate(
      
      need(is.data.frame(ht.dt.heat), "Your file doesn't look like a heats file")
      
    )
    
  } else {
    
    ht.dt.heat <- ht.dt.heat.data()
    
  }
  
  if (!is.null(ht.dt.heat)) {
    
    if (nrow(ht.dt.heat) > 25) {
      
      rhandsontable(ht.dt.heat, stretchH = "all", useTypes = FALSE, height = 600) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
      
    } else {
      
      rhandsontable(ht.dt.heat, stretchH = "all", useTypes = FALSE, height = NULL) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
      
    }
    
  }
  
  
  
})

output$ht.dt.enth <- renderRHandsontable({
  
  in.file <- input$file.ht.dt.enth
  in.file.bulk <- input$file.ht.bulk.input
  in.file.xlsx <- NULL
  
  # bulk input
  
  if (input.source$ht.dt.enth.bulk) {
    
    in.file <- as.data.table(input$file.ht.bulk.input)[name %like% "^(input\\_)*enth*alp(y|ie)s*(\\.csv|\\.txt)*$"][1]
    in.file <- as.data.frame(in.file)
    
    in.file.xlsx <- as.data.table(input$file.ht.bulk.input)[name %like% "\\.xlsx$"]
    
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
    
    if (ht.sep() == ";") {
      ht.dt.enth <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
    } else if (ht.sep() == ",") {
      ht.dt.enth <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
    } else if (ht.sep() == "tab") {
      ht.dt.enth <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
    }
    
    if (is.data.frame(ht.dt.enth)) {
      
      setDT(ht.dt.enth)
      
      cln <- colnames(ht.dt.enth)
      setnames(ht.dt.enth, cln, str_replace(cln, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0x2e), as.raw(0xbf)))), ""))
      
    }
    
  } else if (!is.null(in.file.xlsx)) {
    
    shts <- getSheetNames(in.file.xlsx$datapath)
    
    shts <- shts[shts %like% "^(input\\_)*enth*alp(y|ie)s*"]
    shts <- sort(shts)
    
    ht.dt.enth <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1]), silent = TRUE)
    
  } else {
    
    ht.dt.enth <- ht.dt.enth.data()
    
  }
  
  if (!is.data.frame(ht.dt.enth) || nrow(ht.dt.enth) == 0)
    ht.dt.enth <- data.frame(no.data = "no.data")
  
  if (!is.null(ht.dt.enth)) {
    
    if (nrow(ht.dt.enth) > 25) {
      
      rhandsontable(ht.dt.enth, stretchH = "all", useTypes = FALSE, height = 500) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
      
    } else {
      
      rhandsontable(ht.dt.enth, stretchH = "all", useTypes = FALSE, height = NULL) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
      
    }
    
  }
  
})

output$ht.dt.res <- server_render_dt.res("ht")

output$ht.dt.heat.calc <- renderRHandsontable({
  
  ht.dt.heat.calc <- ht.dt.heat.calc.data()

  if (!is.null(ht.dt.heat.calc)) {
    
    row_highlight <- ht.dt.heat.calc[abs(res.abs / mean(heats)) >= .25, which = TRUE] - 1
    
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
    
    if (nrow(ht.dt.heat.calc) > 25) {
      
      rhandsontable(ht.dt.heat.calc, stretchH = "all", row_highlight = row_highlight, height = 550) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_cols(renderer = renderer)
      
    } else {
      
      rhandsontable(ht.dt.heat.calc, stretchH = "all", row_highlight = row_highlight, height = NULL) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_cols(renderer = renderer)
      
    }
    
  }
  
})

output$ht.cnst.dev <- server_render_cnst.dev("ht")

output$ht.cor.m <- server_render_cor.m("ht")

output$ht.dt.enth.calc <- renderRHandsontable({
  
  ht.dt.enth.calc <- ht.dt.enth.calc.data()
  
  if (!is.null(ht.dt.enth.calc)) {
    
    row_highlight <- ht.dt.enth.calc[abs(dev / value) > .25, which = TRUE] - 1
    
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
    
    if (nrow(ht.dt.enth.calc) > 25) {
      
      rhandsontable(ht.dt.enth.calc, stretchH = "all", row_highlight = row_highlight, height = 550) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_cols(renderer = renderer)
      
    } else {
      
      rhandsontable(ht.dt.enth.calc, stretchH = "all", row_highlight = row_highlight, height = NULL) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
        hot_cols(renderer = renderer)
      
    }
    
  }
  
})

output$ht.dt.metrics <- server_render_dt.metrics("ht")

output$plot.ht.dt.heat <- renderPlotly({
  
  dt <- plot.ht.dt.heat.data()
  
  dt <- melt(dt, id.vars = c("Data"))
  
  g <- ggplot(data = dt) +
    geom_point(aes(x = Data, y = value, group = variable, color = variable), size = .5) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    guides(color = guide_legend(title = "")) +
    labs(x = "Solution", y = "Heats")
  
  g <- ggplotly(g)
  g[["x"]][["layout"]][["annotations"]][[1]][["y"]] <- -0.15
  g <- g %>% plotly::layout(margin = list(b = 100, t = 50))
  
  g
  
})



