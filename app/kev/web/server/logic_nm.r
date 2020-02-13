# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #



# nmr (fast) -------------------------------------------------------


# technical

nm.sep <- reactive({
  
  switch(input$nm.sep,
         comma = ",",
         semicolon = ";",
         tab = "tab")
  
})

observeEvent(input$file.nm.bulk.input, {
  
  # stoichiometric coefficients
  
  if (nrow(as.data.table(input$file.nm.bulk.input)[name %like% "^(input\\_)*stoich(iometric)*\\_coefficients(\\.csv|\\.txt)*"]) > 0){
    input.source$nm.dt.coef.bulk <- TRUE
  }
  
  if (nrow(as.data.table(input$file.nm.bulk.input)[name %like% "\\.xlsx$"]) > 0){
    
    shts <- getSheetNames(input$file.nm.bulk.input$datapath)
    
    if (length(shts[shts %like% "stoich(iometric)*_coefficients"]))
      input.source$nm.dt.coef.bulk <- TRUE
    
  }
  
  # concentrations
  
  if (nrow(as.data.table(input$file.nm.bulk.input)[name %like% "^(input\\_)*concentrations(\\.csv|\\.txt)*"]) > 0){
    input.source$nm.dt.conc.bulk <- TRUE
  }
  
  if (nrow(as.data.table(input$file.nm.bulk.input)[name %like% "\\.xlsx$"]) > 0){
    
    shts <- getSheetNames(input$file.nm.bulk.input$datapath)
    
    if (length(shts[shts %like% "^(input_|output_)*concentrations"]))
      input.source$nm.dt.conc.bulk <- TRUE
    
  }
  
  # constants
  
  if (nrow(as.data.table(input$file.nm.bulk.input)[name %like% "^(input\\_)*k\\_constants\\_log10(\\.csv|\\.txt)*"]) > 0){
    input.source$nm.cnst.bulk <- TRUE
  }
  
  if (nrow(as.data.table(input$file.nm.bulk.input)[name %like% "\\.xlsx$"]) > 0){
    
    shts <- getSheetNames(input$file.nm.bulk.input$datapath)
    
    if (length(shts[shts %like% "^((input_)*k_constants_log10|constants_evaluated)"]))
      input.source$nm.cnst.bulk <- TRUE
    
  }
  
  # chemical shifts
  
  if (nrow(as.data.table(input$file.nm.bulk.input)[name %like% "^(input\\_)*chemical\\_shifts*(\\.csv|\\.txt)*"]) > 0){
    input.source$dt.nm.bulk <- TRUE
  }
  
  if (nrow(as.data.table(input$file.nm.bulk.input)[name %like% "\\.xlsx$"]) > 0){
    
    shts <- getSheetNames(input$file.nm.bulk.input$datapath)
    
    if (length(shts[shts %like% "chemical\\_shifts*" & !(shts %like% "ind(ividual)*\\_(chemical\\_)*shifts*")]))
      input.source$dt.nm.bulk <- TRUE
    
  }
  
  # individual chemical shifts
  
  if (nrow(as.data.table(input$file.nm.bulk.input)[name %like% "^(input\\_)*ind(ividual)*\\_(chemical\\_)*shifts*(\\.csv|\\.txt)*"]) > 0){
    input.source$nm.dt.ind.bulk <- TRUE
    input.source$nm.dt.ind.memory <- FALSE
  }
  
  if (nrow(as.data.table(input$file.nm.bulk.input)[name %like% "\\.xlsx$"]) > 0){
    
    shts <- getSheetNames(input$file.nm.bulk.input$datapath)
    
    if (length(shts[shts %like% "^(input\\_)*ind(ividual)*\\_(chemical\\_)*shifts*"])){
      
      input.source$nm.dt.ind.bulk <- TRUE
      input.source$nm.dt.ind.memory <- FALSE
      
    }
    
  }
  
  
  
}, priority = 1000)

observeEvent(input$file.nm.dt.coef, {
  
  input.source$nm.dt.coef.bulk <- FALSE
  
}, priority = 1000)

observeEvent(input$file.nm.dt.conc, {
  
  input.source$nm.dt.conc.bulk <- FALSE
  
}, priority = 1000)

observeEvent(input$file.nm.cnst, {
  
  input.source$nm.cnst.bulk <- FALSE
  
}, priority = 1000)

observeEvent(input$file.dt.nm, {
  
  input.source$dt.nm.bulk <- FALSE
  
}, priority = 1000)

observeEvent(input$file.nm.dt.ind, {
  
  input.source$nm.dt.ind.bulk <- FALSE
  input.source$nm.dt.ind.memory <- FALSE
  
}, priority = 1000)



# data --------------------- #

# input data

nm.part.names.data <- reactive({
  
  tmp <- input$nm.part.names
  
  tmp <- str_split(tmp, pattern = ",")[[1]]
  tmp <- str_trim(tmp)
  
  tmp
  
})

nm.dt.coef.data <- server_dt.coef.data("nm")

nm.dt.conc.data <- server_dt.conc.data("nm")

nm.part.eq.data <- server_part.eq.data("nm")

nm.cnst.data <- server_cnst.data("nm")

dt.nm.data <- reactive({
  
  if (!is.null(input$dt.nm)) {
    
    dt.nm <- hot_to_r(input$dt.nm)
    
  } else {
    
    if (is.null(values[["dt.nm"]])) {
      
      dt.nm <- matrix(rep(3, 30), 6)
      dt.nm[(nrow(dt.nm) / 2 + 1):nrow(dt.nm), 1:ncol(dt.nm)] <- .005
      
      dt.nm <- data.table(data = c(rep("observation", nrow(dt.nm) / 2), rep("deviation", nrow(dt.nm) / 2))
                          , particle = rep("molecule1", nrow(dt.nm))
                          , signal = c(letters[1:(nrow(dt.nm) / 2)], letters[1:(nrow(dt.nm) / 2)])
                          , dt.nm)
      
      cln <- colnames(dt.nm)
      cln <- cln[cln %like% "^V[0-9]"]
      
      setnames(dt.nm, cln, paste0("S", 1:length(cln)))
      
    } else {
      
      dt.nm <- values[["dt.nm"]]
      
    }
    
  }
  
  dt.nm <- as.data.table(dt.nm)
  
  values[["dt.nm"]] <- dt.nm
  
  dt.nm
  
})

nm.dt.ind.data <- reactive({
  
  if (!is.null(input$nm.dt.ind)) {
    
    nm.dt.ind <- hot_to_r(input$nm.dt.ind)
    
    if (!is.null(input$nm.dt.ind.colnames) && length(input$nm.dt.ind.colnames) == ncol(nm.dt.ind))
      colnames(nm.dt.ind) <- input$nm.dt.ind.colnames
    
  } else {
    
    if (is.null(values[["nm.dt.ind"]])) {
      
      nm.dt.ind <- as.data.table(matrix(rep(3, 3), 3))
      nm.dt.ind <- data.table(signal = letters[1:nrow(nm.dt.ind)], nm.dt.ind)
      
      cln <- colnames(nm.dt.ind)
      cln <- cln[cln %like% "^V[0-9]"]
      
      setnames(nm.dt.ind, cln, paste0("molecule", 1:length(cln)))
      
    } else {
      
      nm.dt.ind <- values[["nm.dt.ind"]]
      
    }
    
  }
  
  nm.dt.ind <- as.data.table(nm.dt.ind)
  
  values[["nm.dt.ind"]] <- nm.dt.ind
  
  nm.dt.ind
  
})

# data returns data
nm.cnst.tune.data <- reactive({
  
  if (!is.null(input$nm.cnst.tune)) {
    
    cnst.tune <- input$nm.cnst.tune
    cnst.tune <- str_split(cnst.tune, "\\, *")
    cnst.tune <- unlist(cnst.tune)
    
  } else {
    
    if (is.null(values[["nm.cnst.tune"]])) {
      
      cnst.tune <- "molecule1"
      
    } else {
      
      cnst.tune <- values[["nm.cnst.tune"]]
      
    }
    
  }
  
  values[["nm.cnst.tune"]] <- cnst.tune
  
  cnst.tune
  
})

# load only updates textinput
nm.cnst.tune.load <- reactive({
  
  in.file.bulk <- input$file.nm.bulk.input
  in.file.xlsx <- NULL
  in.file <- NULL
  
  # bulk input
  
  if (nrow(as.data.table(input$file.nm.bulk.input)[name %like% "^(constants*_names*|targets*)(\\.csv|\\.txt)*"]) > 0){
    
    in.file <- as.data.table(input$file.nm.bulk.input)[name %like% "^(constants*_names*|targets*)(\\.csv|\\.txt)*"][1]
    in.file <- as.data.frame(in.file)
    
  }
  
  in.file.xlsx <- as.data.table(input$file.nm.bulk.input)[name %like% "\\.xlsx$"]
  
  if (nrow(in.file.xlsx) > 0) {
    
    in.file.xlsx <- as.data.frame(in.file.xlsx[1])
    
  } else {
    
    in.file.xlsx <- NULL
    
  }
  
  if (!is.null(in.file.xlsx))
    in.file <- NULL
  
  if (!is.null(in.file)) {
    
    if (nm.sep() == ";") {
      cnst.tune <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", header = FALSE), silent = TRUE)
    } else if (nm.sep() == ",") {
      cnst.tune <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", header = FALSE), silent = TRUE)
    } else if (nm.sep() == "tab") {
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
    
    cnst.tune <- values[["nm.cnst.tune"]]
    
  }
  
  # new format
  
  setDT(cnst.tune)
  
  if (nrow(cnst.tune[X1 %like% "^constants*$"]) > 0) {
    
    cnst.tune <- cnst.tune[X1 %like% "^constants*$"][, !"X1", with = FALSE]
    cnst.tune <- unlist(cnst.tune)
    cnst.tune <- cnst.tune[!is.na(cnst.tune) & cnst.tune != ""]
    
  }
  
  cnst.tune <- unlist(cnst.tune)
  
  values[["nm.cnst.tune"]] <- cnst.tune
  updateTextInput(session, "nm.cnst.tune", value = paste(cnst.tune, collapse = ", "))
  
})

# to save results
nm.target.data <- reactive({
  
  target <- list(constant = nm.cnst.tune.data())
  target <- setDT(lapply(target, "length<-", max(lengths(target))))[]
  
  target[is.na(constant), constant := ""]
  
  target <- as.data.table(t(target), keep.rownames = TRUE)
  
  cln <- target[rn == "constant"] %>% unlist
  
  setnames(target, cln)
  
  target <- target[0]
  
  target
  
  
})


# execute

nm.eval.data <- reactive({
  
  withProgress(message = "Computation... It may take some time", value = 0, {
    
    incProgress(.1)
    
    particles <- c(colnames(nm.dt.coef.data()), nm.dt.coef.data()[, name])
    
    validate(
      
      need(length(particles %in% nm.cnst.tune.data()) > 0, "Input correct component names for constants evaluation")
      
    )
    
    dt.nm <- dt.nm.data()
    
    validate(
      
      need(identical(as.data.table(dt.nm)[data %like% "observ", signal] %>% sort, as.data.table(dt.nm)[data %like% "deviat", signal] %>% sort)
           , "Signal names in Observation part are inconsistent with ones in Deviation part")
      
    )
    
    
    # check if no molar extinction coefficients are known
    
    nm.dt.ind <- nm.dt.ind.data()
    
    if (ncol(nm.dt.ind) <= 1)
      nm.dt.ind <- NA
    
    incProgress(.3)
    
    # run
    
    res <- nm.evaluation.runner(mode = "app"
                                , sep = nm.sep()
                                , eq.thr.type = "rel"
                                , eq.threshold = 1e-08
                                , cnst.tune = nm.cnst.tune.data()
                                , algorithm = "direct search"
                                , nm.mode = "base"
                                , method = "basic wls"
                                , search.density = as.numeric(input$nm.search.density)
                                , lrate.init = .5
                                , nm.threshold = as.numeric(input$nm.threshold)
                                , dt.list = list(dt.coef = nm.dt.coef.data()
                                                 , cnst = nm.cnst.data()
                                                 , dt.conc = nm.dt.conc.data()
                                                 , part.eq = nm.part.eq.data()
                                                 , dt.nm = dt.nm
                                                 , dt.ind = nm.dt.ind)
                                , save.res = FALSE)
    
    incProgress(.6)
    
  })
  
  res
  
})


# output data

nm.dt.res.data <- eventReactive(input$nm.conc.exec.btn, {
  
  nm.eval.data()$dt.eq.conc
  
})

dt.nm.abs.data <- eventReactive(input$nm.conc.exec.btn, {
  
  dt <- nm.eval.data()$dt.nm.calc
  dt.err <- nm.eval.data()$nm.res.abs
  
  dt.comb <- data.table(data = c(rep("observation", nrow(dt)), rep("error", nrow(dt.err)))
                        , rbind(dt, dt.err, use.names = TRUE))
  
  dt.comb
  
})

dt.nm.rel.data <- eventReactive(input$nm.conc.exec.btn, {
  
  dt <- nm.eval.data()$dt.nm.calc
  dt.err <- nm.eval.data()$nm.res.rel
  
  dt.comb <- data.table(data = c(rep("observation", nrow(dt)), rep("error", nrow(dt.err)))
                        , rbind(dt, dt.err, use.names = TRUE))
  
  dt.comb
  
})

plot.dt.nm.data <- eventReactive(input$nm.conc.exec.btn, {
  
  # get data
  
  dt.calc <- copy(nm.eval.data()$dt.nm.calc)
  
  dt.obs <- dt.nm.data()[data %like% "^observ"]
  dt.obs[, data := "Observed"]
  
  # unify column names
  
  cln <- colnames(dt.calc)
  cln <- cln[!(cln %in% c("particle", "signal", "data"))]
  
  setnames(dt.obs, c("data", "particle", "signal", cln))
  
  dt.calc[, data := "Calculated"]
  
  # melt
  
  dt.calc <- melt(dt.calc, id.vars = c("particle", "signal", "data"), variable.name = "solution", value.name = "chem_shift")
  dt.obs <- melt(dt.obs, id.vars = c("particle", "signal", "data"), variable.name = "solution", value.name = "chem_shift")
  
  # convert observed chemical shifts to numerics if not
  
  dt.obs[, chem_shift := as.character(chem_shift)]
  dt.obs[, chem_shift := str_replace_all(chem_shift, " ", "")]
  dt.obs[, chem_shift := str_replace_all(chem_shift, "\\,", "\\.")]
  dt.obs[, chem_shift := as.numeric(chem_shift)]
  
  # bind
  
  dt <- rbind(dt.obs, dt.calc, use.names = TRUE, fill = TRUE)
  
  # convert solution to numeric if not
  
  dt[, solution := as.character(solution)]
  dt[, solution := str_replace_all(solution, " ", "")]
  dt[, solution := str_replace_all(solution, "\\,", "\\.")]
  dt[, solution := as.numeric(solution)]
  
  # return
  
  dt
  
})

nm.cnst.dev.data <- eventReactive(input$nm.conc.exec.btn, {
  
  cnst.dev <- nm.eval.data()$cnst.dev
  cnst.dev <- as.data.table(cnst.dev)
  
  setnames(cnst.dev, c("Component", "Constant", "St.Deviation", "Validity"))
  
})

nm.cor.m.data <- eventReactive(input$nm.conc.exec.btn, {
  
  cor.m <- nm.eval.data()$cor.m
  
  cor.m
  
})

nm.ind.shift.data <- eventReactive(input$nm.conc.exec.btn, {
  
  dt <- nm.eval.data()$ind.shift
  dt.err <- nm.eval.data()$ind.shift.dev
  
  dt.comb <- data.table(data = c(rep("observation", nrow(dt)), rep("error", nrow(dt.err)))
                        , rbind(dt, dt.err, use.names = TRUE))
  
  dt.comb
  
})

nm.adj.r.squared.data <- eventReactive(input$nm.conc.exec.btn, {
  
  nm.adj.r.squared <- nm.eval.data()$adj.r.squared
  nm.adj.r.squared <- data.table(`Adj. R^2` = nm.adj.r.squared)
  
  nm.adj.r.squared
  
})


# rendering ---------------- #

output$nm.dt.coef <- server_render_dt.coef("nm")

output$nm.dt.conc <- server_render_dt.conc("nm")

output$nm.part.eq <- server_render_part.eq("nm")

output$nm.cnst <- server_render_cnst("nm")

output$dt.nm <- renderRHandsontable({
  
  in.file <- input$file.dt.nm
  in.file.bulk <- input$file.nm.bulk.input
  in.file.xlsx <- NULL
  
  # bulk input
  
  if (input.source$dt.nm.bulk) {
    
    in.file <- as.data.table(input$file.nm.bulk.input)[name %like% "^(input\\_)*chemical\\_shifts*(\\.csv|\\.txt)*$"][1]
    in.file <- as.data.frame(in.file)
    
    in.file.xlsx <- as.data.table(input$file.nm.bulk.input)[name %like% "\\.xlsx$"]
    
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
    
    if (nm.sep() == ";") {
      dt.nm <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
    } else if (nm.sep() == ",") {
      dt.nm <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
    } else if (nm.sep() == "tab") {
      dt.nm <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
    }
    
    setDT(dt.nm)
    
    cln <- colnames(dt.nm)
    setnames(dt.nm, cln, str_replace(cln, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0x2e), as.raw(0xbf)))), ""))
    
    validate(
      
      need(is.data.frame(dt.nm), "Your file doesn't look like an chemical shifts file")
      
    )
    
    
  } else if (!is.null(in.file.xlsx)) {
    
    shts <- getSheetNames(in.file.xlsx$datapath)
    
    shts <- shts[shts %like% "chemical\\_shifts*" & !(shts %like% "ind(ividual)*\\_(chemical\\_)*shifts*")]
    shts <- sort(shts, decreasing = TRUE)
    
    dt.nm <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1]), silent = TRUE)
    
    validate(
      
      need(is.data.frame(dt.nm), "Your file doesn't look like an chemical shifts file")
      
    )
    
  } else {
    
    dt.nm <- dt.nm.data()
    
  }
  
  if (!is.null(dt.nm)) {
    
    if (nrow(dt.nm) > 25) {
      
      rhandsontable(dt.nm, stretchH = "all", useTypes = FALSE, height = 600) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
      
    } else {
      
      rhandsontable(dt.nm, stretchH = "all", useTypes = FALSE, height = NULL) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
      
    }
    
  }
  
  
  
})

output$nm.dt.ind <- renderRHandsontable({
  
  in.file <- input$file.nm.dt.ind
  in.file.bulk <- input$file.nm.bulk.input
  in.file.xlsx <- NULL
  
  # bulk input
  
  if (input.source$nm.dt.ind.bulk) {
    
    in.file <- as.data.table(input$file.nm.bulk.input)[name %like% "^(input\\_)*ind(ividual)*\\_(chemical\\_)*shifts*(\\.csv|\\.txt)*$"][1]
    in.file <- as.data.frame(in.file)
    
    in.file.xlsx <- as.data.table(input$file.nm.bulk.input)[name %like% "\\.xlsx$"]
    
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
    
    if (nm.sep() == ";") {
      nm.dt.ind <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
    } else if (nm.sep() == ",") {
      nm.dt.ind <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
    } else if (nm.sep() == "tab") {
      nm.dt.ind <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
    }
    
    if (is.data.frame(nm.dt.ind)) {
      
      setDT(nm.dt.ind)
      
      cln <- colnames(nm.dt.ind)
      setnames(nm.dt.ind, cln, str_replace(cln, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0x2e), as.raw(0xbf)))), ""))
      
    }
    
    if (is.data.frame(nm.dt.ind) && nrow(nm.dt.ind) == 0) {
      
      nm.dt.ind <- NA
      
    }
    
    
  } else if (!is.null(in.file.xlsx)) {
    
    shts <- getSheetNames(in.file.xlsx$datapath)
    
    shts <- shts[shts %like% "^(input\\_)*ind(ividual)*\\_(chemical\\_)*shifts*"]
    shts <- sort(shts)
    
    nm.dt.ind <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1]), silent = TRUE)
    
  } else {
    
    nm.dt.ind <- nm.dt.ind.data()
    
  }
  
  if (!is.data.frame(nm.dt.ind))
    nm.dt.ind <- data.frame(no.data = "no.data")
  
  if (!is.null(nm.dt.ind)) {
    
    if (nrow(nm.dt.ind) > 25) {
      
      rhandsontable(nm.dt.ind, stretchH = "all", useTypes = FALSE, height = 500) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
      
    } else {
      
      rhandsontable(nm.dt.ind, stretchH = "all", useTypes = FALSE, height = NULL) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE
                         , customOpts = list(dt_mol_rename_column =
                                               list(name = "Change column name"
                                                    , callback = htmlwidgets::JS(
                                                      "function (key, options) {
                                                        
                                                        const visualIndex = options.start.col;
                                                        const logicalIndex = this.runHooks('modifyCol', visualIndex);
                                                        
                                                        var res = prompt('Type new column name');
                                                        //res = JSON.stringify(res);
                                                        
                                                        if (res === null) {
                                                        return;
                                                        }
                                                        var instance = this;
                                                        
                                                        var headers = instance.getColHeader();
                                                        headers[logicalIndex] = res;
                                                        
                                                        instance.updateSettings({
                                                        colHeaders: headers
                                                        });
                                                        
                                                        this.render();
                                                        Shiny.onInputChange('nm.dt.ind.colnames', headers);
                                                        //this.view.wt.wtOverlays.adjustElementsSize(true);
      }")
                                               )))
      
    }
    
  }
  
})

output$nm.dt.res <- server_render_dt.res("nm")

output$dt.nm.abs <- renderRHandsontable({
  
  dt.nm.abs <- dt.nm.abs.data()
  
  if (!is.null(dt.nm.abs)) {
    
    row_highlight <- dt.nm.abs[data == "observation", which = TRUE] - 1
    
    renderer <- "
      function (instance, td, row, col, prop, value, cellProperties) {
      
      Handsontable.renderers.TextRenderer.apply(this, arguments);
      
      if (instance.params) {
      hrows = instance.params.row_highlight
      hrows = hrows instanceof Array ? hrows : [hrows]
      }
      
      }" 
    
    if (nrow(dt.nm.abs) > 25) {
      
      rhandsontable(dt.nm.abs, stretchH = "all", row_highlight = row_highlight, height = 550) %>%
        hot_cols(renderer = renderer)
      
    } else {
      
      rhandsontable(dt.nm.abs, stretchH = "all", row_highlight = row_highlight, height = NULL) %>%
        hot_cols(renderer = renderer)
      
    }
    
  }
  
})

output$dt.nm.rel <- renderRHandsontable({
  
  dt.nm.rel <- dt.nm.rel.data()
  
  if (!is.null(dt.nm.rel)) {
    
    row_highlight <- dt.nm.rel[data == "observation", which = TRUE] - 1
    
    renderer <- "
      function (instance, td, row, col, prop, value, cellProperties) {
      
      Handsontable.renderers.TextRenderer.apply(this, arguments);
      
      if (instance.params) {
      hrows = instance.params.row_highlight
      hrows = hrows instanceof Array ? hrows : [hrows]
      }
      
      }" 
    
    if (nrow(dt.nm.rel) > 25) {
      
      rhandsontable(dt.nm.rel, stretchH = "all", row_highlight = row_highlight, height = 550) %>%
        hot_cols(renderer = renderer)
      
    } else {
      
      rhandsontable(dt.nm.rel, stretchH = "all", row_highlight = row_highlight, height = NULL) %>%
        hot_cols(renderer = renderer)
      
    }
    
  }
  
})

output$nm.cnst.dev <- server_render_cnst.dev("nm")

output$nm.cor.m <- server_render_cor.m("nm")

output$nm.ind.shift <- renderRHandsontable({
  
  nm.ind.shift <- nm.ind.shift.data()
  
  if (!is.null(nm.ind.shift)) {
    
    row_highlight <- nm.ind.shift[data == "observation", which = TRUE] - 1
    
    renderer <- "
        function (instance, td, row, col, prop, value, cellProperties) {
        
        Handsontable.renderers.TextRenderer.apply(this, arguments);
        
        if (instance.params) {
        hrows = instance.params.row_highlight
        hrows = hrows instanceof Array ? hrows : [hrows]
        }
        
      }" 
    
    if (nrow(nm.ind.shift) > 25) {
      
      rhandsontable(nm.ind.shift, stretchH = "all", row_highlight = row_highlight, height = 550) %>%
        hot_cols(renderer = renderer)
      
    } else {
      
      rhandsontable(nm.ind.shift, stretchH = "all", row_highlight = row_highlight, height = NULL) %>%
        hot_cols(renderer = renderer)
      
    }
    
  }
  
})

output$nm.adj.r.squared <- server_render_adj.r.squared("nm")

output$plot.dt.nm <- renderPlotly({
  
  dt <- plot.dt.nm.data()
  
  g <- ggplot(data = dt) +
    geom_point(aes(x = solution, y = chem_shift, group = data, color = data), size = .5) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_grid(. ~ signal) +
    labs(x = "Solution", y = "Chemical Shifts")
  
  g <- ggplotly(g)
  g[["x"]][["layout"]][["annotations"]][[1]][["y"]] <- -0.15
  g <- g %>% plotly::layout(margin = list(b = 100, t = 50))
  
  # g$x$data[[1]]$hoverinfo <- "none"
  
  g
  
})



