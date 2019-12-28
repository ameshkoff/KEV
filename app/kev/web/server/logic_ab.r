# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #



# absorbance -------------------------------------------------------


# technical

ab.sep <- reactive({
  
  switch(input$ab.sep,
         comma = ",",
         semicolon = ";",
         tab = "tab")
  
})

observeEvent(input$file.ab.bulk.input, {
  
  # stoichiometric coefficients
  
  if (nrow(as.data.table(input$file.ab.bulk.input)[name %like% "^(input\\_)*stoich(iometric)*\\_coefficients(\\.csv|\\.txt)*"]) > 0){
    input.source$ab.dt.coef.bulk <- TRUE
  }
  
  if (nrow(as.data.table(input$file.ab.bulk.input)[name %like% "\\.xlsx$"]) > 0){
    
    shts <- getSheetNames(input$file.ab.bulk.input$datapath)
    
    if (length(shts[shts %like% "stoich(iometric)*_coefficients"]))
      input.source$ab.dt.coef.bulk <- TRUE
    
  }
  
  # concentrations
  
  if (nrow(as.data.table(input$file.ab.bulk.input)[name %like% "^(input\\_)*concentrations(\\.csv|\\.txt)*"]) > 0){
    input.source$ab.dt.conc.bulk <- TRUE
  }
  
  if (nrow(as.data.table(input$file.ab.bulk.input)[name %like% "\\.xlsx$"]) > 0){
    
    shts <- getSheetNames(input$file.ab.bulk.input$datapath)
    
    if (length(shts[shts %like% "^(input_|output_)*concentrations"]))
      input.source$ab.dt.conc.bulk <- TRUE
    
  }
  
  # constants
  
  if (nrow(as.data.table(input$file.ab.bulk.input)[name %like% "^(input\\_)*k\\_constants\\_log10(\\.csv|\\.txt)*"]) > 0){
    input.source$ab.cnst.bulk <- TRUE
  }
  
  if (nrow(as.data.table(input$file.ab.bulk.input)[name %like% "\\.xlsx$"]) > 0){
    
    shts <- getSheetNames(input$file.ab.bulk.input$datapath)
    
    if (length(shts[shts %like% "^((input_)*k_constants_log10|constants_evaluated)"]))
      input.source$ab.cnst.bulk <- TRUE
    
  }
  
  # absorbance
  
  if (nrow(as.data.table(input$file.ab.bulk.input)[name %like% "^(input\\_)*absorbance(\\.csv|\\.txt)*"]) > 0){
    input.source$dt.ab.bulk <- TRUE
  }
  
  if (nrow(as.data.table(input$file.ab.bulk.input)[name %like% "\\.xlsx$"]) > 0){
    
    shts <- getSheetNames(input$file.ab.bulk.input$datapath)
    
    if (length(shts[shts %like% "absorbance"]))
      input.source$dt.ab.bulk <- TRUE
    
  }
  
  # molar extinction coefficients
  
  if (nrow(as.data.table(input$file.ab.bulk.input)[name %like% "^(input\\_)*mol(ar)*\\_ext(inction)*\\_coefficients(\\.csv|\\.txt)*"]) > 0){
    input.source$dt.mol.bulk <- TRUE
    input.source$dt.mol.memory <- FALSE
  }
  
  if (nrow(as.data.table(input$file.ab.bulk.input)[name %like% "\\.xlsx$"]) > 0){
    
    shts <- getSheetNames(input$file.ab.bulk.input$datapath)
    
    if (length(shts[shts %like% "mol(ar)*_ext(inction)*_coefficients"])){
      
      input.source$dt.mol.bulk <- TRUE
      input.source$dt.mol.memory <- FALSE
      
    }
    
  }
  
  
  
}, priority = 1000)

observeEvent(input$file.ab.dt.coef, {
  
  input.source$ab.dt.coef.bulk <- FALSE
  
}, priority = 1000)

observeEvent(input$file.ab.dt.conc, {
  
  input.source$ab.dt.conc.bulk <- FALSE
  
}, priority = 1000)

observeEvent(input$file.ab.cnst, {
  
  input.source$ab.cnst.bulk <- FALSE
  
}, priority = 1000)

observeEvent(input$file.dt.ab, {
  
  input.source$dt.ab.bulk <- FALSE
  
}, priority = 1000)

observeEvent(input$file.dt.mol, {
  
  input.source$dt.mol.bulk <- FALSE
  input.source$dt.mol.memory <- FALSE
  
}, priority = 1000)

observeEvent(input$dt.mol.memory, {
  
  input.source$dt.mol.memory <- TRUE
  
}, priority = 1000)



# data --------------------- #

# input data

ab.part.names.data <- reactive({
  
  tmp <- input$ab.part.names
  
  tmp <- str_split(tmp, pattern = ",")[[1]]
  tmp <- str_trim(tmp)
  
  tmp
  
})

ab.dt.coef.data <- server_dt.coef.data("ab")

ab.dt.conc.data <- server_dt.conc.data("ab")

ab.part.eq.data <- server_part.eq.data("ab")

ab.cnst.data <- server_cnst.data("ab")

dt.ab.data <- reactive({
  
  if (!is.null(input$dt.ab)) {
    
    dt.ab <- hot_to_r(input$dt.ab)
    
  } else {
    
    if (is.null(values[["dt.ab"]])) {
      
      dt.ab <- matrix(rep(100, 30), 6)
      dt.ab[(nrow(dt.ab) / 2 + 1):nrow(dt.ab), 1:ncol(dt.ab)] <- .001
      
      dt.ab <- data.table(data = c(rep("observation", nrow(dt.ab) / 2), rep("deviation", nrow(dt.ab) / 2))
                          , wavelength = c(100 + seq(10, 10 * nrow(dt.ab) / 2, 10), 100 + seq(10, 10 * nrow(dt.ab) / 2, 10))
                          , dt.ab)
      
      cln <- colnames(dt.ab)
      cln <- cln[cln %like% "^V[0-9]"]
      
      setnames(dt.ab, cln, paste0("S", 1:length(cln)))
      
    } else {
      
      dt.ab <- values[["dt.ab"]]
      
    }
    
  }
  
  dt.ab <- as.data.table(dt.ab)
  
  values[["dt.ab"]] <- dt.ab
  
  dt.ab
  
})

dt.mol.data <- reactive({
  
  if (!is.null(input$dt.mol)) {
    
    dt.mol <- hot_to_r(input$dt.mol)
    
    if (!is.null(input$dt.mol.colnames) && length(input$dt.mol.colnames) == ncol(dt.mol))
      colnames(dt.mol) <- input$dt.mol.colnames
    
  } else {
    
    if (is.null(values[["dt.mol"]])) {
      
      dt.mol <- as.data.table(matrix(rep(0, 6), 3))
      dt.mol <- data.table(wavelength = 100 + seq(10, 10 * nrow(dt.mol), 10), dt.mol)
      
      cln <- colnames(dt.mol)
      cln <- cln[cln %like% "^V[0-9]"]
      
      setnames(dt.mol, cln, paste0("molecule", 1:length(cln)))
      
    } else {
      
      dt.mol <- values[["dt.mol"]]
      
    }
    
  }
  
  dt.mol <- as.data.table(dt.mol)
  
  values[["dt.mol"]] <- dt.mol
  
  dt.mol
  
})

# data returns data
cnst.tune.data <- reactive({
  
  if (!is.null(input$cnst.tune)) {
    
    cnst.tune <- input$cnst.tune
    cnst.tune <- str_split(cnst.tune, "\\, *")
    cnst.tune <- unlist(cnst.tune)
    
  } else {
    
    if (is.null(values[["cnst.tune"]])) {
      
      cnst.tune <- "molecule1"
      
    } else {
      
      cnst.tune <- values[["cnst.tune"]]
      
    }
    
  }
  
  values[["cnst.tune"]] <- cnst.tune
  
  cnst.tune
  
})

# load only updates textinput
ab.cnst.tune.load <- reactive({
  
  in.file.bulk <- input$file.ab.bulk.input
  in.file.xlsx <- NULL
  in.file <- NULL
  
  # bulk input
  
  if (nrow(as.data.table(input$file.ab.bulk.input)[name %like% "^(constants*_names*|targets*)(\\.csv|\\.txt)*"]) > 0){
    
    in.file <- as.data.table(input$file.ab.bulk.input)[name %like% "^(constants*_names*|targets*)(\\.csv|\\.txt)*"][1]
    in.file <- as.data.frame(in.file)
    
  }
  
  in.file.xlsx <- as.data.table(input$file.ab.bulk.input)[name %like% "\\.xlsx$"]
  
  if (nrow(in.file.xlsx) > 0) {
    
    in.file.xlsx <- as.data.frame(in.file.xlsx[1])
    
  } else {
    
    in.file.xlsx <- NULL
    
  }
  
  if (!is.null(in.file.xlsx))
    in.file <- NULL
  
  if (!is.null(in.file)) {
    
    if (ab.sep() == ";") {
      cnst.tune <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", header = FALSE), silent = TRUE)
    } else if (ab.sep() == ",") {
      cnst.tune <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", header = FALSE), silent = TRUE)
    } else if (ab.sep() == "tab") {
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
    
    cnst.tune <- values[["cnst.tune"]]
    
  }
  
  # new format
  
  setDT(cnst.tune)
  
  if (nrow(cnst.tune[X1 == "constant"]) > 0) {
    
    cnst.tune <- cnst.tune[X1 == "constant"][, !"X1", with = FALSE]
    cnst.tune <- unlist(cnst.tune)
    cnst.tune <- cnst.tune[!is.na(cnst.tune) & cnst.tune != ""]
    
  }
  
  cnst.tune <- unlist(cnst.tune)
  
  values[["cnst.tune"]] <- cnst.tune
  updateTextInput(session, "cnst.tune", value = paste(cnst.tune, collapse = ", "))
  
})

# data returns data
wl.tune.data <- reactive({
  
  if (!is.null(input$wl.tune)) {
    
    wl.tune <- input$wl.tune
    
    if (ab.sep() == ";")
      wl.tune <- str_split(wl.tune, "\\; *") %>% unlist
    
    if (length(wl.tune) == 1 && ab.sep() == ",")
      wl.tune <- str_split(wl.tune, "\\, *") %>% unlist
    
    if (length(wl.tune) == 1 && wl.tune %like% "\\, ")
      wl.tune <- str_split(wl.tune, "\\, ") %>% unlist
    
    if (length(wl.tune) == 1 && wl.tune %like% "\\.")
      wl.tune <- str_split(wl.tune, "\\, *") %>% unlist
    
  } else {
    
    if (is.null(values[["wl.tune"]])) {
      
      wl.tune <- "110, 120, 130"
      
    } else {
      
      wl.tune <- values[["wl.tune"]]
      
    }
    
  }
  
  values[["wl.tune"]] <- wl.tune
  
  wl.tune
  
})

# load only updates textinput
wl.tune.load <- reactive({
  
  in.file.bulk <- input$file.ab.bulk.input
  in.file.xlsx <- NULL
  in.file <- NULL
  
  # bulk input
  
  if (nrow(as.data.table(input$file.ab.bulk.input)[name %like% "^(constants*_names*|targets*)(\\.csv|\\.txt)*"]) > 0){
    
    in.file <- as.data.table(input$file.ab.bulk.input)[name %like% "^(constants*_names*|targets*)(\\.csv|\\.txt)*"][1]
    in.file <- as.data.frame(in.file)
    
  }
  
  in.file.xlsx <- as.data.table(input$file.ab.bulk.input)[name %like% "\\.xlsx$"]
  
  if (nrow(in.file.xlsx) > 0) {
    
    in.file.xlsx <- as.data.frame(in.file.xlsx[1])
    
  } else {
    
    in.file.xlsx <- NULL
    
  }
  
  if (!is.null(in.file.xlsx))
    in.file <- NULL
  
  if (!is.null(in.file)) {
    
    if (ab.sep() == ";") {
      wl.tune <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", header = FALSE), silent = TRUE)
    } else if (ab.sep() == ",") {
      wl.tune <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", header = FALSE), silent = TRUE)
    } else if (ab.sep() == "tab") {
      wl.tune <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", header = FALSE), silent = TRUE)
    }
    
    setDT(wl.tune)
    wl.tune[1, V1 := str_replace(V1, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0x2e), as.raw(0xbf)))), "")]
    setnames(wl.tune, "V1", "X1")
    
    
  } else if (!is.null(in.file.xlsx)) {
    
    sht <- getSheetNames(in.file.xlsx$datapath[1])
    sht <- sht[sht %like% "^(constants*_names*|targets*)"]
    
    wl.tune <- try(read.xlsx(in.file.xlsx$datapath, sheet = sht, colNames = FALSE), silent = TRUE)
    
    
  } else {
    
    wl.tune <- values[["wl.tune"]]
    
  }
  
  # new format
  
  setDT(wl.tune)
  
  if (nrow(wl.tune[X1 == "wavelength"]) > 0) {
    
    wl.tune <- wl.tune[X1 == "wavelength"][, !"X1", with = FALSE]
    wl.tune <- unlist(wl.tune)
    wl.tune <- wl.tune[!is.na(wl.tune) & wl.tune != ""]
    
    wl.tune <- unlist(wl.tune)
    
    values[["wl.tune"]] <- wl.tune
    updateTextInput(session, "wl.tune", value = paste(wl.tune, collapse = ", "))
    
  } else {
    
    wl.tune <- head(dt.mol.data()[, wavelength], 10)
    
    values[["wl.tune"]] <- wl.tune
    updateTextInput(session, "wl.tune", value = paste(wl.tune, collapse = ", "))
    
  }
  
})

# to save results
target.data <- reactive({
  
  target <- list(constant = cnst.tune.data(), wavelength = as.character(wl.tune.data()))
  target <- setDT(lapply(target, "length<-", max(lengths(target))))[]
  
  target[is.na(constant), constant := ""]
  target[is.na(wavelength), wavelength := ""]
  
  target <- as.data.table(t(target), keep.rownames = TRUE)
  
  cln <- target[rn == "constant"] %>% unlist
  target <- target[2:nrow(target)]
  
  setnames(target, cln)
  
  target
  
  
})


# execute

ab.eval.data <- reactive({
  
  withProgress(message = "Computation... It may take some time", value = 0, {
    
    incProgress(.1)
    
    # validity tests
    
    particles <- c(colnames(ab.dt.coef.data()), ab.dt.coef.data()[, name])
    
    validate(
      
      need(length(particles %in% cnst.tune.data()) > 0, "Input correct component names for constants evaluation")
      
    )
    
    dt.ab <- dt.ab.data()
    
    validate(
      
      need(identical(as.data.table(dt.ab)[data %like% "observ", wavelength] %>% sort, as.data.table(dt.ab)[data %like% "deviat", wavelength] %>% sort)
           , "Wavelengths in Observation part are inconsistent with ones in Deviation part")
      
    )
    
    # check if no molar extinction coefficients are known
    
    dt.mol <- dt.mol.data()
    
    if (ncol(dt.mol) <= 1)
      dt.mol <- NA #"no.data"
    
    incProgress(.3)
    
    # run
    
    res <- ab.evaluation.runner(mode = "app"
                                , sep = ab.sep()
                                , eq.thr.type = "rel"
                                , eq.threshold = 1e-08
                                , cnst.tune = cnst.tune.data()
                                , algorithm = "direct search"
                                , ab.mode = "base"
                                , method = "basic wls"
                                , search.density = as.numeric(input$search.density)
                                , lrate.init = .5
                                , ab.threshold = as.numeric(input$ab.threshold)
                                , wl.tune = head(wl.tune.data(), 10)
                                , dt.list = list(dt.coef = ab.dt.coef.data()
                                                 , cnst = ab.cnst.data()
                                                 , dt.conc = ab.dt.conc.data()
                                                 , part.eq = ab.part.eq.data()
                                                 , dt.ab = dt.ab
                                                 , dt.mol = dt.mol)
                                , save.res = FALSE)
    
    incProgress(.6)
    
  })
  
  res
  
})


# output data

ab.dt.res.data <- eventReactive(input$ab.conc.exec.btn, {
  
  ab.eval.data()$dt.eq.conc
  
})

dt.ab.abs.data <- eventReactive(input$ab.conc.exec.btn, {
  
  dt <- ab.eval.data()$dt.ab.calc
  dt.err <- ab.eval.data()$ab.res.abs
  
  dt.comb <- data.table(data = c(rep("observation", nrow(dt)), rep("error", nrow(dt.err)))
                        , rbind(dt, dt.err, use.names = TRUE))
  
  dt.comb
  
})

dt.ab.rel.data <- eventReactive(input$ab.conc.exec.btn, {
  
  dt <- ab.eval.data()$dt.ab.calc
  dt.err <- ab.eval.data()$ab.res.rel
  
  dt.comb <- data.table(data = c(rep("observation", nrow(dt)), rep("error", nrow(dt.err)))
                        , rbind(dt, dt.err, use.names = TRUE))
  
  dt.comb
  
})

plot.dt.ab.data <- eventReactive(input$ab.conc.exec.btn, {
  
  # get data
  
  dt.calc <- copy(ab.eval.data()$dt.ab.calc)
  
  dt.obs <- dt.ab.data()[data %like% "^observ"]
  dt.obs[, data := "Observed"]
  
  # unify column names
  
  cln <- colnames(dt.calc)
  cln <- cln[!(cln %in% c("wavelength", "data"))]
  
  setnames(dt.obs, c("data", "wavelength", cln))
  
  dt.calc[, data := "Calculated"]
  
  # melt
  
  dt.calc <- melt(dt.calc, id.vars = c("wavelength", "data"), variable.name = "solution", value.name = "absorbance")
  dt.obs <- melt(dt.obs, id.vars = c("wavelength", "data"), variable.name = "solution", value.name = "absorbance")
  
  # convert observed absorbance to numerics if not
  
  dt.obs[, absorbance := as.character(absorbance)]
  dt.obs[, absorbance := str_replace_all(absorbance, " ", "")]
  dt.obs[, absorbance := str_replace_all(absorbance, "\\,", "\\.")]
  dt.obs[, absorbance := as.numeric(absorbance)]
  
  # bind
  
  intr <- intersect(dt.obs[, wavelength], dt.calc[, wavelength])
  
  dt <- rbind(dt.obs[wavelength %in% intr], dt.calc[wavelength %in% intr], use.names = TRUE, fill = TRUE)
  
  # convert wavelength to numeric if not
  
  dt[, wavelength := as.character(wavelength)]
  dt[, wavelength := str_replace_all(wavelength, " ", "")]
  dt[, wavelength := str_replace_all(wavelength, "\\,", "\\.")]
  dt[, wavelength := as.numeric(wavelength)]
  
  # select wavelengths used in calculation
  dt.cut <- dt[wavelength %in% as.numeric(wl.tune.data())]
  
  # return
  
  list(dt.full = dt, dt.cut = dt.cut)
  
})

ab.cnst.dev.data <- eventReactive(input$ab.conc.exec.btn, {
  
  cnst.dev <- ab.eval.data()$cnst.dev
  cnst.dev <- as.data.table(cnst.dev)
  
  setnames(cnst.dev, c("Component", "Constant", "St.Deviation", "Validity"))
  
})

ab.cor.m.data <- eventReactive(input$ab.conc.exec.btn, {
  
  cor.m <- ab.eval.data()$cor.m
  
  cor.m
  
})

mol.coef.data <- eventReactive(input$ab.conc.exec.btn, {
  
  dt <- ab.eval.data()$mol.coef
  dt.err <- ab.eval.data()$mol.coef.dev
  
  dt.comb <- data.table(data = c(rep("observation", nrow(dt)), rep("error", nrow(dt.err)))
                        , rbind(dt, dt.err, use.names = TRUE))
  
  dt.comb
  
})

ab.adj.r.squared.data <- eventReactive(input$ab.conc.exec.btn, {
  
  ab.adj.r.squared <- ab.eval.data()$adj.r.squared
  ab.adj.r.squared <- data.table(`Adj. R^2` = ab.adj.r.squared)
  
  ab.adj.r.squared
  
})


# rendering ---------------- #

output$ab.dt.coef <- server_render_dt.coef("ab")

output$ab.dt.conc <- server_render_dt.conc("ab")

output$ab.part.eq <- server_render_part.eq("ab")

output$ab.cnst <- server_render_cnst("ab")

output$dt.ab <- renderRHandsontable({
  
  in.file <- input$file.dt.ab
  in.file.bulk <- input$file.ab.bulk.input
  in.file.xlsx <- NULL
  
  # bulk input
  
  if (input.source$dt.ab.bulk) {
    
    in.file <- as.data.table(input$file.ab.bulk.input)[name %like% "^(input\\_)*absorbance(\\.csv|\\.txt)*$"][1]
    in.file <- as.data.frame(in.file)
    
    in.file.xlsx <- as.data.table(input$file.ab.bulk.input)[name %like% "\\.xlsx$"]
    
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
    
    if (ab.sep() == ";") {
      dt.ab <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
    } else if (ab.sep() == ",") {
      dt.ab <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
    } else if (ab.sep() == "tab") {
      dt.ab <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
    }
    
    setDT(dt.ab)
    
    cln <- colnames(dt.ab)
    setnames(dt.ab, cln, str_replace(cln, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0x2e), as.raw(0xbf)))), ""))
    
    validate(
      
      need(is.data.frame(dt.ab), "Your file doesn't look like an absorbance file")
      
    )
    
    
  } else if (!is.null(in.file.xlsx)) {
    
    shts <- getSheetNames(in.file.xlsx$datapath)
    
    shts <- shts[shts %like% "(input_|output_)*absorbance"]
    shts <- sort(shts, decreasing = TRUE)
    
    dt.ab <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1]), silent = TRUE)
    
    validate(
      
      need(is.data.frame(dt.ab), "Your file doesn't look like an absorbance file")
      
    )
    
  } else {
    
    dt.ab <- dt.ab.data()
    
  }
  
  if (!is.null(dt.ab)) {
    
    if (nrow(dt.ab) > 25) {
      
      rhandsontable(dt.ab, stretchH = "all", useTypes = FALSE, height = 600) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
      
    } else {
      
      rhandsontable(dt.ab, stretchH = "all", useTypes = FALSE, height = NULL) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
      
    }
    
  }
  
  
  
})

output$dt.mol <- renderRHandsontable({
  
  in.file <- input$file.dt.mol
  in.file.bulk <- input$file.ab.bulk.input
  in.file.xlsx <- NULL
  
  # bulk input
  
  if (input.source$dt.mol.bulk) {
    
    in.file <- as.data.table(input$file.ab.bulk.input)[name %like% "^(input\\_)*mol(ar)*\\_ext(inction)*\\_coefficients(\\.csv|\\.txt)*$"][1]
    in.file <- as.data.frame(in.file)
    
    in.file.xlsx <- as.data.table(input$file.ab.bulk.input)[name %like% "\\.xlsx$"]
    
    if (nrow(in.file.xlsx) > 0) {
      
      in.file.xlsx <- as.data.frame(in.file.xlsx[1])
      
    } else {
      
      in.file.xlsx <- NULL
      
    }
    
    if (!is.null(in.file.xlsx))
      in.file <- NULL
    
  }
  
  # choose source
  
  if (!is.null(in.file) & !input.source$dt.mol.memory) {
    
    if (ab.sep() == ";") {
      dt.mol <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
    } else if (ab.sep() == ",") {
      dt.mol <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
    } else if (ab.sep() == "tab") {
      dt.mol <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
    }
    
    if (is.data.frame(dt.mol)) {
      
      setDT(dt.mol)
      
      cln <- colnames(dt.mol)
      setnames(dt.mol, cln, str_replace(cln, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0x2e), as.raw(0xbf)))), ""))
      
    }
    
  } else if (!is.null(in.file.xlsx) & !input.source$dt.mol.memory) {
    
    shts <- getSheetNames(in.file.xlsx$datapath)
    
    shts <- shts[shts %like% "^(input_|output_)*mol(ar)*_ext(inction)*_coefficients"]
    shts <- sort(shts)
    
    dt.mol <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1]), silent = TRUE)
    
  } else if (input.source$dt.mol.memory) {
    
    dt.mol <- sp.dt.mol.full.data()
    
    validate(
      
      need(is.data.frame(dt.mol), "Extinction coefficients are not yet calculated (check Extinction Coefficients tab)")
      
    )
    
  } else {
    
    dt.mol <- dt.mol.data()
    
  }
  
  if (!is.data.frame(dt.mol))
    dt.mol <- data.frame(no.data = "no.data")
  
  if (!is.null(dt.mol)) {
    
    if (nrow(dt.mol) > 25) {
      
      rhandsontable(dt.mol, stretchH = "all", useTypes = FALSE, height = 500) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
      
    } else {
      
      rhandsontable(dt.mol, stretchH = "all", useTypes = FALSE, height = NULL) %>%
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
                                                              Shiny.onInputChange('dt.mol.colnames', headers);
                                                              //this.view.wt.wtOverlays.adjustElementsSize(true);
                                                          }")
                                               )))
      
    }
    
  }
  
})

output$ab.dt.res <- server_render_dt.res("ab")

output$dt.ab.abs <- renderRHandsontable({
  
  dt.ab.abs <- dt.ab.abs.data()
  
  if (!is.null(dt.ab.abs)) {
    
    row_highlight <- dt.ab.abs[data == "observation", which = TRUE] - 1
    
    renderer <- "
      function (instance, td, row, col, prop, value, cellProperties) {
      
      Handsontable.renderers.TextRenderer.apply(this, arguments);
      
      if (instance.params) {
      hrows = instance.params.row_highlight
      hrows = hrows instanceof Array ? hrows : [hrows]
      }
      
      if (instance.params && hrows.includes(row) && value < 0) {
      td.style.background = 'pink';
      }
      
      }" 
    
    if (nrow(dt.ab.abs) > 25) {
      
      rhandsontable(dt.ab.abs, stretchH = "all", row_highlight = row_highlight, height = 550) %>%
        hot_cols(renderer = renderer)
      
    } else {
      
      rhandsontable(dt.ab.abs, stretchH = "all", row_highlight = row_highlight, height = NULL) %>%
        hot_cols(renderer = renderer)
      
    }
    
  }
  
})

output$dt.ab.rel <- renderRHandsontable({
  
  dt.ab.rel <- dt.ab.rel.data()
  
  if (!is.null(dt.ab.rel)) {
    
    row_highlight <- dt.ab.rel[data == "observation", which = TRUE] - 1
    
    renderer <- "
      function (instance, td, row, col, prop, value, cellProperties) {
      
        Handsontable.renderers.TextRenderer.apply(this, arguments);
        
        if (instance.params) {
          hrows = instance.params.row_highlight
          hrows = hrows instanceof Array ? hrows : [hrows]
        }
          
        if (instance.params && hrows.includes(row) && value < 0) {
          td.style.background = 'pink';
        }
      
      }" 
    
    if (nrow(dt.ab.rel) > 25) {
      
      rhandsontable(dt.ab.rel, stretchH = "all", row_highlight = row_highlight, height = 550) %>%
        hot_cols(renderer = renderer)
      
    } else {
      
      rhandsontable(dt.ab.rel, stretchH = "all", row_highlight = row_highlight, height = NULL) %>%
        hot_cols(renderer = renderer)
      
    }
    
  }
  
})

output$ab.cnst.dev <- renderRHandsontable({
  
  cnst.dev <- ab.cnst.dev.data()
  
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

output$ab.cor.m <- renderRHandsontable({
  
  cor.m <- ab.cor.m.data()
  
  if (!is.null(cor.m))
    
    rhandsontable(cor.m, stretchH = FALSE, useTypes = FALSE) %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  
})

output$mol.coef <- renderRHandsontable({
  
  mol.coef <- mol.coef.data()
  
  if (!is.null(mol.coef)) {
    
    row_highlight <- mol.coef[data == "observation", which = TRUE] - 1
    
    renderer <- "
      function (instance, td, row, col, prop, value, cellProperties) {
      
        Handsontable.renderers.TextRenderer.apply(this, arguments);
        
        if (instance.params) {
          hrows = instance.params.row_highlight
          hrows = hrows instanceof Array ? hrows : [hrows]
        }
        
        if (instance.params && hrows.includes(row) && value < 0) {
          td.style.background = 'pink';
        }
      
      }" 
    
    if (nrow(mol.coef) > 25) {
      
      rhandsontable(mol.coef, stretchH = "all", row_highlight = row_highlight, height = 550) %>%
        hot_cols(renderer = renderer)
      
    } else {
      
      rhandsontable(mol.coef, stretchH = "all", row_highlight = row_highlight, height = NULL) %>%
        hot_cols(renderer = renderer)
      
    }
    
  }
  
})

output$ab.adj.r.squared <- renderRHandsontable({
  
  ab.adj.r.squared <- ab.adj.r.squared.data()
  
  if (!is.null(ab.adj.r.squared))
    
    rhandsontable(ab.adj.r.squared, stretchH = FALSE, useTypes = FALSE) %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  
})

output$plot.dt.ab <- renderPlotly({
  
  dt <- plot.dt.ab.data()$dt.full
  
  lbl <- sort(unique(dt[, wavelength]))
  
  g <- ggplot(data = dt) +
    geom_point(aes(x = wavelength, y = absorbance, group = data, color = data), size = .5) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_grid(. ~ solution) +
    labs(x = "Wavelength, nm", y = "Absorbance")
  
  g <- ggplotly(g)
  g[["x"]][["layout"]][["annotations"]][[1]][["y"]] <- -0.15
  g <- g %>% plotly::layout(margin = list(b = 100, t = 50))
  
  # g$x$data[[1]]$hoverinfo <- "none"
  
  g
  
})

output$plot.dt.ab.cut <- renderPlotly({
  
  dt <- plot.dt.ab.data()$dt.cut
  
  lbl <- sort(unique(dt[, wavelength]))
  
  g <- ggplot(data = dt) +
    geom_point(aes(x = wavelength, y = absorbance, group = data, color = data), size = .5) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_grid(. ~ solution) +
    labs(x = "Wavelength, nm", y = "Absorbance")
  
  g <- ggplotly(g)
  g[["x"]][["layout"]][["annotations"]][[1]][["y"]] <- -0.15
  g <- g %>% plotly::layout(margin = list(b = 100, t = 50))
  
  # g$x$data[[1]]$hoverinfo <- "none"
  
  g
  
})



