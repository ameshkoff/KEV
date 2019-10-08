# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #



# equilibrium concentrations -------------------------

# technical

eq.sep <- reactive({
  
  switch(input$eq.sep,
         comma = ",",
         semicolon = ";",
         tab = "tab")
  
})

observeEvent(input$file.eq.bulk.input, {
  
  # stoichiometric coefficients
  
  if (nrow(as.data.table(input$file.eq.bulk.input)[name %like% "^(input\\_)*stoich(iometric)*\\_coefficients(\\.csv|\\.txt)*"]) > 0){
    input.source$eq.dt.coef.bulk <- TRUE
  }
  
  if (nrow(as.data.table(input$file.eq.bulk.input)[name %like% "\\.xlsx$"]) > 0){
    
    shts <- getSheetNames(input$file.eq.bulk.input$datapath)
    
    if (length(shts[shts %like% "stoich_coefficients"]))
      input.source$eq.dt.coef.bulk <- TRUE
    
  }
  
  # concentrations
  
  if (nrow(as.data.table(input$file.eq.bulk.input)[name %like% "^(input\\_)*concentrations(\\.csv|\\.txt)*"]) > 0){
    
    input.source$eq.dt.conc.bulk <- TRUE
    input.source$eq.dt.conc.pc.fl <- FALSE
    
  }
  
  if (nrow(as.data.table(input$file.eq.bulk.input)[name %like% "\\.xlsx$"]) > 0){
    
    shts <- getSheetNames(input$file.eq.bulk.input$datapath)
    
    if (length(shts[shts %like% "concentrations"])) {
      
      input.source$eq.dt.conc.bulk <- TRUE
      input.source$eq.dt.conc.pc.fl <- FALSE
      
    }
    
  }
  
  # constants
  
  if (nrow(as.data.table(input$file.eq.bulk.input)[name %like% "^(input\\_)*k\\_constants\\_log10(\\.csv|\\.txt)*"]) > 0){
    input.source$eq.cnst.bulk <- TRUE
  }
  
  if (nrow(as.data.table(input$file.eq.bulk.input)[name %like% "\\.xlsx$"]) > 0){
    
    shts <- getSheetNames(input$file.eq.bulk.input$datapath)
    
    if (length(shts[shts %like% "k_constants_log10"]))
      input.source$eq.cnst.bulk <- TRUE
    
  }
  
}, priority = 1000)

observeEvent(input$file.eq.dt.coef, {
  
  input.source$eq.dt.coef.bulk <- FALSE
  
}, priority = 1000)

observeEvent(input$file.eq.dt.conc, {
  
  input.source$eq.dt.conc.bulk <- FALSE
  input.source$eq.dt.conc.pc.fl <- FALSE
  
}, priority = 1000)

observeEvent(input$file.eq.cnst, {
  
  input.source$eq.cnst.bulk <- FALSE
  
}, priority = 1000)

observeEvent(input$eq.pc.update.btn, {
  
  input.source$eq.dt.conc.pc.fl <- TRUE
  updateTabsetPanel(session, "eq.conc.tab", selected = "input")
  
}, priority = 1000)


# data --------------------- #

# input data

eq.part.names.data <- server_part.names.data("eq")

eq.dt.coef.data <- server_dt.coef.data("eq")

eq.dt.conc.data <- server_dt.conc.data("eq")

eq.part.eq.data <- server_part.eq.data("eq")

eq.dt.conc.pc.data <- reactive({
  
  if (!is.null(input$eq.dt.conc.pc)) {
    
    eq.dt.conc.pc <- hot_to_r(input$eq.dt.conc.pc)
    
  } else {
    
    if (is.null(values[["eq.dt.conc.pc"]])) {
      
      eq.dt.conc.pc <- as.data.table(matrix(rep(1e-03, 3), 1))
      setnames(eq.dt.conc.pc, paste0("molecule", 2:4))
      
    } else {
      
      eq.dt.conc.pc <- values[["eq.dt.conc.pc"]]
      
    }
    
  }
  
  eq.dt.conc.pc <- as.data.table(eq.dt.conc.pc)
  
  cln <- eq.part.names.data()[1:(ncol(eq.dt.conc.pc) + 1)]
  cln <- cln[!(cln %in% eq.pc.name.data())]
  
  validate(
    
    need(ncol(eq.dt.conc.pc) == length(cln), "Check if the variable component name is consistent with component names")
    
  )
  
  setnames(eq.dt.conc.pc, cln)
  
  values[["eq.dt.conc.pc"]] <- eq.dt.conc.pc
  
  eq.dt.conc.pc
  
})

eq.cnst.data <- server_cnst.data("eq")

bs.name.data <- reactive({
  
  if (!is.null(input$bs.name)) {
    
    bs.name <- input$bs.name
    bs.name <- str_split(bs.name, "\\, *")
    bs.name <- unlist(bs.name)
    
  } else {
    
    if (is.null(values[["bs.name"]])) {
      
      bs.name <- "molecule1"
      
    } else {
      
      bs.name <- values[["bs.name"]]
      
    }
    
  }
  
  values[["bs.name"]] <- bs.name
  
  bs.name
  
})

bs.name.load <- reactive({
  
  in.file.bulk <- input$file.eq.bulk.input
  in.file.xlsx <- NULL
  in.file <- NULL
  
  # bulk input
  
  if (nrow(as.data.table(input$file.eq.bulk.input)[name %like% "^part(icle)*_names(\\.csv|\\.txt)*"]) > 0){
    
    in.file <- as.data.table(input$file.eq.bulk.input)[name %like% "^part(icle)*_names(\\.csv|\\.txt)*"][1]
    in.file <- as.data.frame(in.file)
    
  }
  
  in.file.xlsx <- as.data.table(input$file.eq.bulk.input)[name %like% "\\.xlsx$"]
  
  if (nrow(in.file.xlsx) > 0) {
    
    in.file.xlsx <- as.data.frame(in.file.xlsx[1])
    
  } else {
    
    in.file.xlsx <- NULL
    
  }
  
  if (!is.null(in.file.xlsx))
    in.file <- NULL
  
  if (!is.null(in.file)) {
    
    if (eq.sep() == ";") {
      bs.name <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", header = FALSE), silent = TRUE)
    } else if (eq.sep() == ",") {
      bs.name <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", header = FALSE), silent = TRUE)
    } else if (eq.sep() == "tab") {
      bs.name <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", header = FALSE), silent = TRUE)
    }
    
  } else if (!is.null(in.file.xlsx)) {
    
    shts <- getSheetNames(in.file.xlsx$datapath)
    
    shts <- shts[shts %like% "^(particle|component)_name"]
    shts <- sort(shts)
    
    bs.name <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1], colNames = FALSE), silent = TRUE)
    
  } else {
    
    bs.name <- values[["bs.name"]]
    
  }
  
  bs.name <- unlist(bs.name)
  
  values[["bs.name"]] <- bs.name
  updateTextInput(session, "bs.name", value = paste(bs.name, collapse = ", "))
  
})

eq.pc.name.data <- reactive({
  
  if (!is.null(input$eq.pc.name)) {
    
    eq.pc.name <- input$eq.pc.name
    eq.pc.name <- str_trim(eq.pc.name)
    
  } else {
    
    if (is.null(values[["eq.pc.name"]])) {
      
      eq.pc.name <- "molecule1"
      
    } else {
      
      eq.pc.name <- values[["eq.pc.name"]]
      
    }
    
  }
  
  values[["eq.pc.name"]] <- eq.pc.name
  
  eq.pc.name
  
})


# execute

eq.pc.update <- eventReactive(input$eq.pc.update.btn, {
  
  # get data
  
  eq.dt.conc.pc <- eq.dt.conc.pc.data()
  pc.name <- eq.pc.name.data()
  pc.range <- input$eq.pc.range
  
  # pc.range
  
  pc.range <- seq(pc.range[1], pc.range[2], (pc.range[2] - pc.range[1]) / 100)
  pc.range <- 10 ^ -pc.range
  
  pc.range <- data.table(pc.range)
  setnames(pc.range, pc.name)
  
  # concentrations data table
  
  eq.dt.conc <- data.table(eq.dt.conc.pc, pc.range)
  
  # type of concentration
  
  eq.part.eq <- data.table(t(c(rep("tot", ncol(eq.dt.conc.pc)), "eq")))
  
  cln <- c(colnames(eq.dt.conc.pc), pc.name)
  setnames(eq.part.eq, cln)
  
  # restore column order
  
  cln <- colnames(eq.dt.coef.data())
  
  setcolorder(eq.dt.conc, cln)
  setcolorder(eq.part.eq, cln)
  
  # update values
  
  values[["eq.dt.conc"]] <- eq.dt.conc
  values[["eq.part.eq"]] <- eq.part.eq
  
  # return
  
  list(eq.dt.conc = eq.dt.conc, eq.part.eq = eq.part.eq)
  
})

eval.data <- reactive({
  
  withProgress(message = "Computation... It may take some time", value = 0, {
    
    incProgress(.1)
    
    validate(
      
      need(length(colnames(eq.dt.coef.data())[colnames(eq.dt.coef.data()) == bs.name.data()]) > 0, "Input correct component name to get fractions of")
      
    )
    
    incProgress(.3)
    
    pc.name <- bs.name.data()
    
    if (input.source$eq.dt.conc.pc.fl)
      pc.name <- eq.pc.name.data()
    
    res <- eq.evaluation.runner(mode = "app"
                                , sep = eq.sep()
                                , bs.name = bs.name.data()
                                , thr.type = c("rel")
                                , threshold = 1e-08
                                , dt.list = list(dt.coef = eq.dt.coef.data()
                                                 , cnst = eq.cnst.data()
                                                 , dt.conc = eq.dt.conc.data()
                                                 , part.eq = eq.part.eq.data())
                                , save.res = FALSE
                                , pc.name = pc.name)
    
    incProgress(.6)
    
  })
  
  res
  
})


# output data

eq.dt.res.data <- eventReactive(input$eq.conc.exec.btn, {
  
  eval.data()$dt.res
  
})

dt.frac.data <- eventReactive(input$eq.conc.exec.btn, {
  
  dt.frac <- eval.data()$dt.frac
  
  dt.frac <- as.data.table(t(dt.frac), keep.rownames = TRUE)
  setnames(dt.frac, unlist(dt.frac[1]))
  
  dt.frac <- dt.frac[!1]
  
  dt.frac
  
})

dt.err.data <- eventReactive(input$eq.conc.exec.btn, {
  
  eval.data()$dt.err
  
})

eq.dt.conc.tot.data <- eventReactive(input$eq.conc.exec.btn, {
  
  eval.data()$eq.dt.conc.tot
  
})

output$plot.eq.dt.frac <- renderPlotly({
  
  # get data
  
  dt <- dt.frac.data()
  
  # remove garbage
  
  dt[, rn := NULL]
  
  # convert to numerics
  
  cln <- colnames(dt)
  
  for (cl in cln)
    dt[, eval(cl) := as.numeric(eval(as.name(cl)))]
  
  # get pC component name and set names
  
  cln <- colnames(dt)
  cln <- cln[cln %like% "^p\\(.*\\)$"][1]
  
  setnames(dt, cln, "pC")
  
  # melt
  
  dt <- melt(dt, id.vars = "pC", variable.name = "Component")
  
  # plot
  
  g <- ggplot(data = dt) +
    geom_point(aes(x = pC, y = value, group = Component, color = Component), size = .5) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = str_replace_all(cln, "[\\(\\)]", ""), y = "%")
  
  g <- ggplotly(g)
  g[["x"]][["layout"]][["annotations"]][[1]][["y"]] <- -0.15
  g <- g %>% plotly::layout(margin = list(b = 100, t = 50))
  
  g
  
})



# text --------------------- #

output$txt.frac <- renderText({
  paste("Fractions per ", bs.name.data())
})



# rendering ---------------- #

output$eq.dt.coef <- server_render_dt.coef("eq")

output$eq.dt.conc <- server_render_dt.conc("eq")

output$eq.part.eq <- server_render_part.eq("eq")

output$eq.dt.conc.pc <- renderRHandsontable({
  
  eq.dt.conc.pc <- eq.dt.conc.pc.data()
  
  if (!is.null(eq.dt.conc.pc))
    rhandsontable(eq.dt.conc.pc, stretchH = "all", useTypes = FALSE) %>%
    hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
  
})

output$eq.cnst <- server_render_cnst("eq")

output$eq.dt.res <- server_render_dt.res("eq")

output$dt.frac <- renderRHandsontable({
  
  dt.frac <- dt.frac.data()
  
  if (!is.null(dt.frac)) {
    
    if (!is.null(dt.frac)) {
      
      if (nrow(dt.frac) > 15) {
        
        rhandsontable(dt.frac, stretchH = FALSE, useTypes = FALSE, height = 300) %>%
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
        
      } else {
        
        rhandsontable(dt.frac, stretchH = FALSE, useTypes = FALSE, height = NULL) %>%
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
        
      }
      
    }
    
  }
  
})

output$dt.err <- renderRHandsontable({
  
  dt.err <- dt.err.data()
  
  if (!is.null(dt.err)) {
    
    if (nrow(dt.err) > 15) {
      
      rhandsontable(dt.err, stretchH = FALSE, useTypes = FALSE, height = 300) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      
    } else {
      
      rhandsontable(dt.err, stretchH = FALSE, useTypes = FALSE, height = NULL) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      
    }
    
  }
  
})

output$eq.dt.conc.tot <- renderRHandsontable({
  
  eq.dt.conc.tot <- eq.dt.conc.tot.data()
  
  if (!is.null(eq.dt.conc.tot))
    
    rhandsontable(eq.dt.conc.tot, stretchH = FALSE, useTypes = FALSE) %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  
})

