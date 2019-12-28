# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #



# curve fitting -------------------------------------


# reactive values ---------- #

cur.sep <- reactive({
  
  switch(input$cur.sep,
         comma = ",",
         semicolon = ";",
         tab = "tab")
  
})

cur.curves.iterator <- reactiveVal(1L)

cur.update.status <- reactiveVal(1L)

cur.curve.rows <- reactiveValues(values = data.table(cur.id = character(), plot.id = integer()))

cur.formula.observers <- reactiveValues(values = list())

cur.formula.values <- reactiveValues()


# controls ----------------- #

# cur task

observeEvent(input$cur.task, {
  
  if (!is.null(input$cur.task) && values$cur.status@cur.task != input$cur.task)
    values$cur.status@cur.task <- input$cur.task
  
})

# curve inner

cur.curve.input <- function(input.id, label, value, size = 2, type = "numeric") {
  
  if (type == "numeric") {
    
    column(size
           , numericInput(inputId = input.id
                          , label = label
                          , value = value)
           , class = "kev-densed-input-row")
    
  } else if (type == "character") {
    
    column(size
           , textInput(inputId = input.id
                       , label = label
                       , value = value)
           , class = "kev-densed-input-row")
    
  }
  
}

# curve specific

cur.curve.gaussian <- function(fn.type.id, btn.id, cur.id, fn.id, cur.params = NULL) {
  
  if (is.null(cur.params))
    cur.params <- list(amplitude = cur.dt.init.data()[, max(value)]
                       , expvalue = cur.dt.init.data()[, mean(label)]
                       , hwhm = (cur.dt.init.data()[, max(label) - min(label)]) / 4)
  
  cur.params <- data.table(param = names(cur.params), value = unlist(cur.params))
  cur.params <- cur.params[order(param)]
  cur.params[, label := c("Amplitude", "Exp.value", "HWHM")]
  
  # ui
  
  cur.curve.ui <- fluidRow(column(2
                                  , h4("Gaussian")
                                  , class = "kev-densed-input-row")
                           
                           , cur.curve.input(paste0(fn.type.id, fn.id, "_name"), "Name", paste("Curve", fn.id), 2, type = "character")
                           
                           , mapply(function(p, l, v) {
                             
                             cur.curve.input(paste0(fn.type.id, fn.id, "_", p), l, v, 2)
                             
                           }, cur.params[, param], cur.params[, label], cur.params[, value], SIMPLIFY = FALSE)
                           
                           , column(2
                                    , actionButton(btn.id
                                                   , ""
                                                   , icon = icon("trash")
                                                   , style = "margin-top: 25px;"))
                           , id = cur.id)
  
  # event observer
  
  cur.curve.observer(cur.params[, paste0(fn.type.id, fn.id, "_", param)])
  
  # update cur.dt.par :  here - to insert the whole curve at once
  #   and avoid event issues if only some parameters of the curve are already defined
  
  dt.par <- copy(values$cur.dt.par)
  
  for (i in 1:nrow(cur.params)) {
    
    dt.ins <- data.table(name = as.character(fn.id)
                         , design = "gaussian"
                         , param = cur.params[i, param]
                         , value = cur.params[i, value])
    
    if (nrow(merge(dt.par, dt.ins, by = c("name", "design", "param"))) == 0)
      dt.par <- rbind(dt.par, dt.ins, use.names = TRUE, fill = TRUE)
  }
  
  if (nrow(values$cur.dt.par) != nrow(dt.par))
    values$cur.dt.par <- cur.dt.par.sanitize(dt.par)
  
  # return
  
  list(ui = cur.curve.ui)
  
}

cur.curve.lorentzian <- function(fn.type.id, btn.id, cur.id, fn.id, cur.params = NULL) {
  
  if (is.null(cur.params))
    cur.params <- list(amplitude = cur.dt.init.data()[, max(value)]
                       , expvalue = cur.dt.init.data()[, mean(label)]
                       , hwhm = cur.dt.init.data()[, max(label) - min(label)] / 4)
  
  cur.params <- data.table(param = names(cur.params), value = unlist(cur.params))
  cur.params <- cur.params[order(param)]
  cur.params[, label := c("Amplitude", "Exp.value", "HWHM")]
  
  # ui
  
  cur.curve.ui <- fluidRow(column(2
                                  , h4("Lorentzian")
                                  , class = "kev-densed-input-row")
                           
                           , cur.curve.input(paste0(fn.type.id, fn.id, "_name"), "Name", paste("Curve", fn.id), 2, type = "character")
                           
                           , mapply(function(p, l, v) {
                             
                             cur.curve.input(paste0(fn.type.id, fn.id, "_", p), l, v, 2)
                             
                           }, cur.params[, param], cur.params[, label], cur.params[, value], SIMPLIFY = FALSE)
                           
                           , column(2
                                    , actionButton(btn.id
                                                   , ""
                                                   , icon = icon("trash")
                                                   , style = "margin-top: 25px;"))
                           , id = cur.id)
  
  # event observer
  
  cur.curve.observer(cur.params[, paste0(fn.type.id, fn.id, "_", param)])
  
  # update cur.dt.par :  here - to insert the whole curve at once
  #   and avoid event issues if only some parameters of the curve are already defined
  
  dt.par <- copy(values$cur.dt.par)
  
  for (i in 1:nrow(cur.params)) {
    
    dt.ins <- data.table(name = as.character(fn.id)
                         , design = "lorentzian"
                         , param = cur.params[i, param]
                         , value = cur.params[i, value])
    
    if (nrow(merge(dt.par, dt.ins, by = c("name", "design", "param"))) == 0)
      dt.par <- rbind(dt.par, dt.ins, use.names = TRUE, fill = TRUE)
  }
  
  if (nrow(values$cur.dt.par) != nrow(dt.par))
    values$cur.dt.par <- cur.dt.par.sanitize(dt.par)
  
  # return
  
  list(ui = cur.curve.ui)
  
}

# curve basic

cur.curve.insert <- function(fn.type.id, cur.params = NULL, fn.id = NULL) {
  
  cur.itr <- cur.curves.iterator()
  if (is.null(fn.id)) fn.id <- cur.itr
  
  cur.id <- paste0(fn.type.id, fn.id, "_curve.row")
  btn.id <- paste0(fn.type.id, fn.id, "_remove.btn")
  
  # get ui
  
  if (str_to_lower(fn.type.id) == "gaussian") {
    
    cur.curve.ui <- cur.curve.gaussian(str_to_lower(fn.type.id), btn.id, cur.id, fn.id, cur.params)$ui
    
  } else if (str_to_lower(fn.type.id) == "lorentzian") {
    
    cur.curve.ui <- cur.curve.lorentzian(str_to_lower(fn.type.id), btn.id, cur.id, fn.id, cur.params)$ui
    
  }
  
  cur.curves.iterator(cur.itr + as.integer(1))
  cur.curve.rows$values <- rbind(cur.curve.rows$values, data.table(cur.id = cur.id, plot.id = NA))
  
  insertUI(selector = "#cur_new_curves_place"
           , where = "beforeBegin"
           , ui = cur.curve.ui
  )
  
  # remove button event handler
  
  observeEvent(input[[btn.id]], {
    
    if (length(values$cur.dt.par[, unique(name)]) > 1) {
      
      removeUI(paste0("#", escapeRegex(str_replace(btn.id, "\\_remove\\.btn", "_curve.row"))))
      
      cur.id <- str_replace(btn.id, "^(gaussian|lorentzian)", "")
      cur.id <- str_extract(cur.id, ".*\\_")
      cur.id <- str_replace(cur.id, "\\_", "")
      
      dt.par <- values$cur.dt.par
      dt.par <- dt.par[!(name %in% cur.id)]
      
      if (nrow(dt.par) != nrow(values$cur.dt.par)) {
        
        values$cur.dt.par <- cur.dt.par.sanitize(dt.par)
        
      }
      
    }
    
  })
  
}

cur.curve.observer <- function(input.id) {
  
  mapply(function(input.id) {
    
    cur.formula.values[[input.id]] <- reactive(input[[input.id]]) %>% debounce(500)
    
    cur.formula.observers$values[[input.id]] <-
      observeEvent(cur.formula.values[[input.id]]()
                   , {
                     
                     if (cur.update.status() == 1L) {
                       
                       param.id <- str_extract(input.id, "\\_[a-z0-9\\.]+$")
                       param.id <- str_replace(param.id, "\\_", "")
                       
                       cur.id <- str_replace(input.id, "^(gaussian|lorentzian)", "")
                       cur.id <- str_extract(cur.id, ".*\\_")
                       cur.id <- str_replace(cur.id, "\\_", "")
                       
                       vl <- as.numeric(cur.formula.values[[input.id]]())
                       
                       if (!is.null(values$cur.dt.par)) {
                         
                         if (nrow(values$cur.dt.par[name == cur.id & param == param.id]) > 0 && !is.na(vl)) {
                           
                           if (round(values$cur.dt.par[name == cur.id & param == param.id, value], 5) !=
                               round(vl, 5)) {
                             
                             dt.par <- copy(values$cur.dt.par)
                             dt.par[name == cur.id & param == param.id, value := vl]
                             
                             values$cur.dt.par <- cur.dt.par.sanitize(dt.par)
                             # print(values$cur.dt.par[name == cur.id & param == param.id, value])
                             # print(vl)
                           }
                           
                         } 
                         
                       }
                       
                     }
                     
                   }, ignoreNULL = TRUE, priority = 10, suspended = FALSE)
    
  }, input.id, SIMPLIFY = FALSE)
  
}

# restore selectInput default value

observeEvent(input$cur.add.curve.select, {
  
  id <- input$cur.add.curve.select
  
  if (id %in% unlist(cur.curves.list) & id != "Add Curve") {
    
    cur.curve.insert(id)
    
    updateSelectInput(session, "cur.add.curve.select",
                      selected = "Add Curve"
    )
    
  }
  
}
, ignoreInit = TRUE)

# boundaries

observeEvent(input$cur.window.left, {
  
  if (!is.null(values$cur.status) && !input.source$cur.is.file.loaded) {
    
    values$cur.status@window.borders[1] <- input$cur.window.left
    
  } else if (input.source$cur.is.file.loaded) {
    
    input.source$cur.is.file.loaded <- FALSE
    
  }
  
})

observeEvent(input$cur.window.right, {
  
  if (!is.null(values$cur.status) && !input.source$cur.is.file.loaded) {
    
    values$cur.status@window.borders[2] <- input$cur.window.right
    
  } else if (input.source$cur.is.file.loaded) {
    
    input.source$cur.is.file.loaded <- FALSE
    
  }
  
})

# update curve from plot

observeEvent(event_data("plotly_relayout", source = "cur.plot.curves"), {
  
  ev <- event_data("plotly_relayout", source = "cur.plot.curves")
  
  if (length(names(ev)[names(ev) %like% "^shapes.*xanchor$"]) > 0) {
    # browser()
    
    p.id <- names(ev)[1] %>% str_extract("^shapes\\[[0-9]+\\]") %>% str_extract("[0-9]+")
    c.id <- cur.curve.rows$values[plot.id == p.id, cur.id]
    
    updateNumericInput(session, str_replace(c.id, "curve.row", "expvalue"), value = ev[names(ev) %like% "xanchor$"][1][[1]])
    updateNumericInput(session, str_replace(c.id, "curve.row", "amplitude"), value = ev[names(ev) %like% "yanchor$"][1][[1]])
    
  }
  
})


# data --------------------- #

# input data

cur.dt.init.data <- reactive({
  
  if (!is.null(input$cur.dt.init)) {
    
    values$cur.dt.init <- hot_to_r(input$cur.dt.init)
    
  } else if (is.null(values$cur.dt.init)){
    
    set.seed(1)
    
    values$cur.dt.init <- data.table(label = as.numeric(1:121), value = dnorm(seq(-3, 3, .05)) * (1 + rnorm(121, sd = 1e-2)))
    cur.dt.par.data()
    
  }
  
  values$cur.dt.init
  
})

cur.dt.par.data <- reactive({
  
  if (is.null(values$cur.dt.par))
    values$cur.dt.par <- data.table(name = "", design = "", param = "task", value = input$cur.task)
  
  values$cur.dt.par
  
})

cur.dt.par.sanitize <- function(dt.par) {
  
  cln <- colnames(dt.par)
  cln <- cln[cln %in% c("name", "design", "param")]
  
  validate(
    need(length(cln) >= 3
         , paste("Parameters data does not contain one or more of the mandatory columns:"
                 , "`name`, `design`, `param`"))
  )
  
  for (cl in cln)
    dt.par[, eval(cl) := str_replace_all(eval(as.name(cl)), "[^[:alnum:]|[ ,.\\-:]]", ".")]
  
  dt.par
  
}

# load data from file

observeEvent({
  input$file.cur.bulk.input 
  input$cur.sep
}, {
  
  if (is.null(input$file.cur.bulk.input)) return()
  
  # load data ------ #
  
  in.file.bulk <- input$file.cur.bulk.input
  in.file.xlsx <- NULL
  in.file <- NULL
  
  # bulk input
  
  if (!is.null(in.file.bulk)) {
    
    in.file <- as.data.table(in.file.bulk)[name %like% "^(input\\_)*data(\\.csv|\\.txt)*"][1]
    in.file <- as.data.frame(in.file)
    
    in.file.xlsx <- as.data.table(in.file.bulk)[name %like% "\\.xlsx$"]
    
    if (nrow(in.file.xlsx) > 0) {
      
      in.file.xlsx <- as.data.frame(in.file.xlsx[1])
      in.file <- NULL
      
    } else {
      
      in.file.xlsx <- NULL
      
    }
  }
  
  # choose source
  
  if (!is.null(in.file)) {
    
    if (cur.sep() == ";") {
      dt.init <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
    } else if (cur.sep() == ",") {
      dt.init <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
    } else if (cur.sep() == "tab") {
      dt.init <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
    }
    
    if (is.data.frame(dt.init)) {
      
      cln <- colnames(dt.init)
      colnames(dt.init) <- str_replace(cln, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0x2e), as.raw(0xbf)))), "")
      
    }
    
  } else if (!is.null(in.file.xlsx)) {
    
    shts <- getSheetNames(in.file.xlsx$datapath)
    
    shts <- shts[shts %like% "^(input_|output_)*data"]
    shts <- sort(shts)
    
    dt.init <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1]), silent = TRUE)
    
  }
  
  if (!is.data.frame(dt.init) || ncol(dt.init) != 2) {
    
    values$cur.status@model.status <- paste("The data file for curve fitting should contain exactly 1 column `label` for labels"
                                            , "and 1 column `value` for observed values."
                                            , "Check if the column separator provided is consistent with your data"
                                            , "and there are no surplus cells and columns")
    
  }
  
  validate(
    
    need(is.data.frame(dt.init), "Your file doesn't look like a data file (table-like)") %then%
      need(ncol(dt.init) == 2
           , paste("The data file for curve fitting should contain exactly 1 column `label` for labels"
                   , "and 1 column `value` for observed values"))
    
  )
  
  if (!is.null(dt.init)) {
    
    setDT(dt.init)
    values[["cur.dt.init"]] <- dt.init
    input.source$cur.is.file.loaded <- TRUE
    
  }
  
  
  # load params ----- #
  
  in.file.bulk <- input$file.cur.bulk.input
  in.file.xlsx <- NULL
  in.file <- NULL
  
  # bulk input
  
  if (!is.null(in.file.bulk)) {
    
    in.file <- as.data.table(in.file.bulk)[name %like% "^(input\\_)*param(eter)*s*(\\.csv|\\.txt)*$"][1]
    in.file <- as.data.frame(in.file)
    
    in.file.xlsx <- as.data.table(in.file.bulk)[name %like% "\\.xlsx$"]
    
    if (nrow(in.file.xlsx) > 0) {
      
      in.file.xlsx <- as.data.frame(in.file.xlsx[1])
      in.file <- NULL
      
    } else {
      
      in.file.xlsx <- NULL
      
    }
  }
  
  # choose source
  
  if (!is.null(in.file)) {
    
    if (cur.sep() == ";") {
      dt.par <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
    } else if (cur.sep() == ",") {
      dt.par <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
    } else if (cur.sep() == "tab") {
      dt.par <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
    }
    
    if (is.data.frame(dt.par)) {
      
      cln <- colnames(dt.par)
      colnames(dt.par) <- str_replace(cln, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0x2e), as.raw(0xbf)))), "")
      
    }
    
  } else if (!is.null(in.file.xlsx)) {
    
    shts <- getSheetNames(in.file.xlsx$datapath)
    
    shts <- shts[shts %like% "^(input_|output_)*param(eter)*s*"]
    shts <- sort(shts)
    
    dt.par <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1]), silent = TRUE)
    
  } else {
    
    dt.par <- NULL
    
  }
  
  # validate needs output to bw shown (which is impossible in event observer)
  # so use if-s
  
  if (!is.null(dt.par) && !is.data.frame(dt.par)) {
    
    values$cur.status@model.status <- paste("Params file exists but is not properly formatted"
                                            , "Check examples via `Check Examples` button")
    
  } else if (!is.null(dt.par) && ncol(dt.par) <= 1) {
    
    values$cur.status@model.status <- paste("Params file should contain > 1 columns. Check column delimiter")
    
  }
  
  # 
  
  validate(
    
    need(is.null(dt.par) | is.data.frame(dt.par), paste("Params file exists but is not properly formatted"
                                                        , "Check examples via `Check Examples` button")) %then%
      need(is.null(dt.par) | ncol(dt.par) > 1, paste("Params file should contain > 1 columns. Check column delimiter"))
  )
  
  if (!is.null(dt.par)) {
    
    setDT(dt.par)
    values$cur.dt.par <- dt.par
    
  }
  
}
, ignoreInit = TRUE)



# calculating -------------- #

# refresh status

observeEvent(values$cur.dt.init, {
  
  if (!is.null(values$cur.status)
      && !is.logical(all.equal(values$cur.dt.init, values$cur.status@dt.init, check.attributes = FALSE)))
    values$cur.status@dt.init <- values$cur.dt.init
  
})

observeEvent(values$cur.dt.par, {
  
  # update parameters in cur.status object for consistence
  
  if (is.null(values$cur.status) || is.null(values$cur.dt.par) ||
      !is.logical(all.equal(values$cur.dt.par, values$cur.status@dt.par, check.attributes = FALSE, ignore.row.order = TRUE))) {
    
    values$cur.status <- cur.data.runner(mode = "app"
                                         , sep = cur.sep()
                                         , subdir = ""
                                         , file = NULL
                                         , dt.list = list(dt.cur = cur.dt.init.data()
                                                          , dt.par = copy(values$cur.dt.par)))
    isolate(values$cur.dt.par <- cur.dt.par.sanitize(values$cur.status@dt.par))
  }
  
  # update input data in cur.status object for consistence
  
  if (!is.logical(all.equal(values$cur.dt.init, values$cur.status@dt.init, check.attributes = FALSE)))
    values$cur.dt.init <- values$cur.status@dt.init
  
  # update GUI inputs when new data is loaded from the disc
  
  if (input.source$cur.is.file.loaded) {
    
    updateNumericInput(session, "cur.window.left", value = values$cur.status@window.borders[1])
    updateNumericInput(session, "cur.window.right", value = values$cur.status@window.borders[2])
    isolate(updateSelectInput(session, "cur.task", selected = values$cur.status@cur.task))
    
  } else {
    
    values$cur.status@window.borders[1] <- input$cur.window.left
    values$cur.status@window.borders[2] <- input$cur.window.right
    
  }
  
  # define rows to insert, remove and update
  
  dt.cur.rows <- values$cur.status@dt.par[!is.na(design) & design != "", .(name, design)] %>% unique()
  dt.cur.rows[, cur.id := paste0(design, name, "_curve.row")]
  
  dt.cur.rows.insert <- dt.cur.rows[!(cur.id %in% cur.curve.rows$values[, cur.id]) |
                                      length(cur.curve.rows$values[, cur.id]) == 0 | input.source$cur.is.file.loaded]
  dt.cur.rows.update <- dt.cur.rows[(cur.id %in% cur.curve.rows$values[, cur.id]) & !(cur.id %in% dt.cur.rows.insert[, cur.id])]
  
  cur.to.remove <- cur.curve.rows$values[, cur.id]
  cur.to.remove <- cur.to.remove[!(cur.to.remove %in% dt.cur.rows.update[, cur.id])]
  
  
  # update existing rows
  
  if (nrow(dt.cur.rows.update) > 0 && values$cur.status@model.status == "OK") {
    
    dt.par.update <- copy(values$cur.dt.par)
    dt.par.update[, cur.id := paste0(design, name, "_curve.row")]
    
    dt.par.update <- dt.par.update[cur.id %in% dt.cur.rows.update[, cur.id]]
    dt.par.update[, input.id := paste0(design, name, "_", param)]
    
    for (i in 1:nrow(dt.par.update)) {
      
      # browser()
      updateNumericInput(session, dt.par.update[i, input.id], value = as.numeric(dt.par.update[i, value]))
      # values$cur.dt.par[name %in% dt.cur.rows.update[i, name]]
      
    }
    
    # print(dt.cur.rows.update[, cur.id])
    
  }
  
  # delete outdated rows
  
  if (length(cur.to.remove) > 0) {
    
    for (cr in cur.to.remove) {
      
      removeUI(paste0("#", escapeRegex(cr)), multiple = TRUE, immediate = FALSE)
      cur.curve.rows$values <- cur.curve.rows$values[!(cur.id %in% cr)]
      
    }
  }
  
  # insert new rows
  
  if (nrow(dt.cur.rows.insert) > 0) {
    
    # stop events to prevent overwriting with older data
    cur.update.status(0L)
    
    dt.par <- copy(values$cur.status@dt.par)
    dt.par[, input.id := paste0(design, name, "_", param)]
    
    for (nm in dt.cur.rows.insert[, name]) {
      
      params <- as.list(values$cur.status@dt.par[name == nm, value])
      names(params) <- values$cur.status@dt.par[name == nm, param]
      cur.curve.insert(values$cur.status@dt.par[name == nm, design][1]
                       , params
                       , fn.id = nm)
      
    }
    
  }
  
  # if (!is.logical(all.equal(values$cur.dt.par, values$cur.status@dt.par, check.attributes = FALSE, ignore.row.order = TRUE)))
  # values$cur.dt.par <- cur.dt.par.sanitize(values$cur.status@dt.par)
  
}, ignoreNULL = FALSE, priority = 1000)

# calculate

observeEvent(input$cur.exec.btn, {
  
  withProgress(message = "Computation... It may take some time", value = 0, {
    
    incProgress(.3)
    
    values$cur.status <- cur.model(values$cur.status, algorithm = cur.algorithms[label == input$cur.algorithm, value])
    
    incProgress(.5)
    
    if (!is.logical(all.equal(values$cur.dt.par, values$cur.status@dt.par, check.attributes = FALSE))) {
      
      values$cur.dt.par <- cur.dt.par.sanitize(values$cur.status@dt.par)
      
    }
    
    incProgress(.2)
    
  })
  
  
})



# output data -------------- #

cur.auc.data <- reactive({
  
  if (!is.null(values$cur.status) && !is.null(values$cur.status@dt.par)) {
    
    dt <- cur.auc(values$cur.status)
    
    dt[, .(curve = paste("Curve", name), area)]
    
  } else {
    
    NULL
    
  }
  
})

cur.object.effects.data <- reactive({
  
  if (!is.null(values$cur.status) && !is.null(values$cur.status@dt.par)) {
    
    dt <- cur.object.effects(values$cur.status)
    dt <- dt[, !c("observed", "predicted"), with = FALSE]
    
  } else {
    
    NULL
    
  }
  
})



# rendering ---------------- #

output$cur.dt.init <- renderRHandsontable({
  
  dt.init <- cur.dt.init.data()
  
  if (!is.null(dt.init)) {
    
    if (nrow(dt.init) > 20) {
      
      rhandsontable(dt.init, stretchH = "all", useTypes = FALSE, height = 550) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
      
    } else {
      
      rhandsontable(dt.init, stretchH = "all", useTypes = FALSE, height = NULL) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
      
    }
    
  }
  
})

output$cur.plot.curves <- renderPlotly({
  
  # temporary sync solution 
  cur.update.status(1L)
  
  validate(
    
    need(!is.null(values$cur.status), "Error loading or creating curves parameters")
    
  ) 
  
  dt <- values$cur.status@dt.init
  frm <- cur.formula.create(values$cur.status@dt.par, dt)
  
  extr.effects <- cur.formula.effects(dt, frm$formula, frm$start.values)
  
  cln <- colnames(extr.effects)
  cln <- cln[cln %like% "^(Curve .*|label)$"]
  
  lbl.perc <- (dt[, max(label)] - dt[, min(label)]) / 100
  
  # pure plotly plot - not ggplotly to make it editable
  
  cln <- colnames(extr.effects)
  cln <- cln[!(cln %in% c("label", "observed", "predicted"))] %>% sort()
  
  # observed and predicted
  
  g <- plot_ly(height = 600, source = "cur.plot.curves") %>%
    add_lines(x = extr.effects[, label], y = extr.effects[, predicted], name = "Predicted"
              , line = list(color = "darkblue", dash = "dash", width = 4)) %>%
    add_lines(x = extr.effects[, label], y = extr.effects[, observed], name = "Observed", fill = "tozeroy"
              , line = list(color = "rgb(155, 155, 155)"), fillcolor = "rgba(155, 155, 155, 0.5)")
  
  # curves
  
  for (cl in cln) {
    
    g <- add_lines(g, x = extr.effects[, label], y = extr.effects[, eval(as.name(cl))], name = cl)
    
  }
  
  # window borders
  
  g <- add_trace(g, type = "scatter", fill = "toself", mode = "none"
                 , x = c(rep(dt[, min(label)] - 2 * lbl.perc, 2), rep(values$cur.status@window.borders[1], 2))
                 , y = c(0, rep(dt[, max(value) * 1.1], 2), 0)
                 , fillcolor = "rgba(155, 155, 155, 0.2)"
                 , name = "", showlegend = FALSE)
  
  g <- add_trace(g, type = "scatter", fill = "toself", mode = "none"
                 , x = c(rep(values$cur.status@window.borders[2], 2), rep(dt[, max(label)] + 2 * lbl.perc, 2))
                 , y = c(0, rep(dt[, max(value) * 1.1], 2), 0)
                 , fillcolor = "rgba(155, 155, 155, 0.2)"
                 , name = "", showlegend = FALSE)
  
  # define curve controls
  
  dt.par <- copy(values$cur.dt.par)
  dt.par[, cur.id := paste0(design, name, "_curve.row")]
  
  dt.par <- dcast.data.table(dt.par, cur.id + name ~ param, value.var = "value", fun.aggregate = sum)
  dt.par <- dt.par[order(name)]
  
  peak.points <- 
    mapply(function(x, y) {
      list(
        type = "circle"
        , line = list(color = "black")
        , x0 = -3, x1 = 3
        , y0 = -3, y1 = 3
        , xsizemode = "pixel" 
        , ysizemode = "pixel"
        , xanchor = x, yanchor = y
        
      )
    } , dt.par[, expvalue]
    , dt.par[, amplitude]
    , SIMPLIFY = FALSE
    )
  
  # curve controls indices to match with curve names
  
  dt.par[, plot.id := 0:(nrow(dt.par)-1)]
  
  mapply(function(c.id, p.id) {
    
    if (!is.null(cur.curve.rows$values))
      cur.curve.rows$values[cur.id == c.id, plot.id := p.id]
    
  }, dt.par[, cur.id], dt.par[, plot.id])
  
  # curve controls into layout
  
  g <- g %>%
    layout(xaxis = list(title = input$cur.plot.labels, gridcolor = "white")
           , yaxis = list(title = input$cur.plot.values, gridcolor = "white")
           , plot_bgcolor = "#ebebeb"
           , shapes = peak.points
    ) %>%
    config(edits = list(shapePosition = TRUE))
  
  g
  
})

output$cur.title <- renderUI({
  
  mod.st <- values$cur.status@model.status
  
  if (mod.st == "OK") {
    
    mod.st <- paste0("<h4>Status: ", mod.st
                     , ". R<sup>2</sup>: ", round(values$cur.status@metrics$r.squared, 4)
                     , ". RMSE: ", signif(values$cur.status@metrics$rmse, 4)
                     , ". MAE: ", signif(values$cur.status@metrics$mae, 4)
                     , "</h4>")
  } else {
    
    mod.st <- paste0("<h4>Status: ", mod.st)
    
  }
  
  HTML(mod.st)
  
})

output$cur.auc <- renderRHandsontable({
  
  dt <- cur.auc.data()
  
  if (!is.null(dt)) {
    
    if (nrow(dt) > 20) {
      
      rhandsontable(dt, stretchH = "all", useTypes = TRUE, height = 550) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      
    } else {
      
      rhandsontable(dt, stretchH = "all", useTypes = TRUE, height = NULL) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      
    }
    
  }
  
})

output$cur.dt.par <- renderRHandsontable({
  
  dt <- cur.dt.par.data()
  
  dt <- dt[!is.na(design) & !(design == "")]
  
  if (!is.null(dt) && nrow(dt) > 0) {
    
    dt <- dt[order(name, design, param)]
    
    cln <- colnames(dt)
    row_highlight <- integer(0)
    
    if (length(cln[cln == "validity"]) > 0)
      row_highlight <- dt[validity != "OK", which = TRUE] - 1
    
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
    
    if (nrow(dt) > 20) {
      
      rhandsontable(dt, stretchH = "all", row_highlight = row_highlight, useTypes = TRUE, height = 550) %>%
        hot_cols(renderer = renderer)
      
    } else {
      
      rhandsontable(dt, stretchH = "all", row_highlight = row_highlight, useTypes = TRUE, height = NULL) %>%
        hot_cols(renderer = renderer)
      
    }
    
  }
  
})

output$cur.object.effects <- renderRHandsontable({
  
  dt <- cur.object.effects.data()
  
  if (!is.null(dt)) {
    
    if (nrow(dt) > 20) {
      
      rhandsontable(dt, stretchH = "all", useTypes = FALSE, height = 550) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      
    } else {
      
      rhandsontable(dt, stretchH = "all", useTypes = FALSE, height = NULL) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      
    }
    
  }
  
})


