# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #



# ---------------------- load libraries ----------------------

# I/O
library(openxlsx)
# data structure
library(data.table)
# computation
library(MASS)
library(Matrix)
# library(Hmisc)
# strings
library(stringi)
library(stringr)
# reporting
library(shiny)
library(rhandsontable)



# ------------------------- settings ------------------------

# prepare environment

if (Sys.info()['sysname'] %like% "indows")
  Sys.setenv("R_ZIPCMD" = "c:/Rtools/bin/zip.exe")

options(shiny.sanitize.errors = TRUE)
`%then%` <- shiny:::`%OR%`

# load algorithm

source("eq_runner.r", chdir = TRUE)



# frontend ------------------------------------------------- #

ui <- navbarPage("KEV",
                 windowTitle = "KEV: Constant evaluator",
                 tabPanel("Equilibrium concentrations"
                          , id = "page.eq.conc"
                          
                          , fluidPage(
                            
                            includeCSS("styles.css")
                            
                            , titlePanel("KEV: Chemistry Constant Evaluator")
                            
                            , fluidRow(column(12), p(HTML(paste("<br/>"))))
                            
                            , titlePanel("Calculate Equilibrium concentrations")
                            
                            , fluidRow(column(
                              12
                              , wellPanel(
                                fluidRow(column(6
                                                , h4("What column delimiter do your use in your data files?")
                                                , radioButtons("sep", "", inline = TRUE
                                                               , c("," = "comma"
                                                                   , ";" = "semicolon"
                                                                   , "tab" = "tab"))
                                         )
                                         , column(6
                                                  , h4("Particle to get fractions of")
                                                  , textInput("bs.name", "", "molecule1")
                                         ))
                              )))
                            
                            , fluidRow(
                              
                              column(
                                12
                                , wellPanel(
                                  h4("Upload or type input data")
                                  ,  fluidRow(
                                    column(5
                                           , h4("Stoichiometric coefficients")
                                           , rHandsontableOutput("dt.coef")
                                           , fileInput("file.dt.coef", "Choose CSV File",
                                                       accept = c(
                                                         "text/csv",
                                                         "text/comma-separated-values,text/plain",
                                                         ".csv")
                                           )
                                           , fluidRow(class = "download-row"
                                                      , downloadButton("dt.coef.csv", "csv")
                                                      , downloadButton("dt.coef.xlsx", "xlsx"))
                                           , p("")
                                           , textInput("part.names", "Particle names, comma separated", paste(paste0("molecule", 1:4), collapse = ", "))
                                           )
                                    , column(2
                                             , h4("K: lg constants")
                                             , rHandsontableOutput("cnst")
                                             , fileInput("file.cnst", "Choose CSV File",
                                                         accept = c(
                                                           "text/csv",
                                                           "text/comma-separated-values,text/plain",
                                                           ".csv")
                                             )
                                             , fluidRow(class = "download-row"
                                                        , downloadButton("cnst.csv", "csv")
                                                        , downloadButton("cnst.xlsx", "xlsx"))
                                             )
                                    , column(5
                                             , h4("Concentrations")
                                             , tabsetPanel(type = "tabs"
                                                           , tabPanel("Input"
                                                                      , rHandsontableOutput("dt.conc")
                                                                      , rHandsontableOutput("part.eq")
                                                                      , fluidRow(class = "download-row"
                                                                                 , downloadButton("dt.conc.csv", "csv")
                                                                                 , downloadButton("dt.conc.xlsx", "xlsx")))
                                                           , tabPanel("Total"
                                                                      , rHandsontableOutput("dt.conc.tot")
                                                                      , fluidRow(class = "download-row"
                                                                                 , downloadButton("dt.conc.tot.csv", "csv")
                                                                                 , downloadButton("dt.conc.tot.xlsx", "xlsx")))
                                                           )
                                             , fileInput("file.dt.conc", "Choose CSV File",
                                                         accept = c(
                                                           "text/csv",
                                                           "text/comma-separated-values,text/plain",
                                                           ".csv"))
                                             )
                                    )
                                  
                                 )
                              )
                            )
                            
                            , fluidRow(column(
                              12
                              , wellPanel(
                                fluidRow(column(12
                                                , actionButton("eq.conc.exec.btn", "Evaluate")
                                ))
                              )))
                            
                            , fluidRow(column(
                              12
                              , wellPanel(
                                fluidRow(column(12
                                                , h4("Equilibrium concentrations")
                                                , rHandsontableOutput("dt.res")
                                                , fluidRow(class = "download-row"
                                                           , downloadButton("dt.res.csv", "csv")
                                                           , downloadButton("dt.res.xlsx", "xlsx"))))

                                , fluidRow(column(12
                                                  , h4(textOutput("txt.frac"))
                                                  , rHandsontableOutput("dt.frac")
                                                  , fluidRow(class = "download-row"
                                                             , downloadButton("dt.frac.csv", "csv")
                                                             , downloadButton("dt.frac.xlsx", "xlsx"))))
                                
                                , fluidRow(column(12
                                                  , h4("Residuals matrix")
                                                  , rHandsontableOutput("dt.err")
                                                  , fluidRow(class = "download-row"
                                                             , downloadButton("dt.err.csv", "csv")
                                                             , downloadButton("dt.err.xlsx", "xlsx"))))
                              ))
                              
                            )
                            
                          )),
                 tabPanel("(2): To do"),
                 tabPanel("About")
)


# backend -------------------------------------------------- #

server <- function(input, output, session) {
  
  # technical
   
  values <- reactiveValues()
  
  sep <- reactive({
    
    switch(input$sep,
           comma = ",",
           semicolon = ";",
           tab = "tab")
    
  })
  
  
  # data --------------------- #
  
  # input data
  
  part.names.data <- reactive({
    
    tmp <- input$part.names
    
    tmp <- str_split(tmp, pattern = ",")[[1]]
    tmp <- str_trim(tmp)
    
    tmp
    
  })
  
  dt.coef.data <- reactive({
    
    if (!is.null(input$dt.coef)) {
      
      dt.coef <- hot_to_r(input$dt.coef)
      
    } else {
      
      if (is.null(values[["dt.coef"]])) {
        
        dt.coef <- as.data.table(matrix(rep(1, 16), 4))
        setnames(dt.coef, paste0("molecule", 1:4))
        
      } else {
        
        dt.coef <- values[["dt.coef"]]
        
      }
        
    }
    
    dt.coef <- as.data.table(dt.coef)
    setnames(dt.coef, part.names.data()[1:ncol(dt.coef)])
    
    values[["dt.coef"]] <- dt.coef
    
    dt.coef
    
  })

  dt.conc.data <- reactive({
    
    if (!is.null(input$dt.conc)) {
      
      dt.conc <- hot_to_r(input$dt.conc)
      
    } else {
      
      if (is.null(values[["dt.conc"]])) {
        
        dt.conc <- as.data.table(matrix(rep(1e-03, 20), ncol = 4))
        setnames(dt.conc, paste0("molecule", 1:4))
        
      } else {
        
        dt.conc <- values[["dt.conc"]]
        
      }
      
    }
    
    dt.conc <- as.data.table(dt.conc)
    setnames(dt.conc, part.names.data()[1:ncol(dt.conc)])
    
    values[["dt.conc"]] <- dt.conc
    
    dt.conc
    
  })

  part.eq.data <- reactive({
    
    if (!is.null(input$part.eq)) {
      
      part.eq <- hot_to_r(input$part.eq)
      
    } else {
      
      if (is.null(values[["part.eq"]])) {
        
        part.eq <- as.data.table(matrix(rep("tot", 4), ncol = 4))
        setnames(part.eq, paste0("molecule", 1:4))
        
      } else {
        
        part.eq <- values[["part.eq"]]
        
      }
      
    }
    
    part.eq <- as.data.table(part.eq)
    
    values[["part.eq"]] <- part.eq
    
    part.eq
    
  })
  
  cnst.data <- reactive({
    
    if (!is.null(input$cnst)) {
      
      cnst <- hot_to_r(input$cnst)
      
    } else {
      
      if (is.null(values[["cnst"]])) {
        
        cnst <- as.data.table(matrix(rep(1, 4), ncol = 1))
        setnames(cnst, "log10(K)")
        
      } else {
        
        cnst <- values[["cnst"]]
        
      }
      
    }
    
    cnst <- as.data.table(cnst)
    
    values[["cnst"]] <- cnst
    
    cnst
    
  })
  
  bs.name <- reactive({
    
    if (!is.null(input$bs.name)) {
      
      input$bs.name
      
    } else {
      
      "molecule1"
      
    }
    
  })
  
  # execute
  
  eval.data <- reactive({
    
    validate(
      
      need(length(colnames(dt.coef.data())[colnames(dt.coef.data()) == bs.name()]) > 0, "Input correct particle name to get fractions of")
      
    )
    
    eq.evaluation.runner(mode = "app"
                         , sep = sep()
                         , bs.name = bs.name()
                         , thr.type = c("rel")
                         , threshold = 1e-08
                         , dt.list = list(dt.coef = dt.coef.data()
                                          , cnst = cnst.data()
                                          , dt.conc = dt.conc.data()
                                          , part.eq = part.eq.data())
                         , save.res = FALSE)
    
  })
  
  # output data
  
  dt.res.data <- eventReactive(input$eq.conc.exec.btn, {
    
    eval.data()$dt.res
    
  })
  
  dt.frac.data <- eventReactive(input$eq.conc.exec.btn, {
    
    eval.data()$dt.frac
    
  })

  dt.err.data <- eventReactive(input$eq.conc.exec.btn, {
    
    eval.data()$dt.err
    
  })

  dt.conc.tot.data <- eventReactive(input$eq.conc.exec.btn, {
    
    eval.data()$dt.conc.tot
    
  })
  

  
  # text --------------------- #
  
  output$txt.frac <- renderText(
    {
      paste("Fractions per ", bs.name())
    }
  )
  
  
  
  
  # rendering ---------------- #
  
  output$dt.coef <- renderRHandsontable({
    
    in.file <- input$file.dt.coef
    
    if (!is.null(in.file)) {
      
      if (sep() == ";") {
        dt.coef <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character"), silent = TRUE)
      } else if (sep() == ",") {
        dt.coef <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character"), silent = TRUE)
      } else if (sep() == "tab") {
        dt.coef <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character"), silent = TRUE)
      }
      
      validate(
        
        need(is.data.frame(dt.coef), "Your file doesn't look like a stoich. coefficients file") %then%
        need(dt.coef[1, 1][!(dt.coef[1, 1] %like% "[a-zA-Z]")], "Your file doesn't look like a stoich. coefficients file")
        
      )
      
      tmp <- colnames(dt.coef)
      updateTextInput(session, "part.names", value = paste(tmp, collapse = ", "))
      
      
    } else {
      
      dt.coef <- dt.coef.data()
      
    }
    
    setnames(dt.coef, part.names.data()[1:ncol(dt.coef)])
    
    if (!is.null(dt.coef))
      rhandsontable(dt.coef, stretchH = "all", useTypes = FALSE) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    
  })
  
  output$dt.conc <- renderRHandsontable({
    
    in.file <- input$file.dt.conc
    
    if (!is.null(in.file)) {
      
      if (sep() == ";") {
        dt.conc <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1), silent = TRUE)
      } else if (sep() == ",") {
        dt.conc <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1), silent = TRUE)
      } else if (sep() == "tab") {
        dt.conc <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1), silent = TRUE)
      }
      
      validate(need(is.data.frame(dt.conc), "Check the column delimiter or content of your file"))
      
      tmp <- colnames(dt.conc)
      updateTextInput(session, "part.names", value = paste(tmp, collapse = ", "))
      
      
    } else {
      
      dt.conc <- dt.conc.data()
      
    }
    
    setnames(dt.conc, part.names.data()[1:ncol(dt.conc)])
    
    if (!is.null(dt.conc))
      rhandsontable(dt.conc, stretchH = "all", useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    
  })

  output$part.eq <- renderRHandsontable({
    
    in.file <- input$file.dt.conc
    
    part.eq <- part.eq.data()
    
    if (!is.null(in.file)) {
      
      
      if (sep() == ";") {
        
        part.eq <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", nrows = 1, header = FALSE), silent = TRUE)
        tmp <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1, header = FALSE)[1, ], silent = TRUE)
        
      } else if (sep() == ",") {
        
        part.eq <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", nrows = 1, header = FALSE), silent = TRUE)
        tmp <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1, header = FALSE)[1, ], silent = TRUE)
        
      } else if (sep() == "tab") {
        
        part.eq <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", nrows = 1, header = FALSE), silent = TRUE)
        tmp <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1, header = FALSE)[1, ], silent = TRUE)
        
      }

      validate(
        
        need(is.data.frame(part.eq), "Check the column delimiter or content of your file") %then%
        need(ncol(part.eq) == ncol(tmp), "Check the column delimiter or content of your file")
        
      )
      
      colnames(part.eq) <- tmp

    }
    
    if (!is.null(part.eq))
      rhandsontable(part.eq, stretchH = "all", useTypes = FALSE, colHeaders = NULL) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    
  })
  
  output$cnst <- renderRHandsontable({
    
    in.file <- input$file.cnst
    
    cnst <- cnst.data()
    
    if (!is.null(in.file)) {
      
      if (sep() == ";") {
        cnst <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character"), silent = TRUE)
      } else if (sep() == ",") {
        cnst <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character"), silent = TRUE)
      } else if (sep() == "tab") {
        cnst <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character"), silent = TRUE)
      }
     
      validate(
        need(is.data.frame(cnst), "Check the column delimiter or content of your file") %then%
          need(ncol(cnst) == 1, "Check the column delimiter or content of your file")
      )
       
    }
    
    if (!is.null(cnst))
      rhandsontable(cnst, stretchH = "all", useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    
  })

  output$dt.res <- renderRHandsontable({
    
    dt.res <- dt.res.data()
    
    if (!is.null(dt.res))
      
      rhandsontable(dt.res, stretchH = FALSE, useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    
  })
  
  output$dt.frac <- renderRHandsontable({
    
    dt.frac <- dt.frac.data()
    
    if (!is.null(dt.frac)) {
      
      dt.frac <- as.data.table(t(dt.frac), keep.rownames = TRUE)
      setnames(dt.frac, unlist(dt.frac[1]))
      
      dt.frac <- dt.frac[!1]

      rhandsontable(dt.frac, stretchH = FALSE, useTypes = FALSE) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      
    }
    
  })

  output$dt.err <- renderRHandsontable({
    
    dt.err <- dt.err.data()
    
    if (!is.null(dt.err))
      rhandsontable(dt.err, stretchH = FALSE, useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    
  })

  output$dt.conc.tot <- renderRHandsontable({
    
    dt.conc.tot <- dt.conc.tot.data()
    
    if (!is.null(dt.conc.tot))
      
      rhandsontable(dt.conc.tot, stretchH = FALSE, useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    
  })
  
  
  

  # downoad ---------------- #
  
  output$dt.coef.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "input_stoichiometric_coefficients.csv"
      
    },
    
    content = function(file) {
      
      if (sep() == ";") {
        write.csv2(dt.coef.data(), file, row.names = FALSE)
      } else {
        write.csv(dt.coef.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$dt.coef.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "input_stoichiometric_coefficients.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(dt.coef.data(), file)
      
    }
    
  )
  # ----
  
  output$cnst.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "k_constants_log10.csv"
      
    },
    
    content = function(file) {
      
      if (sep() == ";") {
        write.csv2(cnst.data(), file, row.names = FALSE)
      } else {
        write.csv(cnst.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$cnst.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "k_constants_log10.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(cnst.data(), file)
      
    }
    
  )
  # ----
  
  output$dt.conc.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "input_concentrations.csv"
      
    },
    
    content = function(file) {
      
      tmp <- dt.conc.data()
      tmp <- rbind(data.table(t(data.table(colnames(tmp)))), tmp, use.names = FALSE)
      
      setnames(tmp, unlist(part.eq.data()))
      
      if (sep() == ";") {
        write.csv2(tmp, file, row.names = FALSE)
      } else {
        write.csv(tmp, file, row.names = FALSE)
      }
      
    }

  )
  # ----

  output$dt.conc.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "input_concentrations.xlsx"
      
    },
    
    content = function(file) {
      
      tmp <- dt.conc.data()
      tmp <- rbind(data.table(t(data.table(colnames(tmp)))), tmp, use.names = FALSE)
      
      setnames(tmp, unlist(part.eq.data()))
      
      write.xlsx(tmp, file)
      
    }
    
  )
  # ----
  
  output$dt.conc.tot.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "total_concentrations.csv"
      
    },
    
    content = function(file) {
      
      tmp <- try(dt.conc.tot.data())
      
      if (!is.data.frame(tmp))
        tmp <- data.frame(error = "Evaluate before downloading total concentrations")

      if (sep() == ";") {
        write.csv2(tmp, file, row.names = FALSE)
      } else {
        write.csv(tmp, file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$dt.conc.tot.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "total_concentrations.xlsx"
      
    },
    
    content = function(file) {
      
      tmp <- try(dt.conc.tot.data())
      
      if (!is.data.frame(tmp))
        tmp <- data.frame(error = "Evaluate before downloading total concentrations")
      
      write.xlsx(tmp, file)
      
    }
    
  )
  # ----
  
  output$dt.res.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "equilibrium_concentrations.csv"
      
    },
    
    content = function(file) {
      
      if (sep() == ";") {
        write.csv2(dt.res.data(), file, row.names = FALSE)
      } else {
        write.csv(dt.res.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$dt.res.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "equilibrium_concentrations.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(dt.res.data(), file)
      
    }
    
  )
  # ----

  output$dt.frac.csv <- downloadHandler(
    # ----
    filename = function() {
      
      paste0(bs.name, "_fractions.csv")
      
    },
    
    content = function(file) {
      
      if (sep() == ";") {
        write.csv2(dt.frac.data(), file, row.names = FALSE)
      } else {
        write.csv(dt.frac.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$dt.frac.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      paste0(bs.name, "_fractions.xlsx")
      
    },
    
    content = function(file) {
      
      write.xlsx(dt.frac.data(), file)
      
    }
    
  )
  # ----
  
  output$dt.err.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "percent_error.csv"
      
    },
    
    content = function(file) {
      
      if (sep() == ";") {
        write.csv2(dt.err.data(), file, row.names = FALSE)
      } else {
        write.csv(dt.err.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$dt.err.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "percent_error.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(dt.err.data(), file)
      
    }
    
  )
  # ----
  
  
  
}

# run

shinyApp(ui = ui, server = server)

