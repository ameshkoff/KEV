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

if (Sys.info()["sysname"] %like% "indows")
  Sys.setenv("R_ZIPCMD" = "c:/Rtools/bin/zip.exe")

options(shiny.sanitize.errors = TRUE)
`%then%` <- shiny:::`%OR%`

# load algorithm

source("eq_runner.r", chdir = TRUE)
source("ab_runner.r", chdir = TRUE)



# frontend ------------------------------------------------- #

ui <- navbarPage("KEV",
                 windowTitle = "KEV: Constant evaluator",
                 
# equilibrium concentrations -------------------------

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
                                                , h4("Column delimiter of your data files")
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

# absorbance ----------------------------------

                 tabPanel("Absorbance"
                          , id = "page.ab"
                          
                          , fluidPage(
                            
                            includeCSS("styles.css")
                            
                            , titlePanel("KEV: Chemistry Constant Evaluator")
                            
                            , fluidRow(column(12), p(HTML(paste("<br/>"))))
                            
                            , titlePanel("Calculate Equilibrium Constants")
                            
                            , fluidRow(column(
                              12
                              , wellPanel(
                                fluidRow(column(3
                                                , h4("Column delimiter")
                                                , radioButtons("ab.sep", "", inline = TRUE
                                                               , c("," = "comma"
                                                                   , ";" = "semicolon"
                                                                   , "tab" = "tab")))
                                         , column(3
                                                  , HTML("<h4>Constants to evaluate</h4><p>Particle names, comma separated</p>")
                                                  , textInput("cnst.tune", "", "molecule1"))
                                         , column(3
                                                  , HTML(paste("<h4>Threshold</h4><p>Search algorithm precision"
                                                                ,"0&nbsp;&#60;&nbsp;&#950;&nbsp;&#60;&nbsp;1</p>"))
                                                  , textInput("ab.threshold", "", "1e-7"))
                                         , column(3
                                                  , HTML("<h4>Search density</h4><p>Do not change untill you fully understand what you do</p>")
                                                  , textInput("search.density", "", "1"))
                                )
                              )))
                            
                            , fluidRow(column(
                              12
                              , wellPanel(
                                fluidRow(column(12
                                                , h4("Bulk upload / download (optional)")))
                                
                                , fluidRow(column(6
                                                  , h4("Upload all data")
                                                  , fileInput("file.bulk.input", "Choose CSV files or XLSX file with multiple sheets",
                                                              accept = c(
                                                                "text/csv"
                                                                , "text/comma-separated-values,text/plain"
                                                                , ".csv"
                                                                , ".xlsx")
                                                              , multiple = TRUE
                                                  ))
                                           , column(6
                                                    , h4("Download all data")
                                                    , fluidRow(class = "download-row"
                                                               , downloadButton("kev.data.zip", "zip")
                                                               , downloadButton("kev.data.xlsx", "xlsx"))))
                              )))
                            
                            , fluidRow(
                              
                              column(
                                12
                                , wellPanel(
                                  h4("Upload or type input data")
                                  ,  fluidRow(
                                    column(5
                                           , h4("Stoichiometric coefficients")
                                           , rHandsontableOutput("ab.dt.coef")
                                           , fileInput("file.ab.dt.coef", "Choose CSV File",
                                                       accept = c(
                                                         "text/csv",
                                                         "text/comma-separated-values,text/plain",
                                                         ".csv")
                                           )
                                           , fluidRow(class = "download-row"
                                                      , downloadButton("ab.dt.coef.csv", "csv")
                                                      , downloadButton("ab.dt.coef.xlsx", "xlsx"))
                                           , p("")
                                           , textInput("ab.part.names", "Particle names, comma separated"
                                                       , paste(paste0("molecule", 1:4), collapse = ", "))
                                    )
                                    , column(2
                                             , h4("K: lg constants")
                                             , rHandsontableOutput("ab.cnst")
                                             , fileInput("file.ab.cnst", "Choose CSV File",
                                                         accept = c(
                                                           "text/csv",
                                                           "text/comma-separated-values,text/plain",
                                                           ".csv")
                                             )
                                             , fluidRow(class = "download-row"
                                                        , downloadButton("ab.cnst.csv", "csv")
                                                        , downloadButton("ab.cnst.xlsx", "xlsx"))
                                    )
                                    , column(5
                                             , h4("Concentrations")
                                             , rHandsontableOutput("ab.dt.conc")
                                             , rHandsontableOutput("ab.part.eq")
                                             , fileInput("file.ab.dt.conc", "Choose CSV File",
                                                         accept = c(
                                                           "text/csv",
                                                           "text/comma-separated-values,text/plain",
                                                           ".csv")
                                             )
                                             , fluidRow(class = "download-row"
                                                        , downloadButton("ab.dt.conc.csv", "csv")
                                                        , downloadButton("ab.dt.conc.xlsx", "xlsx"))
                                    )
                                  )
                                  , fluidRow(
                                    column(5
                                             , h4("Absorbance and deviations")
                                             , rHandsontableOutput("dt.ab")
                                             , fileInput("file.dt.ab", "Choose CSV File",
                                                         accept = c(
                                                           "text/csv",
                                                           "text/comma-separated-values,text/plain",
                                                           ".csv")
                                             )
                                             , fluidRow(class = "download-row"
                                                        , downloadButton("dt.ab.csv", "csv")
                                                        , downloadButton("dt.ab.xlsx", "xlsx"))

                                    )
                                    , column(5
                                             , h4("Molar extinction coefficients")
                                             , rHandsontableOutput("dt.mol")
                                             , fileInput("file.dt.mol", "Choose CSV File",
                                                         accept = c(
                                                           "text/csv",
                                                           "text/comma-separated-values,text/plain",
                                                           ".csv")
                                             )
                                             , fluidRow(class = "download-row"
                                                        , downloadButton("dt.mol.csv", "csv")
                                                        , downloadButton("dt.mol.xlsx", "xlsx"))
                                    )
                                  )
                                )
                              )
                            )
                            
                            , fluidRow(column(
                              12
                              , wellPanel(
                                fluidRow(column(12
                                                , actionButton("ab.conc.exec.btn", "Evaluate")
                                ))
                              )))
                            
                            , fluidRow(column(
                              12
                              , wellPanel(
                                fluidRow(column(12
                                                , h4("Equilibrium concentrations")
                                                , rHandsontableOutput("ab.dt.res")
                                                , fluidRow(class = "download-row"
                                                           , downloadButton("ab.dt.res.csv", "csv")
                                                           , downloadButton("ab.dt.res.xlsx", "xlsx"))))
                                
                                , fluidRow(column(12
                                                  , h4("Calculated Absorbance")
                                                  , tabsetPanel(type = "tabs"
                                                                , tabPanel("Absolute Errors"
                                                                           , rHandsontableOutput("dt.ab.abs")
                                                                           , fluidRow(class = "download-row"
                                                                                      , downloadButton("dt.ab.abs.csv", "csv")
                                                                                      , downloadButton("dt.ab.abs.xlsx", "xlsx")))
                                                                , tabPanel("Relative Errors"
                                                                           , rHandsontableOutput("dt.ab.rel")
                                                                           , fluidRow(class = "download-row"
                                                                                      , downloadButton("dt.ab.rel.csv", "csv")
                                                                                      , downloadButton("dt.ab.rel.xlsx", "xlsx")))
                                                  )))
                                
                                , fluidRow(column(12)
                                           , column(4
                                                    , h4("Evaluated Constants")
                                                    , rHandsontableOutput("cnst.dev")
                                                    , fluidRow(class = "download-row"
                                                               , downloadButton("cnst.dev.csv", "csv")
                                                               , downloadButton("cnst.dev.xlsx", "xlsx")))
                                           , column(4
                                                    , h4("Correlation Matrix")
                                                    , rHandsontableOutput("cor.m")
                                                    , fluidRow(class = "download-row"
                                                               , downloadButton("cor.m.csv", "csv")
                                                               , downloadButton("cor.m.xlsx", "xlsx")))
                                           , column(4
                                                    , h4("Last Fmin Step")
                                                    , rHandsontableOutput("err.diff")
                                                    , fluidRow(class = "download-row"
                                                               , downloadButton("err.diff.csv", "csv")
                                                               , downloadButton("err.diff.xlsx", "xlsx"))))
                                
                                , fluidRow(column(12
                                                  , h4("Extinction Molar Coefficients with St.Deviations")
                                                  , rHandsontableOutput("mol.coef")
                                                  , fluidRow(class = "download-row"
                                                             , downloadButton("mol.coef.csv", "csv")
                                                             , downloadButton("mol.coef.xlsx", "xlsx"))))
                                
                                
                              ))
                            )
                            
                          )
                          
                 ),
# about ---------------------------------------
                 tabPanel("About")
)


# backend -------------------------------------------------- #

server <- function(input, output, session) {

  
  values <- reactiveValues()
  
  
  # equilibrium concentrations -------------------------
  
  # technical
   
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
  
  
  # absorbance -----------------------------------------
  
  # technical
  
  ab.sep <- reactive({
    
    switch(input$ab.sep,
           comma = ",",
           semicolon = ";",
           tab = "tab")
    
  })
  
  input.source <- reactiveValues(
    
    ab.dt.coef.bulk = FALSE
    , ab.dt.conc.bulk = FALSE
    , ab.cnst.bulk = FALSE
    , dt.ab.bulk = FALSE
    , dt.mol.bulk = FALSE
    
  )
  
  observeEvent(input$file.bulk.input, {
    
    # stoichiometric coefficients
    
    if (nrow(as.data.table(input$file.bulk.input)[name %like% "^(input\\_)*stoich(iometric)*\\_coefficients(\\.csv|\\.txt)*"]) > 0){
      input.source$ab.dt.coef.bulk <- TRUE
    }
    
    if (nrow(as.data.table(input$file.bulk.input)[name %like% "\\.xlsx$"]) > 0){
      
      shts <- getSheetNames(input$file.bulk.input$datapath)
      
      if (length(shts[shts %in% "stoich_coefficients"]))
        input.source$ab.dt.coef.bulk <- TRUE
      
    }
    
    # concentrations
    
    if (nrow(as.data.table(input$file.bulk.input)[name %like% "^(input\\_)*concentrations(\\.csv|\\.txt)*"]) > 0){
      input.source$ab.dt.conc.bulk <- TRUE
    }
    
    if (nrow(as.data.table(input$file.bulk.input)[name %like% "\\.xlsx$"]) > 0){
      
      shts <- getSheetNames(input$file.bulk.input$datapath)
      
      if (length(shts[shts %in% "concentrations"]))
        input.source$ab.dt.conc.bulk <- TRUE
      
    }
    
    # constants
    
    if (nrow(as.data.table(input$file.bulk.input)[name %like% "^(input\\_)*k\\_constants\\_log10(\\.csv|\\.txt)*"]) > 0){
      input.source$ab.cnst.bulk <- TRUE
    }
    
    if (nrow(as.data.table(input$file.bulk.input)[name %like% "\\.xlsx$"]) > 0){
      
      shts <- getSheetNames(input$file.bulk.input$datapath)
      
      if (length(shts[shts %in% "k_constants_log10"]))
        input.source$ab.cnst.bulk <- TRUE
      
    }
    
    # absorbance
    
    if (nrow(as.data.table(input$file.bulk.input)[name %like% "^(input\\_)*absorbance(\\.csv|\\.txt)*"]) > 0){
      input.source$dt.ab.bulk <- TRUE
    }
    
    if (nrow(as.data.table(input$file.bulk.input)[name %like% "\\.xlsx$"]) > 0){
      
      shts <- getSheetNames(input$file.bulk.input$datapath)
      
      if (length(shts[shts %in% "absorbance"]))
        input.source$dt.ab.bulk <- TRUE
      
    }
    
    # molar extinction coefficients
    
    if (nrow(as.data.table(input$file.bulk.input)[name %like% "^(input\\_)*mol(ar)*\\_ext(inction)*\\_coefficients(\\.csv|\\.txt)*"]) > 0){
      input.source$dt.mol.bulk <- TRUE
    }
    
    if (nrow(as.data.table(input$file.bulk.input)[name %like% "\\.xlsx$"]) > 0){
      
      shts <- getSheetNames(input$file.bulk.input$datapath)
      
      if (length(shts[shts %in% "mol_ext_coefficients"]))
        input.source$dt.mol.bulk <- TRUE
      
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
    
  }, priority = 1000)
  
  
  # data --------------------- #
  
  # input data
  
  ab.part.names.data <- reactive({
    
    tmp <- input$ab.part.names
    
    tmp <- str_split(tmp, pattern = ",")[[1]]
    tmp <- str_trim(tmp)
    
    tmp
    
  })
  
  ab.dt.coef.data <- reactive({
    
    if (!is.null(input$ab.dt.coef)) {
      
      dt.coef <- hot_to_r(input$ab.dt.coef)
      
    } else {
      
      if (is.null(values[["ab.dt.coef"]])) {
        
        dt.coef <- as.data.table(matrix(rep(1, 16), 4))
        setnames(dt.coef, paste0("molecule", 1:4))
        
        dt.coef <- as.data.table(dt.coef)
        dt.coef <- cbind(dt.coef, name = paste0("product", 1:4))
        
      } else {
        
        dt.coef <- values[["ab.dt.coef"]]
        
      }
      
    }
    
    dt.coef <- as.data.table(dt.coef)
    
    setnames(dt.coef, c(ab.part.names.data()[1:(ncol(dt.coef) - 1)], "name"))
    
    values[["ab.dt.coef"]] <- dt.coef
    
    dt.coef
    
  })
  
  ab.dt.conc.data <- reactive({
    
    if (!is.null(input$ab.dt.conc)) {
      
      dt.conc <- hot_to_r(input$ab.dt.conc)
      
    } else {
      
      if (is.null(values[["ab.dt.conc"]])) {
        
        dt.conc <- as.data.table(matrix(rep(1e-03, 20), ncol = 4))
        setnames(dt.conc, paste0("molecule", 1:4))
        
      } else {
        
        dt.conc <- values[["ab.dt.conc"]]
        
      }
      
    }
    
    dt.conc <- as.data.table(dt.conc)
    setnames(dt.conc, ab.part.names.data()[1:ncol(dt.conc)])
    
    values[["ab.dt.conc"]] <- dt.conc
    
    dt.conc
    
  })
  
  ab.part.eq.data <- reactive({
    
    if (!is.null(input$ab.part.eq)) {
      
      part.eq <- hot_to_r(input$ab.part.eq)
      
    } else {
      
      if (is.null(values[["ab.part.eq"]])) {
        
        part.eq <- as.data.table(matrix(rep("tot", 4), ncol = 4))
        setnames(part.eq, paste0("molecule", 1:4))
        
      } else {
        
        part.eq <- values[["ab.part.eq"]]
        
      }
      
    }
    
    part.eq <- as.data.table(part.eq)
    
    values[["ab.part.eq"]] <- part.eq
    
    part.eq
    
  })
  
  ab.cnst.data <- reactive({
    
    if (!is.null(input$ab.cnst)) {
      
      cnst <- hot_to_r(input$ab.cnst)
      
    } else {
      
      if (is.null(values[["ab.cnst"]])) {
        
        cnst <- as.data.table(matrix(rep(1, 4), ncol = 1))
        setnames(cnst, "log10(K)")
        
      } else {
        
        cnst <- values[["ab.cnst"]]
        
      }
      
    }
    
    cnst <- as.data.table(cnst)
    
    values[["ab.cnst"]] <- cnst
    
    cnst
    
  })
  
  dt.ab.data <- reactive({
    
    if (!is.null(input$dt.ab)) {
      
      dt.ab <- hot_to_r(input$dt.ab)
      
    } else {
      
      if (is.null(values[["dt.ab"]])) {
        
        dt.ab <- matrix(rep(100, 30), 5)
        dt.ab[, which((1:ncol(dt.ab) %% 2 == 0))] <- .001
        
        dt.ab <- as.data.table(dt.ab)
        
        setnames(dt.ab, colnames(dt.ab)[which(1:ncol(dt.ab) %% 2 == 1)], paste("W", 1:(ncol(dt.ab) / 2), sep = "_"))
        setnames(dt.ab, colnames(dt.ab)[which(1:ncol(dt.ab) %% 2 == 0)], paste("W", 1:(ncol(dt.ab) / 2), "dev", sep = "_"))
        
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
      
    } else {
      
      if (is.null(values[["dt.mol"]])) {
        
        dt.mol <- as.data.table(matrix(rep(0, 6), 2))
        dt.mol <- as.data.table(dt.mol)

        setnames(dt.mol, c("Particle", paste0("W_", (2:ncol(dt.mol)) - 1)))
        
        dt.mol <- cbind(particle = c("molecule1", "molecule2"), dt.mol)

      } else {
        
        dt.mol <- values[["dt.mol"]]
        
      }
      
    }
    
    dt.mol <- as.data.table(dt.mol)
    
    values[["dt.mol"]] <- dt.mol
    
    dt.mol
    
  })
  
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
  
  cnst.tune.load <- reactive({
    
    in.file.bulk <- input$file.bulk.input
    in.file.xlsx <- NULL
    in.file <- NULL
    
    # bulk input
    
    if (nrow(as.data.table(input$file.bulk.input)[name %like% "^constants_names(\\.csv|\\.txt)*"]) > 0){

      in.file <- as.data.table(input$file.bulk.input)[name %like% "^constants_names(\\.csv|\\.txt)*"][1]
      in.file <- as.data.frame(in.file)
      
    }
    
    in.file.xlsx <- as.data.table(input$file.bulk.input)[name %like% "\\.xlsx$"]
    
    if (nrow(in.file.xlsx) > 0) {
      
      in.file.xlsx <- as.data.frame(in.file.xlsx[1])
      
    } else {
      
      in.file.xlsx <- NULL
      
    }
    
    if (!is.null(in.file.xlsx))
      in.file <- NULL
      
    if (!is.null(in.file)) {
      
      if (sep() == ";") {
        cnst.tune <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", header = FALSE), silent = TRUE)
      } else if (sep() == ",") {
        cnst.tune <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", header = FALSE), silent = TRUE)
      } else if (sep() == "tab") {
        cnst.tune <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", header = FALSE), silent = TRUE)
      }
      
    } else if (!is.null(in.file.xlsx)) {
      
      cnst.tune <- try(read.xlsx(in.file.xlsx$datapath, sheet = "constant_names", colNames = FALSE), silent = TRUE)
      
    } else {
      
      cnst.tune <- values[["cnst.tune"]]
      
    }
    
    cnst.tune <- unlist(cnst.tune)
    
    values[["cnst.tune"]] <- cnst.tune
    updateTextInput(session, "cnst.tune", value = paste(cnst.tune, collapse = ", "))
    
  })
  
  
  # execute
  
  ab.eval.data <- reactive({

    particles <- c(colnames(ab.dt.coef.data()), ab.dt.coef.data()[, name])
    
    validate(
      
      need(length(particles %in% cnst.tune.data()) > 0, "Input correct particle names for constants evaluation")
      
    )
    
    # check if no molar extinction coefficients are known
    
    dt.mol <- dt.mol.data()
    
    if (ncol(dt.mol) <= 1)
      dt.mol <- "no.data"
    
    # run
    
    ab.evaluation.runner(mode = "app"
                         , sep = sep()
                         , eq.thr.type = "rel"
                         , eq.threshold = 1e-08
                         , cnst.tune = cnst.tune.data()
                         , algorithm = "direct search"
                         , ab.mode = "base"
                         , method = "basic wls"
                         , search.density = as.numeric(input$search.density)
                         , lrate.init = .5
                         , ab.threshold = as.numeric(input$ab.threshold)
                         , dt.list = list(dt.coef = ab.dt.coef.data()
                                          , cnst = ab.cnst.data()
                                          , dt.conc = ab.dt.conc.data()
                                          , part.eq = ab.part.eq.data()
                                          , dt.ab = dt.ab.data()
                                          , dt.mol = dt.mol)
                         , save.res = FALSE)
    
  })
  
  
  # output data
  
  ab.dt.res.data <- eventReactive(input$ab.conc.exec.btn, {
    
    ab.eval.data()$dt.eq.conc
    
  })
  
  dt.ab.abs.data <- eventReactive(input$ab.conc.exec.btn, {
    
    dt <- ab.eval.data()$dt.ab.calc
    dt.err <- as.data.table(ab.eval.data()$ab.res.abs)
    
    dt.comb <- data.table(rn = NA)
    
    for (i in 1:ncol(dt)) {
      
      dt.comb <- data.table(dt.comb, dt[, i, with = FALSE], dt.err[, i, with = FALSE])
      
    }
    
    dt.comb[, rn := NULL]
    
    setnames(dt.comb, as.character(1:ncol(dt.comb)))
    
    setnames(dt.comb, colnames(dt.comb)[which(1:ncol(dt.comb) %% 2 == 1)], paste("W", 1:(ncol(dt.comb) / 2), sep = "_"))
    setnames(dt.comb, colnames(dt.comb)[which(1:ncol(dt.comb) %% 2 == 0)], paste("W", 1:(ncol(dt.comb) / 2), "err", sep = "_"))
    
    dt.comb
    
  })
  
  dt.ab.rel.data <- eventReactive(input$ab.conc.exec.btn, {
    
    dt <- ab.eval.data()$dt.ab.calc
    dt.err <- as.data.table(ab.eval.data()$ab.res.rel)
    
    dt.comb <- data.table(rn = NA)
    
    for (i in 1:ncol(dt)) {
      
      dt.comb <- data.table(dt.comb, dt[, i, with = FALSE], dt.err[, i, with = FALSE])
      
    }
    
    dt.comb[, rn := NULL]
    
    setnames(dt.comb, as.character(1:ncol(dt.comb)))
    
    setnames(dt.comb, colnames(dt.comb)[which(1:ncol(dt.comb) %% 2 == 1)], paste("W", 1:(ncol(dt.comb) / 2), sep = "_"))
    setnames(dt.comb, colnames(dt.comb)[which(1:ncol(dt.comb) %% 2 == 0)], paste("W", 1:(ncol(dt.comb) / 2), "err", sep = "_"))
    
    dt.comb
    
  })

  cnst.dev.data <- eventReactive(input$ab.conc.exec.btn, {
    
    cnst.dev <- ab.eval.data()$cnst.dev
    cnst.dev <- as.data.table(cnst.dev)
    
    setnames(cnst.dev, c("Particle", "Constant", "St.Deviation"))
    
  })

  cor.m.data <- eventReactive(input$ab.conc.exec.btn, {
    
    cor.m <- ab.eval.data()$cor.m
    
  })
  
  mol.coef.data <- eventReactive(input$ab.conc.exec.btn, {
    
    dt <- as.data.table(t(ab.eval.data()$mol.coef))
    dt.err <- as.data.table(t(ab.eval.data()$mol.coef.dev))
    
    setnames(dt, paste("W", 1:ncol(dt), sep = "_"))
    setnames(dt.err, paste("W", 1:ncol(dt.err), "dev", sep = "_"))
    
    dt.comb <- data.table(rn = NA)
    
    for (i in 1:ncol(dt)) {
      
      dt.comb <- data.table(dt.comb, dt[, i, with = FALSE], dt.err[, i, with = FALSE])

    }
    
    dt.comb[, rn := NULL]
    dt.comb <- data.table(Particle = cnst.dev.data()[, Particle], dt.comb)
    
    dt.comb
    
  })

  err.diff.data <- eventReactive(input$ab.conc.exec.btn, {
    
    err.diff <- ab.eval.data()$err.diff
    err.diff <- data.table(Particle = cnst.tune.data(), Fmin.Last = err.diff)
    
    err.diff
    
  })
  
  
  # rendering ---------------- #
  
  output$ab.dt.coef <- renderRHandsontable({
    
    in.file <- input$file.ab.dt.coef
    in.file.bulk <- input$file.bulk.input
    in.file.xlsx <- NULL
    
    # bulk input
    
    if (input.source$ab.dt.coef.bulk) {
      
      cnst.tune.load()
      
      in.file <- as.data.table(input$file.bulk.input)[name %like% "^(input\\_)*stoich(iometric)*\\_coefficients(\\.csv|\\.txt)*"][1]
      in.file <- as.data.frame(in.file)
      
      in.file.xlsx <- as.data.table(input$file.bulk.input)[name %like% "\\.xlsx$"]
      
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
      updateTextInput(session, "ab.part.names", value = paste(tmp[1:(length(tmp) - 1)], collapse = ", "))
      
      
    } else if (!is.null(in.file.xlsx)) {
      
      dt.coef <- try(read.xlsx(in.file.xlsx$datapath, sheet = "stoich_coefficients"), silent = TRUE)
      
      validate(
        
        need(is.data.frame(dt.coef), "Your file doesn't look like a stoich. coefficients file") %then%
          need(dt.coef[1, 1][!(dt.coef[1, 1] %like% "[a-zA-Z]")], "Your file doesn't look like a stoich. coefficients file")
        
      )
      
      tmp <- colnames(dt.coef)
      updateTextInput(session, "ab.part.names", value = paste(tmp[1:(length(tmp) - 1)], collapse = ", "))
      
    } else {
      
      dt.coef <- ab.dt.coef.data()
      
    }
    
    setnames(dt.coef, c(ab.part.names.data()[1:(ncol(dt.coef) - 1)], "name"))
    
    if (!is.null(dt.coef))
      rhandsontable(dt.coef, stretchH = "all", useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    
  })
  
  output$ab.dt.conc <- renderRHandsontable({
    
    in.file <- input$file.ab.dt.conc
    in.file.bulk <- input$file.bulk.input
    in.file.xlsx <- NULL
    
    # bulk input
    
    if (input.source$ab.dt.conc.bulk) {
      
      in.file <- as.data.table(input$file.bulk.input)[name %like% "^(input\\_)*concentrations(\\.csv|\\.txt)*"][1]
      in.file <- as.data.frame(in.file)
      
      in.file.xlsx <- as.data.table(input$file.bulk.input)[name %like% "\\.xlsx$"]
      
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
      
      if (sep() == ";") {
        dt.conc <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1), silent = TRUE)
      } else if (sep() == ",") {
        dt.conc <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1), silent = TRUE)
      } else if (sep() == "tab") {
        dt.conc <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1), silent = TRUE)
      }
      
      validate(need(is.data.frame(dt.conc), "Check the column delimiter or content of your file"))
      
      tmp <- colnames(dt.conc)
      updateTextInput(session, "ab.part.names", value = paste(tmp, collapse = ", "))
      
      
    } else if (!is.null(in.file.xlsx)) {
      
      dt.conc <- try(read.xlsx(in.file.xlsx$datapath, sheet = "concentrations", startRow = 2), silent = TRUE)
      
      validate(need(is.data.frame(dt.conc), "Check the column delimiter or content of your file"))
      
      tmp <- colnames(dt.conc)
      updateTextInput(session, "ab.part.names", value = paste(tmp, collapse = ", "))
      
    } else {
      
      dt.conc <- ab.dt.conc.data()
      
    }
    
    setnames(dt.conc, ab.part.names.data()[1:ncol(dt.conc)])
    
    if (!is.null(dt.conc))
      rhandsontable(dt.conc, stretchH = "all", useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    
  })
  
  output$ab.part.eq <- renderRHandsontable({
    
    in.file <- input$file.ab.dt.conc
    in.file.bulk <- input$file.bulk.input
    in.file.xlsx <- NULL
    
    # bulk input
    
    if (input.source$ab.dt.conc.bulk) {
      
      in.file <- as.data.table(input$file.bulk.input)[name %like% "^(input\\_)*concentrations(\\.csv|\\.txt)*"][1]
      in.file <- as.data.frame(in.file)
      
      in.file.xlsx <- as.data.table(input$file.bulk.input)[name %like% "\\.xlsx$"]
      
      if (nrow(in.file.xlsx) > 0) {
        
        in.file.xlsx <- as.data.frame(in.file.xlsx[1])
        
      } else {
        
        in.file.xlsx <- NULL
        
      }
      
      if (!is.null(in.file.xlsx))
        in.file <- NULL
      
    }
    
    # choose source
    
    part.eq <- ab.part.eq.data()
    
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
      
    } else if (!is.null(in.file.xlsx)) {
      
      part.eq <- try(read.xlsx(in.file.xlsx$datapath, sheet = "concentrations", colNames = FALSE, rows = 1), silent = TRUE)
      tmp <- try(read.xlsx(in.file.xlsx$datapath, sheet = "concentrations", colNames = FALSE, rows = 2), silent = TRUE)
      
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
  
  output$ab.cnst <- renderRHandsontable({
    
    in.file <- input$file.ab.cnst
    in.file.bulk <- input$file.bulk.input
    in.file.xlsx <- NULL
    
    # bulk input
    
    if (input.source$ab.cnst.bulk) {
      
      in.file <- as.data.table(input$file.bulk.input)[name %like% "^(input\\_)*k\\_constants\\_log10(\\.csv|\\.txt)*"][1]
      in.file <- as.data.frame(in.file)
      
      in.file.xlsx <- as.data.table(input$file.bulk.input)[name %like% "\\.xlsx$"]
      
      if (nrow(in.file.xlsx) > 0) {
        
        in.file.xlsx <- as.data.frame(in.file.xlsx[1])
        
      } else {
        
        in.file.xlsx <- NULL
        
      }
      
      if (!is.null(in.file.xlsx))
        in.file <- NULL
      
    }
    
    # choose source
    
    cnst <- ab.cnst.data()
    
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
      
    } else if (!is.null(in.file.xlsx)) {
      
      cnst <- try(read.xlsx(in.file.xlsx$datapath, sheet = "k_constants_log10"), silent = TRUE)
      
      validate(
        need(is.data.frame(cnst), "Check the column delimiter or content of your file") %then%
          need(ncol(cnst) == 1, "Check the column delimiter or content of your file")
      )
      
    }
    
    if (!is.null(cnst))
      rhandsontable(cnst, stretchH = "all", useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    
  })
  
  output$dt.ab <- renderRHandsontable({
    
    in.file <- input$file.dt.ab
    in.file.bulk <- input$file.bulk.input
    in.file.xlsx <- NULL
    
    # bulk input
    
    if (input.source$dt.ab.bulk) {
      
      in.file <- as.data.table(input$file.bulk.input)[name %like% "^(input\\_)*absorbance(\\.csv|\\.txt)*"][1]
      in.file <- as.data.frame(in.file)
      
      in.file.xlsx <- as.data.table(input$file.bulk.input)[name %like% "\\.xlsx$"]
      
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
      
      if (sep() == ";") {
        dt.ab <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", header = FALSE), silent = TRUE)
      } else if (sep() == ",") {
        dt.ab <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", header = FALSE), silent = TRUE)
      } else if (sep() == "tab") {
        dt.ab <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", header = FALSE), silent = TRUE)
      }
      
      validate(
        
        need(is.data.frame(dt.ab), "Your file doesn't look like an absorbance file")
        
      )
      

    } else if (!is.null(in.file.xlsx)) {
      
      dt.ab <- try(read.xlsx(in.file.xlsx$datapath, sheet = "absorbance", colNames = FALSE), silent = TRUE)
      
      validate(
        
        need(is.data.frame(dt.ab), "Your file doesn't look like an absorbance file")
        
      )
      
    } else {
      
      dt.ab <- dt.ab.data()
      
    }
    
    setnames(dt.ab, colnames(dt.ab)[which(1:ncol(dt.ab) %% 2 == 1)], paste("W", 1:(ncol(dt.ab) / 2), sep = "_"))
    setnames(dt.ab, colnames(dt.ab)[which(1:ncol(dt.ab) %% 2 == 0)], paste("W", 1:(ncol(dt.ab) / 2), "dev", sep = "_"))
    
    if (!is.null(dt.ab))
      rhandsontable(dt.ab, stretchH = "all", useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    
  })
  
  output$dt.mol <- renderRHandsontable({
    
    in.file <- input$file.dt.mol
    in.file.bulk <- input$file.bulk.input
    in.file.xlsx <- NULL
    
    # bulk input
    
    if (input.source$dt.mol.bulk) {
      
      in.file <- as.data.table(input$file.bulk.input)[name %like% "^(input\\_)*mol(ar)*\\_ext(inction)*\\_coefficients(\\.csv|\\.txt)*"][1]
      in.file <- as.data.frame(in.file)
      
      in.file.xlsx <- as.data.table(input$file.bulk.input)[name %like% "\\.xlsx$"]
      
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
      
      if (sep() == ";") {
        dt.mol <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", header = FALSE), silent = TRUE)
      } else if (sep() == ",") {
        dt.mol <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", header = FALSE), silent = TRUE)
      } else if (sep() == "tab") {
        dt.mol <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", header = FALSE), silent = TRUE)
      }
      
      # browser()
      
      # validate(
      #   
      #   need(is.data.frame(dt.mol), "Your file doesn't look like a molar extinction coefficients file")
      #   
      # )
      
      
    } else if (!is.null(in.file.xlsx)) {
      
      dt.mol <- try(read.xlsx(in.file.xlsx$datapath, sheet = "mol_ext_coefficients", colNames = FALSE), silent = TRUE)
      
      # validate(
      #   
      #   need(is.data.frame(dt.mol), "Your file doesn't look like a molar extinction coefficients file")
      #   
      # )
      
    } else {
      
      dt.mol <- dt.mol.data()
      
    }
    
    if (!is.data.frame(dt.mol))
      dt.mol <- data.frame(no.data = "no.data")
    
    setnames(dt.mol, paste0("W_", 1:ncol(dt.mol)))
    
    if (!is.null(dt.mol))
      rhandsontable(dt.mol, stretchH = "all", useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    
  })
  
  output$ab.dt.res <- renderRHandsontable({
    
    dt.res <- ab.dt.res.data()
    
    if (!is.null(dt.res))
      
      rhandsontable(dt.res, stretchH = FALSE, useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    
  })
  
  output$dt.ab.abs <- renderRHandsontable({
    
    dt.ab.abs <- dt.ab.abs.data()
    
    if (!is.null(dt.ab.abs))
      
      rhandsontable(dt.ab.abs, stretchH = FALSE, useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    
  })
  
  output$dt.ab.rel <- renderRHandsontable({
    
    dt.ab.rel <- dt.ab.rel.data()
    
    if (!is.null(dt.ab.rel))
      
      rhandsontable(dt.ab.rel, stretchH = FALSE, useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    
  })
  
  output$cnst.dev <- renderRHandsontable({
    
    cnst.dev <- cnst.dev.data()
    
    if (!is.null(cnst.dev))
      
      rhandsontable(cnst.dev, stretchH = FALSE, useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    
  })

  output$cor.m <- renderRHandsontable({
    
    cor.m <- cor.m.data()
    
    if (!is.null(cor.m))
      
      rhandsontable(cor.m, stretchH = FALSE, useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    
  })

  output$mol.coef <- renderRHandsontable({
    
    mol.coef <- mol.coef.data()
    
    if (!is.null(mol.coef))
      
      rhandsontable(mol.coef, stretchH = FALSE, useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    
  })

  output$err.diff <- renderRHandsontable({
    
    err.diff <- err.diff.data()
    
    if (!is.null(err.diff))
      
      rhandsontable(err.diff, stretchH = FALSE, useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    
  })
  
  
  # end of main server part ----------------------------
  
  

  # equilibrium downoad ---------------- #
  
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
      
      paste0(input$bs.name, "_fractions.csv")
      
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
      
      paste0(input$bs.name, "_fractions.xlsx")
      
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
  

  # absorbance downoad ---------------- #
  
  output$ab.dt.coef.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "input_stoichiometric_coefficients.csv"
      
    },
    
    content = function(file) {
      
      if (sep() == ";") {
        write.csv2(ab.dt.coef.data(), file, row.names = FALSE)
      } else {
        write.csv(ab.dt.coef.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$ab.dt.coef.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "input_stoichiometric_coefficients.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(ab.dt.coef.data(), file)
      
    }
    
  )
  # ----
  
  output$ab.cnst.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "input_k_constants_log10.csv"
      
    },
    
    content = function(file) {
      
      if (sep() == ";") {
        write.csv2(ab.cnst.data(), file, row.names = FALSE)
      } else {
        write.csv(ab.cnst.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$ab.cnst.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "input_k_constants_log10.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(ab.cnst.data(), file)
      
    }
    
  )
  # ----
  
  output$ab.dt.conc.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "input_concentrations.csv"
      
    },
    
    content = function(file) {
      
      tmp <- ab.dt.conc.data()
      tmp <- rbind(data.table(t(data.table(colnames(tmp)))), tmp, use.names = FALSE)
      
      setnames(tmp, unlist(ab.part.eq.data()))
      
      if (sep() == ";") {
        write.csv2(tmp, file, row.names = FALSE)
      } else {
        write.csv(tmp, file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$ab.dt.conc.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "input_concentrations.xlsx"
      
    },
    
    content = function(file) {
      
      tmp <- ab.dt.conc.data()
      tmp <- rbind(data.table(t(data.table(colnames(tmp)))), tmp, use.names = FALSE)
      
      setnames(tmp, unlist(ab.part.eq.data()))
      
      write.xlsx(tmp, file)
      
    }
    
  )
  # ----
  
  output$dt.ab.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "input_absorbance.csv"
      
    },
    
    content = function(file) {
      
      if (sep() == ";") {
        write.csv2(dt.ab.data(), file, row.names = FALSE)
      } else {
        write.csv(dt.ab.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$dt.ab.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "input_absorbance.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(dt.ab.data(), file)
      
    }
    
  )
  # ----
  
  output$dt.mol.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "input_mol_ext_coefficients.csv"
      
    },
    
    content = function(file) {
      
      if (sep() == ";") {
        write.csv2(dt.mol.data(), file, row.names = FALSE)
      } else {
        write.csv(dt.mol.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$dt.mol.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "input_mol_ext_coefficients.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(dt.mol.data(), file)
      
    }
    
  )
  # ----

  output$ab.dt.res.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "equilibrium_concentrations.csv"
      
    },
    
    content = function(file) {
      
      if (sep() == ";") {
        write.csv2(ab.dt.res.data(), file, row.names = FALSE)
      } else {
        write.csv(ab.dt.res.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$ab.dt.res.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "equilibrium_concentrations.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(ab.dt.res.data(), file)
      
    }
    
  )
  # ----
  
  output$dt.ab.abs.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "absorbance_calculated_abs_errors.csv"
      
    },
    
    content = function(file) {
      
      if (sep() == ";") {
        write.csv2(dt.ab.abs.data(), file, row.names = FALSE)
      } else {
        write.csv(dt.ab.abs.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$dt.ab.abs.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "absorbance_calculated_abs_errors.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(dt.ab.abs.data(), file)
      
    }
    
  )
  # ----
  
  output$dt.ab.rel.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "absorbance_calculated_rel_errors.csv"
      
    },
    
    content = function(file) {
      
      if (sep() == ";") {
        write.csv2(dt.ab.rel.data(), file, row.names = FALSE)
      } else {
        write.csv(dt.ab.rel.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$dt.ab.rel.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "absorbance_calculated_rel_errors.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(dt.ab.rel.data(), file)
      
    }
    
  )
  # ----
  
  output$cnst.dev.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "constants_evaluated.csv"
      
    },
    
    content = function(file) {
      
      if (sep() == ";") {
        write.csv2(cnst.dev.data(), file, row.names = FALSE)
      } else {
        write.csv(cnst.dev.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$cnst.dev.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "constants_evaluated.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(cnst.dev.data(), file)
      
    }
    
  )
  # ----
  
  output$cor.m.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "correlation_matrix.csv"
      
    },
    
    content = function(file) {
      
      if (sep() == ";") {
        write.csv2(cor.m.data(), file, row.names = FALSE)
      } else {
        write.csv(cor.m.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$cor.m.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "correlation_matrix.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(cor.m.data(), file)
      
    }
    
  )
  # ----
  
  output$err.diff.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "fmin_last_step.csv"
      
    },
    
    content = function(file) {
      
      if (sep() == ";") {
        write.csv2(err.diff.data(), file, row.names = FALSE)
      } else {
        write.csv(err.diff.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$err.diff.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "fmin_last_step.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(err.diff.data(), file)
      
    }
    
  )
  # ----
  
  output$mol.coef.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "mol_ext_coefficients_calculated.csv"
      
    },
    
    content = function(file) {
      
      if (sep() == ";") {
        write.csv2(mol.coef.data(), file, row.names = FALSE)
      } else {
        write.csv(mol.coef.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$mol.coef.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "mol_ext_coefficients_calculated.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(mol.coef.data(), file)
      
    }
    
  )
  # ----
  
  output$kev.data.zip <- downloadHandler(
    # ----
    filename = function() {
      
      "kev.constants.data.zip"
      
    },
    
    content = function(file) {
      
      data.files <- c(
        
        ab.dt.coef = "input_stoichiometric_coefficients.csv"
        , ab.cnst = "input_k_constants_log10.csv"
        , ab.dt.conc = "input_concentrations.csv"
        , dt.ab = "input_absorbance.csv"
        , dt.mol = "input_mol_ext_coefficients.csv"
        , ab.dt.res = "equilibrium_concentrations.csv"
        , dt.ab.abs = "absorbance_calculated_abs_errors.csv"
        , dt.ab.rel = "absorbance_calculated_rel_errors.csv"
        , cnst.dev = "constants_evaluated.csv"
        , cor.m = "correlation_matrix.csv"
        , err.diff = "fmin_last_step.csv"
        , mol.coef = "mol_ext_coefficients_calculated.csv"
        , cnst.tune = "constants_names.csv"
        
      )
      
      for (i in length(data.files):1) {
        
        # check if all files are present (in case run before evaluation)
        
        dt <- NULL
        try(dt <- eval(expr = parse(text = paste0(names(data.files)[i], ".data()"))), silent = TRUE)
        
        if (sep() == ";") {

          if (!is.null(dt)) {
            
            write.csv2(dt, data.files[i], row.names = FALSE)
            
          } else {
            
            data.files <- data.files[-i]
            
          }
          
        } else {
          
          if (!is.null(dt)) {
            
            write.csv(dt, data.files[i], row.names = FALSE)
            
          } else {
            
            data.files <- data.files[-i]
            
          }
        }
        
      }
      
      utils::zip(file, data.files)

    }
    
  )
  # ----
  
  output$kev.data.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "kev.constants.data.xlsx"
      
    },
    
    content = function(file) {
      
      data.files <- c(
        
        ab.dt.coef = "input_stoich_coefficients"
        , ab.cnst = "input_k_constants_log10"
        , ab.dt.conc = "input_concentrations"
        , dt.ab = "input_absorbance"
        , dt.mol = "input_mol_ext_coefficients"
        , cnst.tune = "constant_names"
        , ab.dt.res = "equilibrium_concentrations"
        , dt.ab.abs = "absorbance_calc_abs_errors"
        , dt.ab.rel = "absorbance_calc_rel_errors"
        , cnst.dev = "constants_evaluated"
        , cor.m = "correlation_matrix"
        , err.diff = "fmin_last_step"
        , mol.coef = "mol_ext_coefficients_calc"
        
      )
      
      dt.list <- list()
      
      for (i in 1:length(data.files)) {
        
        # check if all files are present (in case run before evaluation)
        
        dt <- NULL
        try(dt <- eval(expr = parse(text = paste0(names(data.files)[i], ".data()"))), silent = TRUE)
        
        if (!is.null(dt)) {
          
          dt.list[[eval(data.files[i])]] <- dt
          
        }
        
      }
      
      write.xlsx(dt.list, file)
      
    }
    
  )
  # ----
  
  
}

# run

shinyApp(ui = ui, server = server)




