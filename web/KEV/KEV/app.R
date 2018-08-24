# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2018                                                 #
#                                                            #
# ########################################################## #



# ---------------------- load libraries ----------------------

# I/O
library(readr)
library(openxlsx)
# data structure
library(data.table)
# computation
library(MASS)
library(Matrix)
library(Hmisc)
# strings
library(stringi)
library(stringr)
# reporting
library(shiny)
library(rhandsontable)



# ------------------------- settings ------------------------

if (Sys.info()['sysname'] %like% "indows")
  Sys.setenv("R_ZIPCMD" = "c:/Rtools/bin/zip.exe")

source("eq_concentrations.web.r", chdir = TRUE)



# frontend ------------------------------------------------- #

ui <- navbarPage("KEV",
                 tabPanel("Equilibrium concentrations"
                          , id = "page.eq.conc"
                          
                          , fluidPage(
                            
                            includeCSS("styles.css")
                            
                            , titlePanel("KEV: Chemistry Constant evaluator")
                            
                            , fluidRow(column(12), p(HTML("<br/>")))
                            
                            , titlePanel("Part 1: Equilibrium concentrations")
                            
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
                                                      , downloadButton("dt.coef.xlsx", "xlsx")))
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
                                                        , downloadButton("cnst.xlsx", "xlsx")))
                                    , column(5
                                             , h4("Concentrations")
                                             , rHandsontableOutput("part.eq")
                                             , rHandsontableOutput("dt.conc")
                                             , fileInput("file.dt.conc", "Choose CSV File",
                                                         accept = c(
                                                           "text/csv",
                                                           "text/comma-separated-values,text/plain",
                                                           ".csv")
                                             )
                                             , fluidRow(class = "download-row"
                                                        , downloadButton("dt.conc.csv", "csv")
                                                        , downloadButton("dt.conc.xlsx", "xlsx")))
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
                                                  , h4("Fractions per molecule")
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
                 tabPanel("To do"),
                 tabPanel("To do"),
                 tabPanel("About")
)


# backend -------------------------------------------------- #

server <- function(input, output) {
  
  # technical
   
  values <- reactiveValues()
  
  sep <- reactive({
    
    switch(input$sep,
           comma = ",",
           semicolon = ";",
           tab = "\\t")
    
  })
  
  
  # data --------------------- #
  
  # input data
  
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
  
  # raw output data
  
  eval.data <- reactive({
    
    eq.conc.exec(sep(), dt.coef.data(), cnst.data(), dt.conc.data(), part.eq.data(), bs.name(), "rel", threshold = 1e-08)

  })
  
  # output data
  
  dt.res.data <- eventReactive(input$eq.conc.exec.btn, {
    
    eval.data()$dt.res
    
  })
  
  dt.frac.data <- eventReactive(input$eq.conc.exec.btn, {
    
    eval.data()$dt.frac
    
  })

  dt.err.data <- eventReactive(input$eq.conc.exec.btn, {
    
    eval.data()$dt.res
    
  })

    
  
  # rendering ---------------- #
  
  output$dt.coef <- renderRHandsontable({
    
    in.file <- input$file.dt.coef
    
    dt.coef <- dt.coef.data()
    
    if (!is.null(in.file)) {
      
      if (sep() == ";") {
        dt.coef <- read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character")
      } else if (sep() == ",") {
        dt.coef <- read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character")
      }
      
    }
      
    if (!is.null(dt.coef))
      rhandsontable(dt.coef, stretchH = "all", useTypes = FALSE) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    
  })
  
  output$dt.conc <- renderRHandsontable({
    
    in.file <- input$file.dt.conc
    
    dt.conc <- dt.conc.data()
    
    if (!is.null(in.file)) {
      
      if (sep() == ";") {
        dt.conc <- read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1)
      } else if (sep() == ",") {
        dt.conc <- read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1)
      }
      
    }
    
    if (!is.null(dt.conc))
      rhandsontable(dt.conc, stretchH = "all", useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    
  })

  output$part.eq <- renderRHandsontable({
    
    in.file <- input$file.dt.conc
    
    part.eq <- part.eq.data()
    
    if (!is.null(in.file)) {
      
      
      if (sep() == ";") {
        
        part.eq <- read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", nrows = 1, header = FALSE)
        tmp <- read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1, header = FALSE)[1, ]
        # browser()
        
      } else if (sep() == ",") {
        
        part.eq <- read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", nrows = 1, header = FALSE)
        tmp <- read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1, header = FALSE)[1, ]
        
      }

      colnames(part.eq) <- tmp

    }
    
    if (!is.null(part.eq))
      rhandsontable(part.eq, stretchH = "all", useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    
  })
  
  output$cnst <- renderRHandsontable({
    
    in.file <- input$file.cnst
    
    cnst <- cnst.data()
    
    if (!is.null(in.file)) {
      
      if (sep() == ";") {
        cnst <- read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character")
      } else if (sep() == ",") {
        cnst <- read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character")
      }
      
    }
    
    if (!is.null(cnst))
      rhandsontable(cnst, stretchH = "all", useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    
  })

  output$dt.res <- renderRHandsontable({
    
    dt.res <- dt.res.data()
    
    if (!is.null(dt.res))
      
      # dt.res <- as.data.table(t(dt.res), keep.rownames = TRUE)
      # cln <- colnames(dt.res)
      # setnames(dt.res, cln, str_replace(cln, "^V", "S_"))
      
      rhandsontable(dt.res, stretchH = "all", useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    
  })
  
  output$dt.frac <- renderRHandsontable({
    
    dt.frac <- dt.frac.data()
    
    if (!is.null(dt.frac)) {
      
      dt.frac <- as.data.table(t(dt.frac), keep.rownames = TRUE)
      setnames(dt.frac, unlist(dt.frac[1]))
      
      dt.frac <- dt.frac[!1]
      # cln <- colnames(dt.res)
      # setnames(dt.res, cln, str_replace(cln, "^V", "S_"))
      
      rhandsontable(dt.frac, stretchH = "all", useTypes = FALSE) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
      
    }
    
  })

  output$dt.err <- renderRHandsontable({
    
    dt.err <- dt.err.data()
    
    if (!is.null(dt.err))
      rhandsontable(dt.err, stretchH = "all", useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    
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
      } else if (sep() == ",") {
        write.csv(dt.coef.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$cnst.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "log10_K_constants.csv"
      
    },
    
    content = function(file) {
      
      if (sep() == ";") {
        write.csv2(cnst.data(), file, row.names = FALSE)
      } else if (sep() == ",") {
        write.csv(cnst.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$dt.conc.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "input_total_concentrations.csv"
      
    },
    
    content = function(file) {
      
      if (sep() == ";") {
        write.csv2(dt.conc.data(), file, row.names = FALSE)
      } else if (sep() == ",") {
        write.csv(dt.conc.data(), file, row.names = FALSE)
      }
      
    }

  )
  # ----
  
}

# run

shinyApp(ui = ui, server = server)

