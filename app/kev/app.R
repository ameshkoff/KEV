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
library(plotly)



# ------------------------- settings ------------------------

# if google analytics to be shown

google.an <- "google-analytics.html"

# prepare environment

if (Sys.info()["sysname"] %like% "indows") {
  
  Sys.setenv("R_ZIPCMD" = "c:/Rtools/bin/zip.exe")
  google.an <- ""
  
}
  

options(shiny.sanitize.errors = TRUE)
`%then%` <- shiny:::`%OR%`

userguide.date <- "20190331"

cur.curves.list <- list("Add Curve", "Gaussian", "Lorentzian")


# load algorithm

source("eq_runner.r", chdir = TRUE)
source("ab_runner.r", chdir = TRUE)
source("sp_runner.r", chdir = TRUE)
source("emf_runner.r", chdir = TRUE)
source("nm_runner.r", chdir = TRUE)
source("cur_runner.r", chdir = TRUE)



# frontend ------------------------------------------------- #

ui <- tagList(
  
  tags$head(

    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Saira+Extra+Condensed:400,500,700")
    , tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Muli:400,700")
    # , includeCSS("styles.css")

  ),
  
  navbarPage(title = "KEV",
             windowTitle = "KEV: Constant Evaluator",
             theme = "kev.css",

# equilibrium concentrations -------------------------

                 tabPanel("Equilibrium concentrations"
                          , id = "page.eq.conc"
                          
                          , fluidPage(
                            
                            fluidRow(column(12, p(HTML("KEV: Chemistry Constant Evaluator<br/>"))))
                            
                            , titlePanel("Equilibrium Concentrations")
                            
                            , fluidRow(column(
                              12
                              , wellPanel(
                                fluidRow(column(1, img(src = "eq-icon.png", class = "kev-icon"))
                                        , column(3
                                                , h4("Column delimiter of your data files")
                                                , radioButtons("eq.sep", "", inline = TRUE
                                                               , c("," = "comma"
                                                                   , ";" = "semicolon"
                                                                   , "tab" = "tab"))
                                         )
                                         , column(8
                                                  , h4("Component to get fractions of")
                                                  , textInput("bs.name", "", "molecule1")
                                         ))
                              )))
                            
                            , fluidRow(column(
                              12
                              , wellPanel(
                                fluidRow(column(12
                                                , h3("Bulk upload / download (optional)")))
                                
                                , fluidRow(column(5
                                                  , h4("Upload all data")
                                                  , fileInput("file.eq.bulk.input", "Choose CSV files or XLSX file with multiple sheets",
                                                              accept = c(
                                                                "text/csv"
                                                                , "text/comma-separated-values,text/plain"
                                                                , ".csv"
                                                                , ".xlsx")
                                                              , multiple = TRUE
                                                  ))
                                           , column(3
                                                    , h4("Download all data")
                                                    , fluidRow(class = "download-row"
                                                               , downloadButton("kev.eq.data.zip", "zip")
                                                               , downloadButton("kev.eq.data.xlsx", "xlsx")))
                                           , column(4
                                                    , h4("Example data")
                                                    , p("Learn how to prepare data via example datasets")
                                                    , fluidRow(
                                                      column(12
                                                             , actionButton(inputId = "eq.example.data"
                                                                              , label = HTML("&nbsp;Check examples")
                                                                              , icon = icon("database")
                                                                              , onclick = paste0("window.open('https://gitlab.com/"
                                                                                                 , "a.meshkov/KEV/tree/"
                                                                                                 ,"master/input/concentrations', '_blank')")))))
                                           )
                              )))
                            
                            , fluidRow(
                              
                              column(
                                12
                                , wellPanel(
                                  h3("Upload or type input data")
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
                                           , textInput("part.names", "Component names, comma separated", paste(paste0("molecule", 1:4), collapse = ", "))
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
                                             , tabsetPanel(type = "tabs", id = "eq.conc.tab"
                                                           , tabPanel("Input", value = "input"
                                                                      , rHandsontableOutput("dt.conc")
                                                                      , rHandsontableOutput("part.eq")
                                                                      , fluidRow(class = "download-row"
                                                                                 , downloadButton("dt.conc.csv", "csv")
                                                                                 , downloadButton("dt.conc.xlsx", "xlsx")))
                                                           , tabPanel("Total", value = "total"
                                                                      , rHandsontableOutput("dt.conc.tot")
                                                                      , fluidRow(class = "download-row"
                                                                                 , downloadButton("dt.conc.tot.csv", "csv")
                                                                                 , downloadButton("dt.conc.tot.xlsx", "xlsx")))
                                                           , tabPanel("pC range (optional)", value = "pc"
                                                                      , h5("Variable component and its pC range")
                                                                      , textInput("eq.pc.name", "", "molecule1")
                                                                      , sliderInput("eq.pc.range", "", min = 0, max = 15, value = c(0, 3))
                                                                      , h5("Total concentations of other components")
                                                                      , rHandsontableOutput("eq.dt.conc.pc")
                                                                      , fluidRow(class = "download-row"
                                                                                 , actionButton("eq.pc.update.btn", "Update Input Concentrations")))
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
                                                , actionButton("eq.conc.exec.btn", "Evaluate", class = "kev-ev-button")
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
                                                  , tabsetPanel(type = "tabs"
                                                                , tabPanel("Table"
                                                                           , rHandsontableOutput("dt.frac")
                                                                           , fluidRow(class = "download-row"
                                                                                      , downloadButton("dt.frac.csv", "csv")
                                                                                      , downloadButton("dt.frac.xlsx", "xlsx")))
                                                                , tabPanel("Plot"
                                                                           , plotlyOutput("plot.eq.dt.frac"))
                                                  )))
                                
                                , fluidRow(column(12
                                                  , h4("Residuals matrix")
                                                  , rHandsontableOutput("dt.err")
                                                  , fluidRow(class = "download-row"
                                                             , downloadButton("dt.err.csv", "csv")
                                                             , downloadButton("dt.err.xlsx", "xlsx"))))
                              ))
                              
                            )
                            
                            , tags$div(includeHTML(google.an))
                            
                          )),

# absorbance (spectrophotometry) ----------------------------------
  
              navbarMenu("Eqilibrium Constants"
                 , tabPanel(title = "Spectrophotometry"
                          , id = "page.ab"
                          
                          , fluidPage(
                            
                            fluidRow(column(12, p(HTML("KEV: Chemistry Constant Evaluator<br/>"))))
                            
                            , titlePanel(HTML("Equilibrium Constants <i>via Spectrophotometry</i>"))
                            
                            , fluidRow(column(
                              12
                              , wellPanel(
                                fluidRow(column(1, img(src = "abs-icon.png", class = "kev-icon"))
                                         , column(2
                                                , h4("Column delimiter")
                                                , radioButtons("ab.sep", "", inline = TRUE
                                                               , c("," = "comma"
                                                                   , ";" = "semicolon"
                                                                   , "tab" = "tab")))
                                         , column(3
                                                  , HTML("<h4>Constants to evaluate</h4><p>Component names, comma separated</p>")
                                                  , textInput("cnst.tune", "", "molecule1"))
                                         , column(3
                                                  , HTML(paste("<h4>Threshold</h4><p>Search algorithm precision"
                                                                ,"0&nbsp;&#60;&nbsp;&#950;&nbsp;&#60;&nbsp;1</p>"))
                                                  , textInput("ab.threshold", "", "1e-7"))
                                         , column(3
                                                  , HTML("<h4>Search density</h4><p>Do not change unless you fully understand what you are doing</p>")
                                                  , textInput("search.density", "", "1"))
                                )
                              )))
                            
                            , fluidRow(column(
                              12
                              , wellPanel(
                                fluidRow(column(12
                                                , h3("Bulk upload / download (optional)")))
                                
                                , fluidRow(column(5
                                                  , h4("Upload all data")
                                                  , fileInput("file.bulk.input", "Choose CSV files or XLSX file with multiple sheets",
                                                              accept = c(
                                                                "text/csv"
                                                                , "text/comma-separated-values,text/plain"
                                                                , ".csv"
                                                                , ".xlsx")
                                                              , multiple = TRUE
                                                  ))
                                           , column(3
                                                    , h4("Download all data")
                                                    , fluidRow(class = "download-row"
                                                               , downloadButton("kev.data.zip", "zip")
                                                               , downloadButton("kev.data.xlsx", "xlsx")))
                                           , column(4
                                                    , h4("Example data")
                                                    , p("Learn how to prepare data via example datasets")
                                                    , fluidRow(
                                                      column(12
                                                             , actionButton(inputId = "ab.example.data"
                                                                            , label = HTML("&nbsp;Check examples")
                                                                            , icon = icon("database")
                                                                            , onclick = paste0("window.open('https://gitlab.com/"
                                                                                               , "a.meshkov/KEV/tree/"
                                                                                               , "master/input/spectrophotometry', '_blank')")))))
                                )
                              )))
                            
                            , fluidRow(
                              
                              column(
                                12
                                , wellPanel(
                                  h3("Upload or type input data")
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
                                           , textInput("ab.part.names", "Component names, comma separated"
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
                                    column(6
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
                                    , column(6
                                             , h4("Molar extinction coefficients")
                                             , textInput("wl.tune", "Peaks (up to 10)", "110, 120, 130")
                                             , p("")
                                             , rHandsontableOutput("dt.mol")
                                             , fluidRow(
                                               column(8, fileInput("file.dt.mol", "Choose CSV File",
                                                         accept = c(
                                                           "text/csv",
                                                           "text/comma-separated-values,text/plain",
                                                           ".csv")))
                                               , column(4, fluidRow(column(12, HTML("<label>&nbsp;</label>")))
                                                         , fluidRow(column(12, actionButton("dt.mol.memory", "From memory")))
                                                        )
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
                                                , actionButton("ab.conc.exec.btn", "Evaluate", class = "kev-ev-button")
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
                                                                , tabPanel("Plot (Peaks)"
                                                                           , plotlyOutput("plot.dt.ab.cut"))
                                                                , tabPanel("Plot (Full)"
                                                                           , plotlyOutput("plot.dt.ab"))
                                                  )))
                                
                                , fluidRow(column(12)
                                           , column(6
                                                    , h4("Evaluated Constants")
                                                    , rHandsontableOutput("cnst.dev")
                                                    , fluidRow(class = "download-row"
                                                               , downloadButton("cnst.dev.csv", "csv")
                                                               , downloadButton("cnst.dev.xlsx", "xlsx")))
                                           , column(3
                                                    , h4("Correlation Matrix")
                                                    , rHandsontableOutput("cor.m")
                                                    , fluidRow(class = "download-row"
                                                               , downloadButton("cor.m.csv", "csv")
                                                               , downloadButton("cor.m.xlsx", "xlsx")))
                                           , column(3
                                                    , h4(HTML("Adjusted R<sup>2</sup>"))
                                                    , rHandsontableOutput("ab.adj.r.squared")
                                                    , fluidRow(class = "download-row"
                                                               , downloadButton("ab.adj.r.squared.csv", "csv")
                                                               , downloadButton("ab.adj.r.squared.xlsx", "xlsx"))))
                                
                                , fluidRow(column(12
                                                  , h4("Extinction Molar Coefficients with St.Errors")
                                                  , rHandsontableOutput("mol.coef")
                                                  , fluidRow(class = "download-row"
                                                             , downloadButton("mol.coef.csv", "csv")
                                                             , downloadButton("mol.coef.xlsx", "xlsx"))))
                                
                                
                              ))
                            )
                            
                          )
                          
                 ),

# extinction coefficients ----------------------------------

      tabPanel(title = "Extinction Coefficients for Spectrophotometry"
         , id = "page.sp"
         
         , fluidPage(
           
           fluidRow(column(12, p(HTML("KEV: Chemistry Constant Evaluator<br/>"))))
           
           , titlePanel("Calculate Molar Extinction Coefficients")
           
           , fluidRow(column(
             12
             , wellPanel(
               fluidRow(column(1, img(src = "abs-icon.png", class = "kev-icon"))
                        , column(3
                               , h4("Column delimiter")
                               , radioButtons("sp.sep", "", inline = TRUE
                                              , c("," = "comma"
                                                  , ";" = "semicolon"
                                                  , "tab" = "tab")
                                              , selected = "tab"))
               )
             )))
           
           , fluidRow(column(
             12
             , wellPanel(
               fluidRow(column(12
                               , h3("Bulk upload")))
               
               , fluidRow(column(6
                                 , h4("Upload all data")
                                 , fileInput("file.sp.bulk.input", "Choose CSV, TXT files or XLSX file with multiple sheets",
                                             accept = c(
                                               "text/csv"
                                               , "text/comma-separated-values,text/plain"
                                               , ".csv"
                                               , ".xlsx")
                                             , multiple = TRUE
                                 ))
                          , column(6
                                   , h4("Example data")
                                   , p("Learn how to prepare data via example datasets")
                                   , fluidRow(
                                     column(12
                                            , actionButton(inputId = "mol.coef.example.data"
                                                           , label = HTML("&nbsp;Check examples")
                                                           , icon = icon("database")
                                                           , onclick = paste0("window.open('https://gitlab.com/"
                                                                              , "a.meshkov/KEV/tree/"
                                                                              ,"master/input/molar.extinction.coefficients', '_blank')")))))
               )
             )))
           
           , fluidRow(column(
             12
             , wellPanel(
               fluidRow(column(12
                               , h4("Molar extinction coefficients")
                               , rHandsontableOutput("sp.dt.mol.full")
                               , fluidRow(class = "download-row"
                                          , downloadButton("sp.dt.mol.full.csv", "csv")
                                          , downloadButton("sp.dt.mol.full.xlsx", "xlsx"))))
               
             ))
           )
         )
         
    ),
# emf (potentiometry) ----------------------------------

    tabPanel(title = "E.M.F. (Potentiometry)"
         , id = "page.emf"
         
         , fluidPage(
           
           fluidRow(column(12, p(HTML("KEV: Chemistry Constant Evaluator<br/>"))))
           
           , titlePanel(HTML("Equilibrium Constants <i>via E.M.F.</i>"))
           
           , fluidRow(column(
             12
             , wellPanel(
               fluidRow(column(1, img(src = "emf-icon.png", class = "kev-icon"))
                        , column(2
                               , h4("Column delimiter")
                               , radioButtons("emf.sep", "", inline = TRUE
                                              , c("," = "comma"
                                                  , ";" = "semicolon"
                                                  , "tab" = "tab")))
                        , column(3
                                 , HTML("<h4>Constants to evaluate</h4><p>Component names, comma separated</p>")
                                 , textInput("emf.cnst.tune", "", "molecule1"))
                        , column(3
                                 , HTML(paste("<h4>Threshold</h4><p>Search algorithm precision"
                                              ,"0&nbsp;&#60;&nbsp;&#950;&nbsp;&#60;&nbsp;1</p>"))
                                 , textInput("emf.threshold", "", "1e-7"))
                        , column(3
                                 , HTML("<h4>Search density</h4><p>Do not change unless you fully understand what you are doing</p>")
                                 , textInput("emf.search.density", "", "1"))
               )
             )))
           
           , fluidRow(column(
             12
             , wellPanel(
               fluidRow(column(12
                               , h3("Bulk upload / download (optional)")))
               
               , fluidRow(column(5
                                 , h4("Upload all data")
                                 , fileInput("file.emf.bulk.input", "Choose CSV files or XLSX file with multiple sheets",
                                             accept = c(
                                               "text/csv"
                                               , "text/comma-separated-values,text/plain"
                                               , ".csv"
                                               , ".xlsx")
                                             , multiple = TRUE
                                 ))
                          , column(3
                                   , h4("Download all data")
                                   , fluidRow(class = "download-row"
                                              , downloadButton("kev.emf.data.zip", "zip")
                                              , downloadButton("kev.emf.data.xlsx", "xlsx")))
                          , column(4
                                   , h4("Example data")
                                   , p("Learn how to prepare data via example datasets")
                                   , fluidRow(
                                     column(12
                                            , actionButton(inputId = "emf.example.data"
                                                           , label = HTML("&nbsp;Check examples")
                                                           , icon = icon("database")
                                                           , onclick = paste0("window.open('https://gitlab.com/"
                                                                              , "a.meshkov/KEV/tree/"
                                                                              ,"master/input/emf', '_blank')")))))
               )
             )))
           
           , fluidRow(
             
             column(
               12
               , wellPanel(
                 h3("Upload or type input data")
                 ,  fluidRow(
                   column(5
                          , h4("Stoichiometric coefficients")
                          , rHandsontableOutput("emf.dt.coef")
                          , fileInput("file.emf.dt.coef", "Choose CSV File",
                                      accept = c(
                                        "text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")
                          )
                          , fluidRow(class = "download-row"
                                     , downloadButton("emf.dt.coef.csv", "csv")
                                     , downloadButton("emf.dt.coef.xlsx", "xlsx"))
                          , p("")
                          , textInput("emf.part.names", "Component names, comma separated"
                                      , paste(paste0("molecule", 1:4), collapse = ", "))
                   )
                   , column(2
                            , h4("K: lg constants")
                            , rHandsontableOutput("emf.cnst")
                            , fileInput("file.emf.cnst", "Choose CSV File",
                                        accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")
                            )
                            , fluidRow(class = "download-row"
                                       , downloadButton("emf.cnst.csv", "csv")
                                       , downloadButton("emf.cnst.xlsx", "xlsx"))
                   )
                   , column(5
                            , h4("Concentrations")
                            , rHandsontableOutput("emf.dt.conc")
                            , rHandsontableOutput("emf.part.eq")
                            , fileInput("file.emf.dt.conc", "Choose CSV File",
                                        accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")
                            )
                            , fluidRow(class = "download-row"
                                       , downloadButton("emf.dt.conc.csv", "csv")
                                       , downloadButton("emf.dt.conc.xlsx", "xlsx"))
                   )
                 )
                 , fluidRow(
                   column(6
                          , h4("EMF and deviations")
                          , rHandsontableOutput("dt.emf")
                          , fileInput("file.dt.emf", "Choose CSV File",
                                      accept = c(
                                        "text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")
                          )
                          , fluidRow(class = "download-row"
                                     , downloadButton("dt.emf.csv", "csv")
                                     , downloadButton("dt.emf.xlsx", "xlsx"))
                          
                   )
                   , column(6
                            , HTML("<h4>E<sub>0</sub> and Slope</h4>")
                            , rHandsontableOutput("emf.dt.params")
                            , fileInput("file.emf.dt.params", "Choose CSV File",
                                        accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")
                            )
                            , fluidRow(class = "download-row"
                                       , downloadButton("emf.target.csv", "csv")
                                       , downloadButton("emf.target.xlsx", "xlsx"))
                   )
                 )
               )
             )
           )
           
           , fluidRow(column(
             12
             , wellPanel(
               fluidRow(column(12
                               , actionButton("emf.conc.exec.btn", "Evaluate", class = "kev-ev-button")
               ))
             )))
           
           , fluidRow(column(
             12
             , wellPanel(
               fluidRow(column(12
                               , h4("Equilibrium concentrations")
                               , rHandsontableOutput("emf.dt.res")
                               , fluidRow(class = "download-row"
                                          , downloadButton("emf.dt.res.csv", "csv")
                                          , downloadButton("emf.dt.res.xlsx", "xlsx"))))
               
               , fluidRow(column(12
                                 , h4("Calculated EMF")
                                 , tabsetPanel(type = "tabs"
                                               , tabPanel("Absolute Errors"
                                                          , rHandsontableOutput("dt.emf.abs")
                                                          , fluidRow(class = "download-row"
                                                                     , downloadButton("dt.emf.abs.csv", "csv")
                                                                     , downloadButton("dt.emf.abs.xlsx", "xlsx")))
                                               , tabPanel("Relative Errors"
                                                          , rHandsontableOutput("dt.emf.rel")
                                                          , fluidRow(class = "download-row"
                                                                     , downloadButton("dt.emf.rel.csv", "csv")
                                                                     , downloadButton("dt.emf.rel.xlsx", "xlsx")))
                                               , tabPanel("Plot"
                                                          , plotlyOutput("plot.dt.emf"))
                                 )))
               
               , fluidRow(column(12)
                          , column(6
                                   , h4("Evaluated Constants")
                                   , rHandsontableOutput("emf.cnst.dev")
                                   , fluidRow(class = "download-row"
                                              , downloadButton("emf.cnst.dev.csv", "csv")
                                              , downloadButton("emf.cnst.dev.xlsx", "xlsx")))
                          , column(3
                                   , h4("Correlation Matrix")
                                   , rHandsontableOutput("emf.cor.m")
                                   , fluidRow(class = "download-row"
                                              , downloadButton("emf.cor.m.csv", "csv")
                                              , downloadButton("emf.cor.m.xlsx", "xlsx")))
                          , column(3
                                   , h4(HTML("Adjusted R<sup>2</sup>"))
                                   , rHandsontableOutput("emf.adj.r.squared")
                                   , fluidRow(class = "download-row"
                                              , downloadButton("emf.adj.r.squared.csv", "csv")
                                              , downloadButton("emf.adj.r.squared.xlsx", "xlsx"))))
               

             ))
           )
           
         )
         
  ),

# nmr (fast) ----------------------------------

  tabPanel(title = "NMR (Fast Exchange)"

         , id = "page.nm"
         
         , fluidPage(
           
           fluidRow(column(12, p(HTML("KEV: Chemistry Constant Evaluator<br/>"))))
           
           , titlePanel(HTML("Equilibrium Constants <i>via NMR (Fast Exchange)</i>"))
           
           , fluidRow(column(
             12
             , wellPanel(
               fluidRow(column(1, img(src = "nmr-icon.png", class = "kev-icon"))
                        , column(2
                               , h4("Column delimiter")
                               , radioButtons("nm.sep", "", inline = TRUE
                                              , c("," = "comma"
                                                  , ";" = "semicolon"
                                                  , "tab" = "tab")))
                        , column(3
                                 , HTML("<h4>Constants to evaluate</h4><p>Component names, comma separated</p>")
                                 , textInput("nm.cnst.tune", "", "molecule1"))
                        , column(3
                                 , HTML(paste("<h4>Threshold</h4><p>Search algorithm precision"
                                              ,"0&nbsp;&#60;&nbsp;&#950;&nbsp;&#60;&nbsp;1</p>"))
                                 , textInput("nm.threshold", "", "1e-7"))
                        , column(3
                                 , HTML("<h4>Search density</h4><p>Do not change unless you fully understand what you are doing</p>")
                                 , textInput("nm.search.density", "", "1"))
               )
             )))
           
           , fluidRow(column(
             12
             , wellPanel(
               fluidRow(column(12
                               , h3("Bulk upload / download (optional)")))
               
               , fluidRow(column(5
                                 , h4("Upload all data")
                                 , fileInput("nm.file.bulk.input", "Choose CSV files or XLSX file with multiple sheets",
                                             accept = c(
                                               "text/csv"
                                               , "text/comma-separated-values,text/plain"
                                               , ".csv"
                                               , ".xlsx")
                                             , multiple = TRUE
                                 ))
                          , column(3
                                   , h4("Download all data")
                                   , fluidRow(class = "download-row"
                                              , downloadButton("kev.nm.data.zip", "zip")
                                              , downloadButton("kev.nm.data.xlsx", "xlsx")))
                          , column(4
                                   , h4("Example data")
                                   , p("Learn how to prepare data via example datasets")
                                   , fluidRow(
                                     column(12
                                            , actionButton(inputId = "nm.example.data"
                                                           , label = HTML("&nbsp;Check examples")
                                                           , icon = icon("database")
                                                           , onclick = paste0("window.open('https://gitlab.com/"
                                                                              , "a.meshkov/KEV/tree/"
                                                                              ,"master/input/nmr', '_blank')")))))
               )
             )))
           
           , fluidRow(
             
             column(
               12
               , wellPanel(
                 h3("Upload or type input data")
                 ,  fluidRow(
                   column(5
                          , h4("Stoichiometric coefficients")
                          , rHandsontableOutput("nm.dt.coef")
                          , fileInput("file.nm.dt.coef", "Choose CSV File",
                                      accept = c(
                                        "text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")
                          )
                          , fluidRow(class = "download-row"
                                     , downloadButton("nm.dt.coef.csv", "csv")
                                     , downloadButton("nm.dt.coef.xlsx", "xlsx"))
                          , p("")
                          , textInput("nm.part.names", "Component names, comma separated"
                                      , paste(paste0("molecule", 1:4), collapse = ", "))
                   )
                   , column(2
                            , h4("K: lg constants")
                            , rHandsontableOutput("nm.cnst")
                            , fileInput("file.nm.cnst", "Choose CSV File",
                                        accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")
                            )
                            , fluidRow(class = "download-row"
                                       , downloadButton("nm.cnst.csv", "csv")
                                       , downloadButton("nm.cnst.xlsx", "xlsx"))
                   )
                   , column(5
                            , h4("Concentrations")
                            , rHandsontableOutput("nm.dt.conc")
                            , rHandsontableOutput("nm.part.eq")
                            , fileInput("file.nm.dt.conc", "Choose CSV File",
                                        accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")
                            )
                            , fluidRow(class = "download-row"
                                       , downloadButton("nm.dt.conc.csv", "csv")
                                       , downloadButton("nm.dt.conc.xlsx", "xlsx"))
                   )
                 )
                 , fluidRow(
                   column(6
                          , h4("Chemical shifts and deviations")
                          , rHandsontableOutput("dt.nm")
                          , fileInput("file.dt.nm", "Choose CSV File",
                                      accept = c(
                                        "text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")
                          )
                          , fluidRow(class = "download-row"
                                     , downloadButton("dt.nm.csv", "csv")
                                     , downloadButton("dt.nm.xlsx", "xlsx"))
                          
                   )
                   , column(6
                            , h4("Individual chemical shifts")
                            , rHandsontableOutput("nm.dt.ind")
                            , fluidRow(
                              column(8, fileInput("file.nm.dt.ind", "Choose CSV File",
                                                  accept = c(
                                                    "text/csv",
                                                    "text/comma-separated-values,text/plain",
                                                    ".csv")))
                            )
                            , fluidRow(class = "download-row"
                                       , downloadButton("nm.dt.ind.csv", "csv")
                                       , downloadButton("nm.dt.ind.xlsx", "xlsx"))
                   )
                 )
               )
             )
           )
           
           , fluidRow(column(
             12
             , wellPanel(
               fluidRow(column(12
                               , actionButton("nm.conc.exec.btn", "Evaluate", class = "kev-ev-button")
               ))
             )))
           
           , fluidRow(column(
             12
             , wellPanel(
               fluidRow(column(12
                               , h4("Equilibrium concentrations")
                               , rHandsontableOutput("nm.dt.res")
                               , fluidRow(class = "download-row"
                                          , downloadButton("nm.dt.res.csv", "csv")
                                          , downloadButton("nm.dt.res.xlsx", "xlsx"))))
               
               , fluidRow(column(12
                                 , h4("Calculated Chemical Shifts")
                                 , tabsetPanel(type = "tabs"
                                               , tabPanel("Absolute Errors"
                                                          , rHandsontableOutput("dt.nm.abs")
                                                          , fluidRow(class = "download-row"
                                                                     , downloadButton("dt.nm.abs.csv", "csv")
                                                                     , downloadButton("dt.nm.abs.xlsx", "xlsx")))
                                               , tabPanel("Relative Errors"
                                                          , rHandsontableOutput("dt.nm.rel")
                                                          , fluidRow(class = "download-row"
                                                                     , downloadButton("dt.nm.rel.csv", "csv")
                                                                     , downloadButton("dt.nm.rel.xlsx", "xlsx")))
                                               , tabPanel("Plot"
                                                          , plotlyOutput("plot.dt.nm"))
                                 )))
               
               , fluidRow(column(12)
                          , column(6
                                   , h4("Evaluated Constants")
                                   , rHandsontableOutput("nm.cnst.dev")
                                   , fluidRow(class = "download-row"
                                              , downloadButton("nm.cnst.dev.csv", "csv")
                                              , downloadButton("nm.cnst.dev.xlsx", "xlsx")))
                          , column(3
                                   , h4("Correlation Matrix")
                                   , rHandsontableOutput("nm.cor.m")
                                   , fluidRow(class = "download-row"
                                              , downloadButton("nm.cor.m.csv", "csv")
                                              , downloadButton("nm.cor.m.xlsx", "xlsx")))
                          , column(3
                                   , h4(HTML("Adjusted R<sup>2</sup>"))
                                   , rHandsontableOutput("nm.adj.r.squared")
                                   , fluidRow(class = "download-row"
                                              , downloadButton("nm.adj.r.squared.csv", "csv")
                                              , downloadButton("nm.adj.r.squared.xlsx", "xlsx"))))
               
               , fluidRow(column(12
                                 , h4("Individual Chemical Shifts with St.Errors")
                                 , rHandsontableOutput("nm.ind.shift")
                                 , fluidRow(class = "download-row"
                                            , downloadButton("nm.ind.shift.csv", "csv")
                                            , downloadButton("nm.ind.shift.xlsx", "xlsx"))))
               
               
             ))
           )
           
         )
         
    )
  )

# curve fitting -------------------------

  , tabPanel("Curve Fitting"
         , id = "page.cur"
         
         , fluidPage(
           
           fluidRow(column(12, p(HTML("KEV: Chemistry Constant Evaluator<br/>"))))
           
           , titlePanel("Curve Fitting")
           
           , fluidRow(column(
             12
             , wellPanel(
               fluidRow(column(1, img(src = "cur-icon.png", class = "kev-icon"))
                        , column(3
                                 , h4("Column delimiter of your data files")
                                 , radioButtons("cur.sep", "", inline = TRUE
                                                , c("," = "comma"
                                                    , ";" = "semicolon"
                                                    , "tab" = "tab"))
                        )
                        , column(8
                                 , h4("Task")
                                 , selectInput("cur.task"
                                               , ""
                                               , list("Spectrophotometry", "Other")
                                               , selected = "Spectrophotometry")
                        ))
             )))
           
           , fluidRow(column(
             12
             , wellPanel(
               fluidRow(column(12
                               , h3("Bulk upload / download")))
               
               , fluidRow(column(5
                                 , h4("Upload all data")
                                 , fileInput("file.cur.bulk.input", "Choose CSV files or XLSX file with multiple sheets",
                                             accept = c(
                                               "text/csv"
                                               , "text/comma-separated-values,text/plain"
                                               , ".csv"
                                               , ".xlsx")
                                             , multiple = TRUE))
                          
                          , column(3
                                   , h4("Download all data")
                                   , fluidRow(class = "download-row"
                                              , downloadButton("kev.cur.data.zip", "zip")
                                              , downloadButton("kev.cur.data.xlsx", "xlsx")))
                          
                          , column(4
                                   , h4("Example data")
                                   , p("Learn how to prepare data via example datasets")
                                   , fluidRow(
                                     column(12
                                            , actionButton(inputId = "cur.example.data"
                                                           , label = HTML("&nbsp;Check examples")
                                                           , icon = icon("database")
                                                           , onclick = paste0("window.open('https://gitlab.com/"
                                                                              , "a.meshkov/KEV/tree/"
                                                                              , "master/input/curves', '_blank')")))))
               )
             )))
           
           , fluidRow(column(
             12
             , wellPanel(
               fluidRow(column(4
                               , fluidRow(column(12
                                                 , selectInput("cur.add.curve.select"
                                                               , NULL
                                                               , cur.curves.list
                                                               , selected = "Add Curve")))
                               , fluidRow(id = "cur_new_curves_place", column(12, p("")))
                               , fluidRow(column(12, actionButton("cur.exec.btn", "Evaluate", class = "kev-ev-button")))
                               )
                        
                        , column(8
                                 , h4("")
                                 
                                 , tabsetPanel(type = "tabs"
                                               , tabPanel("Plot"
                                                          , plotlyOutput("plot.cur"))
                                               , tabPanel("Data"
                                                          , h4("Input Data")
                                                          , rHandsontableOutput("cur.dt.init")
                                                          , h4("Fitted Curves")
                                                          , rHandsontableOutput("cur.model.effects"))
                                               )
                                 
                                 , fluidRow(class = "download-row"
                                            , downloadButton("cur.model.effects.csv", "csv")
                                            , downloadButton("cur.model.effects.xlsx", "xlsx"))))
               
             ))
             
           )
           
        ))

# info ---------------------------------------

  , navbarMenu("More",
               tabPanel(HTML(paste0(
                 "<li><a href='https://gitlab.com/a.meshkov/KEV/raw/master/userguide/User_Guide_"
                 , userguide.date
                 ,".pdf?inline=false' target='_blank'>Help</a></li>"
                 ,"<li><a href='https://k-ev.org' target='_blank'>Home"))
               ))
# ---------------------------------------

  )
  , tags$footer(class = "kev-footer", HTML("Copyright 2018, 2019 &copy; G.Gamov, A.Meshkov | GNU GPL 3.0 | <a href='../#contact'>Contact us</a>"))

)


# backend -------------------------------------------------- #

server <- function(input, output, session) {

  
  values <- reactiveValues()
  
  input.source <- reactiveValues(
    
    eq.dt.coef.bulk = FALSE
    , eq.dt.conc.bulk = FALSE
    , eq.cnst.bulk = FALSE
    , eq.dt.conc.pc.fl = FALSE
    
    , ab.dt.coef.bulk = FALSE
    , ab.dt.conc.bulk = FALSE
    , ab.cnst.bulk = FALSE
    , dt.ab.bulk = FALSE
    , dt.mol.bulk = FALSE
    , dt.mol.memory = FALSE
    
    , emf.dt.coef.bulk = FALSE
    , emf.dt.conc.bulk = FALSE
    , emf.cnst.bulk = FALSE
    , dt.emf.bulk = FALSE
    , emf.dt.params.bulk = FALSE

    , nm.dt.coef.bulk = FALSE
    , nm.dt.conc.bulk = FALSE
    , nm.cnst.bulk = FALSE
    , dt.nm.bulk = FALSE
    , nm.dt.ind.bulk = FALSE
    
  )

  
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
  
  part.names.data <- reactive({
    
    if (!is.null(input$part.names)) {
      
      part.names <- input$part.names
      part.names <- str_split(part.names, "\\, *")
      part.names <- unlist(part.names)
      
    } else {
      
      if (is.null(values[["part.names"]])) {
        
        part.names <- "molecule1"
        
      } else {
        
        part.names <- values[["part.names"]]
        
      }
      
    }
    
    part.names <- str_trim(part.names)
    values[["part.names"]] <- part.names
    
    part.names

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
    
    cln <- part.names.data()[1:(ncol(eq.dt.conc.pc) + 1)]
    cln <- cln[!(cln %in% eq.pc.name.data())]
    
    validate(
      
      need(ncol(eq.dt.conc.pc) == length(cln), "Check if the variable component name is consistent with component names")
      
    )
    
    setnames(eq.dt.conc.pc, cln)
    
    values[["eq.dt.conc.pc"]] <- eq.dt.conc.pc
    
    eq.dt.conc.pc
    
  })
  
  cnst.data <- reactive({
    
    if (!is.null(input$cnst)) {
      
      cnst <- hot_to_r(input$cnst)
      
    } else {
      
      if (is.null(values[["cnst"]])) {
        
        cnst <- as.data.table(matrix(rep(1, 4), ncol = 1))
        setnames(cnst, "k_constants_log10")
        
      } else {
        
        cnst <- values[["cnst"]]
        
      }
      
    }
    
    cnst <- as.data.table(cnst)
    
    values[["cnst"]] <- cnst
    
    cnst
    
  })
  
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

    dt.conc <- data.table(eq.dt.conc.pc, pc.range)
    
    # type of concentration
    
    part.eq <- data.table(t(c(rep("tot", ncol(eq.dt.conc.pc)), "eq")))
    
    cln <- c(colnames(eq.dt.conc.pc), pc.name)
    setnames(part.eq, cln)

    # restore column order
    
    cln <- colnames(dt.coef.data())
    
    setcolorder(dt.conc, cln)
    setcolorder(part.eq, cln)
    
    # update values
    
    values[["dt.conc"]] <- dt.conc
    values[["part.eq"]] <- part.eq
    
    # input.source$eq.dt.conc.pc <- TRUE
    
    # return

    list(dt.conc = dt.conc, part.eq = part.eq)
    
  })
  
  eval.data <- reactive({
    
    withProgress(message = "Computation... It may take some time", value = 0, {
      
      incProgress(.1)
        
      validate(
        
        need(length(colnames(dt.coef.data())[colnames(dt.coef.data()) == bs.name.data()]) > 0, "Input correct component name to get fractions of")
        
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
                                 , dt.list = list(dt.coef = dt.coef.data()
                                                  , cnst = cnst.data()
                                                  , dt.conc = dt.conc.data()
                                                  , part.eq = part.eq.data())
                                 , save.res = FALSE
                                 , pc.name = pc.name)
    
      incProgress(.6)
      
    })
    
    res
    
  })
  
  
  # output data
  
  dt.res.data <- eventReactive(input$eq.conc.exec.btn, {
    
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

  dt.conc.tot.data <- eventReactive(input$eq.conc.exec.btn, {
    
    eval.data()$dt.conc.tot
    
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
  
  output$dt.coef <- renderRHandsontable({
    
    in.file <- input$file.dt.coef
    in.file.bulk <- input$file.eq.bulk.input
    in.file.xlsx <- NULL
    
    # bulk input
    
    if (input.source$eq.dt.coef.bulk) {
      
      bs.name.load()
      
      in.file <- as.data.table(input$file.eq.bulk.input)[name %like% "^(input\\_)*stoich(iometric)*\\_coefficients(\\.csv|\\.txt)*"][1]
      in.file <- as.data.frame(in.file)
      
      in.file.xlsx <- as.data.table(input$file.eq.bulk.input)[name %like% "\\.xlsx$"]
      
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
      
      if (eq.sep() == ";") {
        dt.coef <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character"), silent = TRUE)
      } else if (eq.sep() == ",") {
        dt.coef <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character"), silent = TRUE)
      } else if (eq.sep() == "tab") {
        dt.coef <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character"), silent = TRUE)
      }
      
      validate(
        
        need(is.data.frame(dt.coef), "Your file doesn't look like a stoich. coefficients file") %then%
          need(dt.coef[1, 1][!(dt.coef[1, 1] %like% "[a-zA-Z]")], "Your file doesn't look like a stoich. coefficients file")
        
      )
      
      tmp <- colnames(dt.coef)
      updateTextInput(session, "part.names", value = paste(tmp, collapse = ", "))
      
      
    } else if (!is.null(in.file.xlsx)) {
      
      shts <- getSheetNames(in.file.xlsx$datapath)
      
      shts <- shts[shts %like% "^(input_|output_)*stoich_coefficients"]
      shts <- sort(shts)
      
      dt.coef <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1]), silent = TRUE)

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
    in.file.bulk <- input$file.eq.bulk.input
    in.file.xlsx <- NULL
    
    # bulk input
    
    if (input.source$eq.dt.conc.bulk) {
      
      in.file <- as.data.table(input$file.eq.bulk.input)[name %like% "^(input\\_)*concentrations(\\.csv|\\.txt)*"][1]
      in.file <- as.data.frame(in.file)
      
      in.file.xlsx <- as.data.table(input$file.eq.bulk.input)[name %like% "\\.xlsx$"]
      
      if (nrow(in.file.xlsx) > 0) {
        
        in.file.xlsx <- as.data.frame(in.file.xlsx[1])
        
      } else {
        
        in.file.xlsx <- NULL
        
      }
      
      if (!is.null(in.file.xlsx))
        in.file <- NULL
      
    }
    
    if (input.source$eq.dt.conc.pc.fl) {
        
      in.file <- NULL
      in.file.xlsx <- NULL
      
    }
    
    # choose source
    
    if (!is.null(in.file)) {
      
      if (eq.sep() == ";") {
        dt.conc <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1), silent = TRUE)
      } else if (eq.sep() == ",") {
        dt.conc <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1), silent = TRUE)
      } else if (eq.sep() == "tab") {
        dt.conc <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1), silent = TRUE)
      }
      
      validate(need(is.data.frame(dt.conc), "Check the column delimiter or content of your file"))
      
      tmp <- colnames(dt.conc)
      updateTextInput(session, "part.names", value = paste(tmp, collapse = ", "))
      
      
    } else if (!is.null(in.file.xlsx)) {
      
      shts <- getSheetNames(in.file.xlsx$datapath)
      
      shts <- shts[shts %like% "^(input_|output_)*concentrations"]
      shts <- sort(shts)
      
      dt.conc <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1], startRow = 2), silent = TRUE)

      validate(need(is.data.frame(dt.conc), "Check the column delimiter or content of your file"))
      
      tmp <- colnames(dt.conc)
      updateTextInput(session, "part.names", value = paste(tmp, collapse = ", "))
      
    } else if (input.source$eq.dt.conc.pc.fl) {
      
      dt.conc <- eq.pc.update()$dt.conc
      
    } else {
      
      dt.conc <- dt.conc.data()
      
    }
    
    setnames(dt.conc, part.names.data()[1:ncol(dt.conc)])
    
    if (!is.null(dt.conc)) {
      
      if (nrow(dt.conc) > 15) {
        
        rhandsontable(dt.conc, stretchH = "all", useTypes = FALSE, height = 300) %>%
          hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
        
      } else {
        
        rhandsontable(dt.conc, stretchH = "all", useTypes = FALSE, height = NULL) %>%
          hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
        
      }
      
    }
    
    
    
  })

  output$part.eq <- renderRHandsontable({
    
    in.file <- input$file.dt.conc
    in.file.bulk <- input$file.eq.bulk.input
    in.file.xlsx <- NULL
    
    # bulk input
    
    if (input.source$eq.dt.conc.bulk) {
      
      in.file <- as.data.table(input$file.eq.bulk.input)[name %like% "^(input\\_)*concentrations(\\.csv|\\.txt)*"][1]
      in.file <- as.data.frame(in.file)
      
      in.file.xlsx <- as.data.table(input$file.eq.bulk.input)[name %like% "\\.xlsx$"]
      
      if (nrow(in.file.xlsx) > 0) {
        
        in.file.xlsx <- as.data.frame(in.file.xlsx[1])
        
      } else {
        
        in.file.xlsx <- NULL
        
      }
      
      if (!is.null(in.file.xlsx))
        in.file <- NULL
      
    }
    
    if (input.source$eq.dt.conc.pc.fl) {
      
      in.file <- NULL
      in.file.xlsx <- NULL
      
    }
    
    # choose source
    
    part.eq <- part.eq.data()
    
    if (!is.null(in.file)) {
      
      
      if (eq.sep() == ";") {
        
        part.eq <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", nrows = 1, header = FALSE), silent = TRUE)
        tmp <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1, header = FALSE)[1, ], silent = TRUE)
        
      } else if (eq.sep() == ",") {
        
        part.eq <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", nrows = 1, header = FALSE), silent = TRUE)
        tmp <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1, header = FALSE)[1, ], silent = TRUE)
        
      } else if (eq.sep() == "tab") {
        
        part.eq <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", nrows = 1, header = FALSE), silent = TRUE)
        tmp <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1, header = FALSE)[1, ], silent = TRUE)
        
      }
      
      validate(
        
        need(is.data.frame(part.eq), "Check the column delimiter or content of your file") %then%
          need(ncol(part.eq) == ncol(tmp), "Check the column delimiter or content of your file")
        
      )
      
      colnames(part.eq) <- tmp
      
    } else if (!is.null(in.file.xlsx)) {
      
      shts <- getSheetNames(in.file.xlsx$datapath)
      
      shts <- shts[shts %like% "^(input_|output_)*concentrations"]
      shts <- sort(shts)
      
      part.eq <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1], colNames = FALSE, rows = 1), silent = TRUE)
      tmp <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1], colNames = FALSE, rows = 2), silent = TRUE)

      validate(
        
        need(is.data.frame(part.eq), "Check the column delimiter or content of your file") %then%
          need(ncol(part.eq) == ncol(tmp), "Check the column delimiter or content of your file")
        
      )
      
      colnames(part.eq) <- tmp
      
    } else if (input.source$eq.dt.conc.pc.fl) {
      
      part.eq <- eq.pc.update()$part.eq
      
    }
    
    if (!is.null(part.eq))
      rhandsontable(part.eq, stretchH = "all", useTypes = FALSE, colHeaders = NULL) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    
  })
  
  output$eq.dt.conc.pc <- renderRHandsontable({
    
    eq.dt.conc.pc <- eq.dt.conc.pc.data()

    if (!is.null(eq.dt.conc.pc))
      rhandsontable(eq.dt.conc.pc, stretchH = "all", useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    
  })
  
  output$cnst <- renderRHandsontable({

    in.file <- input$file.cnst
    in.file.bulk <- input$file.eq.bulk.input
    in.file.xlsx <- NULL
    
    # bulk input
    
    if (input.source$eq.cnst.bulk) {
      
      in.file <- as.data.table(input$file.eq.bulk.input)[name %like% "^(input\\_)*k\\_constants\\_log10(\\.csv|\\.txt)*"][1]
      in.file <- as.data.frame(in.file)
      
      in.file.xlsx <- as.data.table(input$file.eq.bulk.input)[name %like% "\\.xlsx$"]
      
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
      
      if (eq.sep() == ";") {
        cnst <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character"), silent = TRUE)
      } else if (eq.sep() == ",") {
        cnst <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character"), silent = TRUE)
      } else if (eq.sep() == "tab") {
        cnst <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character"), silent = TRUE)
      }
      
      validate(
        need(is.data.frame(cnst), "Check the column delimiter or content of your file") %then%
          need(ncol(cnst) == 1, "Check the column delimiter or content of your file")
      )
      
    } else if (!is.null(in.file.xlsx)) {
      
      shts <- getSheetNames(in.file.xlsx$datapath)
      
      shts <- shts[shts %like% "^(input_|output_)*k_constants_log10"]
      shts <- sort(shts)
      
      cnst <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1]), silent = TRUE)

      validate(
        need(is.data.frame(cnst), "Check the column delimiter or content of your file") %then%
          need(ncol(cnst) == 1, "Check the column delimiter or content of your file")
      )
      
    } else {
      
      cnst <- cnst.data()
      
    }

    if (!is.null(cnst))
      rhandsontable(cnst, stretchH = "all", useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    
  })

  output$dt.res <- renderRHandsontable({
    
    dt.res <- dt.res.data()
    
    if (!is.null(dt.res)) {
      
      renderer <- "
      function (instance, td, row, col, prop, value, cellProperties) {
      
        Handsontable.renderers.TextRenderer.apply(this, arguments);
        
        if (parseInt(value, 10) < 0) {
        td.style.background = 'pink';
        }
        
      }" 
      
      if (nrow(dt.res) > 15) {
        
        rhandsontable(dt.res, stretchH = FALSE, useTypes = FALSE, height = 300) %>%
          hot_cols(renderer = renderer)
        
      } else {
        
        rhandsontable(dt.res, stretchH = FALSE, useTypes = FALSE, height = NULL) %>%
          hot_cols(renderer = renderer)
        
      }
      
    }
    

  })
  
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

  output$dt.conc.tot <- renderRHandsontable({
    
    dt.conc.tot <- dt.conc.tot.data()
    
    if (!is.null(dt.conc.tot))
      
      rhandsontable(dt.conc.tot, stretchH = FALSE, useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    
  })
  
  
  
  # absorbance -------------------------------------------------------
  
  
  # technical
  
  ab.sep <- reactive({
    
    switch(input$ab.sep,
           comma = ",",
           semicolon = ";",
           tab = "tab")
    
  })
  
  observeEvent(input$file.bulk.input, {
    
    # stoichiometric coefficients
    
    if (nrow(as.data.table(input$file.bulk.input)[name %like% "^(input\\_)*stoich(iometric)*\\_coefficients(\\.csv|\\.txt)*"]) > 0){
      input.source$ab.dt.coef.bulk <- TRUE
    }
    
    if (nrow(as.data.table(input$file.bulk.input)[name %like% "\\.xlsx$"]) > 0){
      
      shts <- getSheetNames(input$file.bulk.input$datapath)
      
      if (length(shts[shts %like% "stoich(iometric)*_coefficients"]))
        input.source$ab.dt.coef.bulk <- TRUE
      
    }
    
    # concentrations
    
    if (nrow(as.data.table(input$file.bulk.input)[name %like% "^(input\\_)*concentrations(\\.csv|\\.txt)*"]) > 0){
      input.source$ab.dt.conc.bulk <- TRUE
    }
    
    if (nrow(as.data.table(input$file.bulk.input)[name %like% "\\.xlsx$"]) > 0){
      
      shts <- getSheetNames(input$file.bulk.input$datapath)
      
      if (length(shts[shts %like% "^(input_|output_)*concentrations"]))
        input.source$ab.dt.conc.bulk <- TRUE
      
    }
    
    # constants
    
    if (nrow(as.data.table(input$file.bulk.input)[name %like% "^(input\\_)*k\\_constants\\_log10(\\.csv|\\.txt)*"]) > 0){
      input.source$ab.cnst.bulk <- TRUE
    }
    
    if (nrow(as.data.table(input$file.bulk.input)[name %like% "\\.xlsx$"]) > 0){
      
      shts <- getSheetNames(input$file.bulk.input$datapath)
      
      if (length(shts[shts %like% "^((input_)*k_constants_log10|constants_evaluated)"]))
        input.source$ab.cnst.bulk <- TRUE
      
    }
    
    # absorbance
    
    if (nrow(as.data.table(input$file.bulk.input)[name %like% "^(input\\_)*absorbance(\\.csv|\\.txt)*"]) > 0){
      input.source$dt.ab.bulk <- TRUE
    }
    
    if (nrow(as.data.table(input$file.bulk.input)[name %like% "\\.xlsx$"]) > 0){
      
      shts <- getSheetNames(input$file.bulk.input$datapath)
      
      if (length(shts[shts %like% "absorbance"]))
        input.source$dt.ab.bulk <- TRUE
      
    }
    
    # molar extinction coefficients
    
    if (nrow(as.data.table(input$file.bulk.input)[name %like% "^(input\\_)*mol(ar)*\\_ext(inction)*\\_coefficients(\\.csv|\\.txt)*"]) > 0){
      input.source$dt.mol.bulk <- TRUE
      input.source$dt.mol.memory <- FALSE
    }
    
    if (nrow(as.data.table(input$file.bulk.input)[name %like% "\\.xlsx$"]) > 0){
      
      shts <- getSheetNames(input$file.bulk.input$datapath)
      
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
  
  ab.dt.coef.data <- reactive({
    
    if (!is.null(input$ab.dt.coef)) {
      
      dt.coef <- hot_to_r(input$ab.dt.coef)
      
    } else {
      
      if (is.null(values[["ab.dt.coef"]])) {
        
        dt.coef <- as.data.table(matrix(rep(1, 16), 4))
        setnames(dt.coef, paste0("molecule", 1:4))
        
        # dt.coef <- as.data.table(dt.coef)
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
        setnames(cnst, "k_constants_log10")
        
      } else {
        
        cnst <- values[["ab.cnst"]]
        
      }
      
    }
    
    cnst <- as.data.table(cnst)
    
    values[["ab.cnst"]] <- cnst
    # browser()
    cnst
    
  })
  
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
  cnst.tune.load <- reactive({
    
    in.file.bulk <- input$file.bulk.input
    in.file.xlsx <- NULL
    in.file <- NULL
    
    # bulk input
    
    if (nrow(as.data.table(input$file.bulk.input)[name %like% "^(constants*_names*|targets*)(\\.csv|\\.txt)*"]) > 0){

      in.file <- as.data.table(input$file.bulk.input)[name %like% "^(constants*_names*|targets*)(\\.csv|\\.txt)*"][1]
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
    
    in.file.bulk <- input$file.bulk.input
    in.file.xlsx <- NULL
    in.file <- NULL
    
    # bulk input
    
    if (nrow(as.data.table(input$file.bulk.input)[name %like% "^(constants_names|targets*)(\\.csv|\\.txt)*"]) > 0){
      
      in.file <- as.data.table(input$file.bulk.input)[name %like% "^(constants_names|targets*)(\\.csv|\\.txt)*"][1]
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
  
  cnst.dev.data <- eventReactive(input$ab.conc.exec.btn, {
    
    cnst.dev <- ab.eval.data()$cnst.dev
    cnst.dev <- as.data.table(cnst.dev)
    
    setnames(cnst.dev, c("Component", "Constant", "St.Deviation", "Validity"))
    
  })

  cor.m.data <- eventReactive(input$ab.conc.exec.btn, {
    
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
  
  output$ab.dt.coef <- renderRHandsontable({
    
    in.file <- input$file.ab.dt.coef
    in.file.bulk <- input$file.bulk.input
    in.file.xlsx <- NULL
    
    # bulk input
    
    if (input.source$ab.dt.coef.bulk) {
      
      try(cnst.tune.load(), silent = TRUE)
      try(wl.tune.load(), silent = TRUE)
      
      
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
      
      if (ab.sep() == ";") {
        dt.coef <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
      } else if (ab.sep() == ",") {
        dt.coef <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
      } else if (ab.sep() == "tab") {
        dt.coef <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
      }

      setDT(dt.coef)
      
      cln <- colnames(dt.coef)
      setnames(dt.coef, cln, str_replace(cln, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0x2e), as.raw(0xbf)))), ""))
      
      validate(
        
        need(is.data.frame(dt.coef), "Your file doesn't look like a stoich. coefficients file") %then%
          need(dt.coef[1, 1][!(dt.coef[1, 1] %like% "[a-zA-Z]")], "Your file doesn't look like a stoich. coefficients file") %then%
          need(nrow(dt.coef) + ncol(dt.coef) == length(unique(c(colnames(dt.coef), dt.coef$name))), "Duplicate component names")
        
      )
      
      # dt.coef <- as.data.table(dt.coef)
      
      tmp <- colnames(dt.coef)
      updateTextInput(session, "ab.part.names", value = paste(tmp[1:(length(tmp) - 1)], collapse = ", "))
      
      
    } else if (!is.null(in.file.xlsx)) {
      
      shts <- getSheetNames(in.file.xlsx$datapath)
      
      shts <- shts[shts %like% "^(input_|output_)*stoich_coefficients"]
      shts <- sort(shts)

      dt.coef <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1]), silent = TRUE)
      
      validate(
        
        need(is.data.frame(dt.coef), "Your file doesn't look like a stoich. coefficients file") %then%
          need(dt.coef[1, 1][!(dt.coef[1, 1] %like% "[a-zA-Z]")], "Your file doesn't look like a stoich. coefficients file") %then%
          need(nrow(dt.coef) + ncol(dt.coef) == length(unique(c(colnames(dt.coef), dt.coef$name))), "Duplicate component names")
        
      )
      
      # dt.coef <- as.data.table(dt.coef)
      
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
      
      in.file <- as.data.table(input$file.bulk.input)[name %like% "^(input\\_)*concentrations(\\.csv|\\.txt)*$"][1]
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
      
      if (ab.sep() == ";") {
        dt.conc <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1, check.names = FALSE), silent = TRUE)
      } else if (ab.sep() == ",") {
        dt.conc <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1, check.names = FALSE), silent = TRUE)
      } else if (ab.sep() == "tab") {
        dt.conc <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1, check.names = FALSE), silent = TRUE)
      }
      
      setDT(dt.conc)
      
      cln <- colnames(dt.conc)
      setnames(dt.conc, cln, str_replace(cln, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0x2e), as.raw(0xbf)))), ""))
      
      validate(need(is.data.frame(dt.conc), "Check the column delimiter or content of your file"))
      
      tmp <- colnames(dt.conc)
      updateTextInput(session, "ab.part.names", value = paste(tmp, collapse = ", "))
      
      
    } else if (!is.null(in.file.xlsx)) {
      
      shts <- getSheetNames(in.file.xlsx$datapath)
      
      shts <- shts[shts %like% "^(input_|output_)*concentrations"]
      shts <- sort(shts)
      
      dt.conc <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1], startRow = 2), silent = TRUE)
      
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
      
      in.file <- as.data.table(input$file.bulk.input)[name %like% "^(input\\_)*concentrations(\\.csv|\\.txt)*$"][1]
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
      
      
      if (ab.sep() == ";") {
        
        part.eq <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", nrows = 1, header = FALSE), silent = TRUE)
        tmp <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1, header = FALSE)[1, ], silent = TRUE)
        
      } else if (ab.sep() == ",") {
        
        part.eq <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", nrows = 1, header = FALSE), silent = TRUE)
        tmp <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1, header = FALSE)[1, ], silent = TRUE)
        
      } else if (ab.sep() == "tab") {
        
        part.eq <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", nrows = 1, header = FALSE), silent = TRUE)
        tmp <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1, header = FALSE)[1, ], silent = TRUE)
        
      }

      setDT(part.eq)
      part.eq[1, V1 := str_replace(V1, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0xbb), as.raw(0xbf)))), "")]
      
      validate(
        
        need(is.data.frame(part.eq), "Check the column delimiter or content of your file") %then%
          need(ncol(part.eq) == ncol(tmp), "Check the column delimiter or content of your file")
        
      )
      
      colnames(part.eq) <- unlist(tmp)
      
    } else if (!is.null(in.file.xlsx)) {
      
      shts <- getSheetNames(in.file.xlsx$datapath)
      
      shts <- shts[shts %like% "^(input_|output_)*concentrations"]
      shts <- sort(shts)

      part.eq <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1], colNames = FALSE, rows = 1), silent = TRUE)
      tmp <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1], colNames = FALSE, rows = 2), silent = TRUE)
      
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
      
      in.file <- as.data.table(input$file.bulk.input)[name %like% "^(input\\_)*k\\_constants\\_log10(\\.csv|\\.txt)*$"][1]
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
      
      if (ab.sep() == ";") {
        cnst <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
      } else if (ab.sep() == ",") {
        cnst <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
      } else if (ab.sep() == "tab") {
        cnst <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
      }
      
      setDT(cnst)
      
      cln <- colnames(cnst)
      setnames(cnst, cln, str_replace(cln, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0x2e), as.raw(0xbf)))), ""))
      
      validate(
        need(is.data.frame(cnst), "Check the column delimiter or content of your file") %then%
          need(length(colnames(cnst)[colnames(cnst) %like% "^Constant$|^k_constants_log10$|^cnst$"]) == 1
               , "Check the column delimiter or content of your file")
      )
      
    } else if (!is.null(in.file.xlsx)) {
      
      shts <- getSheetNames(in.file.xlsx$datapath)
      
      shts <- shts[shts %like% "^((input_)*k_constants_log10|constants_evaluated)"]
      shts <- sort(shts, decreasing = TRUE)
      
      cnst <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1]), silent = TRUE)
      
      validate(
        need(is.data.frame(cnst), "Check the column delimiter or content of your file") %then%
          need(length(colnames(cnst)[colnames(cnst) %like% "^Constant$|^k_constants_log10$|^cnst$"]) == 1
               , "Check the column delimiter or content of your file")
      )
      
    } else {
      
      cnst <- ab.cnst.data()
      
    }
    
    setDT(cnst)
    
    cln <- colnames(cnst)
    cln <- cln[cln %like% "^Constant$|^k_constants_log10$|^cnst$"]
    
    cnst <- cnst[, cln, with = FALSE]
    
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
      
      in.file <- as.data.table(input$file.bulk.input)[name %like% "^(input\\_)*absorbance(\\.csv|\\.txt)*$"][1]
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
    in.file.bulk <- input$file.bulk.input
    in.file.xlsx <- NULL
    
    # bulk input
    
    if (input.source$dt.mol.bulk) {
      
      in.file <- as.data.table(input$file.bulk.input)[name %like% "^(input\\_)*mol(ar)*\\_ext(inction)*\\_coefficients(\\.csv|\\.txt)*$"][1]
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
  
  output$ab.dt.res <- renderRHandsontable({
    
    dt.res <- ab.dt.res.data()
    
    if (!is.null(dt.res)) {
      
      renderer <- "
        function (instance, td, row, col, prop, value, cellProperties) {
    
          Handsontable.renderers.TextRenderer.apply(this, arguments);
          
          if (parseInt(value, 10) < 0) {
            td.style.background = 'pink';
          }
          
        }" 

      rhandsontable(dt.res, stretchH = FALSE, useTypes = FALSE) %>%
        hot_cols(renderer = renderer)
    }
    
  })
  
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
  
  output$cnst.dev <- renderRHandsontable({
    
    cnst.dev <- cnst.dev.data()
    
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

  output$cor.m <- renderRHandsontable({
    
    cor.m <- cor.m.data()
    
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
  
  emf.dt.coef.data <- reactive({
    
    if (!is.null(input$emf.dt.coef)) {
      
      dt.coef <- hot_to_r(input$emf.dt.coef)
      
    } else {
      
      if (is.null(values[["emf.dt.coef"]])) {
        
        dt.coef <- as.data.table(matrix(rep(1, 16), 4))
        setnames(dt.coef, paste0("molecule", 1:4))
        
        # dt.coef <- as.data.table(dt.coef)
        dt.coef <- cbind(dt.coef, name = paste0("product", 1:4))
        
      } else {
        
        dt.coef <- values[["emf.dt.coef"]]
        
      }
      
    }
    
    dt.coef <- as.data.table(dt.coef)
    
    setnames(dt.coef, c(emf.part.names.data()[1:(ncol(dt.coef) - 1)], "name"))
    
    values[["emf.dt.coef"]] <- dt.coef
    
    dt.coef
    
  })
  
  emf.dt.conc.data <- reactive({
    
    if (!is.null(input$emf.dt.conc)) {
      
      dt.conc <- hot_to_r(input$emf.dt.conc)
      
    } else {
      
      if (is.null(values[["emf.dt.conc"]])) {
        
        dt.conc <- as.data.table(matrix(rep(1e-03, 20), ncol = 4))
        setnames(dt.conc, paste0("molecule", 1:4))
        
      } else {
        
        dt.conc <- values[["emf.dt.conc"]]
        
      }
      
    }
    
    dt.conc <- as.data.table(dt.conc)
    setnames(dt.conc, emf.part.names.data()[1:ncol(dt.conc)])
    
    values[["emf.dt.conc"]] <- dt.conc
    
    dt.conc
    
  })
  
  emf.part.eq.data <- reactive({
    
    if (!is.null(input$emf.part.eq)) {
      
      part.eq <- hot_to_r(input$emf.part.eq)
      
    } else {
      
      if (is.null(values[["emf.part.eq"]])) {
        
        part.eq <- as.data.table(matrix(rep("tot", 4), ncol = 4))
        setnames(part.eq, paste0("molecule", 1:4))
        
      } else {
        
        part.eq <- values[["emf.part.eq"]]
        
      }
      
    }
    
    part.eq <- as.data.table(part.eq)
    
    values[["emf.part.eq"]] <- part.eq
    
    part.eq
    
  })
  
  emf.cnst.data <- reactive({
    
    if (!is.null(input$emf.cnst)) {
      
      cnst <- hot_to_r(input$emf.cnst)
      
    } else {
      
      if (is.null(values[["emf.cnst"]])) {
        
        cnst <- as.data.table(matrix(rep(1, 4), ncol = 1))
        setnames(cnst, "k_constants_log10")
        
      } else {
        
        cnst <- values[["emf.cnst"]]
        
      }
      
    }
    
    cnst <- as.data.table(cnst)
    
    values[["emf.cnst"]] <- cnst
    # browser()
    cnst
    
  })
  
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
  
  output$emf.dt.coef <- renderRHandsontable({
    
    in.file <- input$file.emf.dt.coef
    in.file.bulk <- input$file.emf.bulk.input
    in.file.xlsx <- NULL
    
    # bulk input
    
    if (input.source$emf.dt.coef.bulk) {
      
      in.file <- as.data.table(input$file.emf.bulk.input)[name %like% "^(input\\_)*stoich(iometric)*\\_coefficients(\\.csv|\\.txt)*"][1]
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
        dt.coef <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
      } else if (emf.sep() == ",") {
        dt.coef <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
      } else if (emf.sep() == "tab") {
        dt.coef <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
      }
      
      setDT(dt.coef)
      
      cln <- colnames(dt.coef)
      setnames(dt.coef, cln, str_replace(cln, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0x2e), as.raw(0xbf)))), ""))
      
      validate(
        
        need(is.data.frame(dt.coef), "Your file doesn't look like a stoich. coefficients file") %then%
          need(dt.coef[1, 1][!(dt.coef[1, 1] %like% "[a-zA-Z]")], "Your file doesn't look like a stoich. coefficients file") %then%
          need(nrow(dt.coef) + ncol(dt.coef) == length(unique(c(colnames(dt.coef), dt.coef$name))), "Duplicate component names")
        
      )
      
      tmp <- colnames(dt.coef)
      updateTextInput(session, "emf.part.names", value = paste(tmp[1:(length(tmp) - 1)], collapse = ", "))
      
      
    } else if (!is.null(in.file.xlsx)) {
      
      shts <- getSheetNames(in.file.xlsx$datapath)
      
      shts <- shts[shts %like% "^(input_|output_)*stoich_coefficients"]
      shts <- sort(shts)
      
      dt.coef <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1]), silent = TRUE)
      
      validate(
        
        need(is.data.frame(dt.coef), "Your file doesn't look like a stoich. coefficients file") %then%
          need(dt.coef[1, 1][!(dt.coef[1, 1] %like% "[a-zA-Z]")], "Your file doesn't look like a stoich. coefficients file") %then%
          need(nrow(dt.coef) + ncol(dt.coef) == length(unique(c(colnames(dt.coef), dt.coef$name))), "Duplicate component names")
        
      )
      
      tmp <- colnames(dt.coef)
      updateTextInput(session, "emf.part.names", value = paste(tmp[1:(length(tmp) - 1)], collapse = ", "))
      
    } else {
      
      dt.coef <- emf.dt.coef.data()
      
    }
    
    setnames(dt.coef, c(emf.part.names.data()[1:(ncol(dt.coef) - 1)], "name"))
    
    if (!is.null(dt.coef))
      rhandsontable(dt.coef, stretchH = "all", useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    
  })
  
  output$emf.dt.conc <- renderRHandsontable({
    
    in.file <- input$file.emf.dt.conc
    in.file.bulk <- input$file.emf.bulk.input
    in.file.xlsx <- NULL
    
    # bulk input
    
    if (input.source$emf.dt.conc.bulk) {
      
      in.file <- as.data.table(input$file.emf.bulk.input)[name %like% "^(input\\_)*concentrations(\\.csv|\\.txt)*$"][1]
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
        dt.conc <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1, check.names = FALSE), silent = TRUE)
      } else if (emf.sep() == ",") {
        dt.conc <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1, check.names = FALSE), silent = TRUE)
      } else if (emf.sep() == "tab") {
        dt.conc <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1, check.names = FALSE), silent = TRUE)
      }
      
      setDT(dt.conc)
      
      cln <- colnames(dt.conc)
      setnames(dt.conc, cln, str_replace(cln, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0x2e), as.raw(0xbf)))), ""))
      
      validate(need(is.data.frame(dt.conc), "Check the column delimiter or content of your file"))
      
      tmp <- colnames(dt.conc)
      updateTextInput(session, "emf.part.names", value = paste(tmp, collapse = ", "))
      
      
    } else if (!is.null(in.file.xlsx)) {
      
      shts <- getSheetNames(in.file.xlsx$datapath)
      
      shts <- shts[shts %like% "^(input_|output_)*concentrations"]
      shts <- sort(shts)
      
      dt.conc <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1], startRow = 2), silent = TRUE)
      
      validate(need(is.data.frame(dt.conc), "Check the column delimiter or content of your file"))
      
      tmp <- colnames(dt.conc)
      updateTextInput(session, "emf.part.names", value = paste(tmp, collapse = ", "))
      
    } else {
      
      dt.conc <- emf.dt.conc.data()
      
    }
    
    setnames(dt.conc, emf.part.names.data()[1:ncol(dt.conc)])
    
    if (!is.null(dt.conc))
      rhandsontable(dt.conc, stretchH = "all", useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    
  })
  
  output$emf.part.eq <- renderRHandsontable({
    
    in.file <- input$file.emf.dt.conc
    in.file.bulk <- input$file.emf.bulk.input
    in.file.xlsx <- NULL
    
    # bulk input
    
    if (input.source$emf.dt.conc.bulk) {
      
      in.file <- as.data.table(input$file.emf.bulk.input)[name %like% "^(input\\_)*concentrations(\\.csv|\\.txt)*$"][1]
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
    
    part.eq <- emf.part.eq.data()
    
    if (!is.null(in.file)) {
      
      
      if (emf.sep() == ";") {
        
        part.eq <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", nrows = 1, header = FALSE), silent = TRUE)
        tmp <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1, header = FALSE)[1, ], silent = TRUE)
        
      } else if (emf.sep() == ",") {
        
        part.eq <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", nrows = 1, header = FALSE), silent = TRUE)
        tmp <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1, header = FALSE)[1, ], silent = TRUE)
        
      } else if (emf.sep() == "tab") {
        
        part.eq <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", nrows = 1, header = FALSE), silent = TRUE)
        tmp <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1, header = FALSE)[1, ], silent = TRUE)
        
      }
      
      setDT(part.eq)
      part.eq[1, V1 := str_replace(V1, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0xbb), as.raw(0xbf)))), "")]
      
      validate(
        
        need(is.data.frame(part.eq), "Check the column delimiter or content of your file") %then%
          need(ncol(part.eq) == ncol(tmp), "Check the column delimiter or content of your file")
        
      )
      
      colnames(part.eq) <- unlist(tmp)
      
    } else if (!is.null(in.file.xlsx)) {
      
      shts <- getSheetNames(in.file.xlsx$datapath)
      
      shts <- shts[shts %like% "^(input_|output_)*concentrations"]
      shts <- sort(shts)
      
      part.eq <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1], colNames = FALSE, rows = 1), silent = TRUE)
      tmp <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1], colNames = FALSE, rows = 2), silent = TRUE)
      
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
  
  output$emf.cnst <- renderRHandsontable({
    
    in.file <- input$file.emf.cnst
    in.file.bulk <- input$file.emf.bulk.input
    in.file.xlsx <- NULL
    
    # bulk input
    
    if (input.source$emf.cnst.bulk) {
      
      in.file <- as.data.table(input$file.emf.bulk.input)[name %like% "^(input\\_)*k\\_constants\\_log10(\\.csv|\\.txt)*$"][1]
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
        cnst <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character"), silent = TRUE)
      } else if (emf.sep() == ",") {
        cnst <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character"), silent = TRUE)
      } else if (emf.sep() == "tab") {
        cnst <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character"), silent = TRUE)
      }
      
      setDT(cnst)
      
      cln <- colnames(cnst)
      setnames(cnst, cln, str_replace(cln, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0x2e), as.raw(0xbf)))), ""))
      
      validate(
        need(is.data.frame(cnst), "Check the column delimiter or content of your file") %then%
          need(length(colnames(cnst)[colnames(cnst) %like% "^Constant$|^k_constants_log10$|^cnst$"]) == 1
               , "Check the column delimiter or content of your file")
      )
      
    } else if (!is.null(in.file.xlsx)) {
      
      shts <- getSheetNames(in.file.xlsx$datapath)
      
      shts <- shts[shts %like% "^((input_)*k_constants_log10|constants_evaluated)"]
      shts <- sort(shts, decreasing = TRUE)
      
      cnst <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1]), silent = TRUE)
      
      validate(
        need(is.data.frame(cnst), "Check the column delimiter or content of your file") %then%
          need(length(colnames(cnst)[colnames(cnst) %like% "^Constant$|^k_constants_log10$|^cnst$"]) == 1
               , "Check the column delimiter or content of your file")
      )
      
    } else {
      
      cnst <- emf.cnst.data()
      
    }
    
    setDT(cnst)
    
    cln <- colnames(cnst)
    cln <- cln[cln %like% "^Constant$|^k_constants_log10$|^cnst$"]
    
    cnst <- cnst[, cln, with = FALSE]
    
    if (!is.null(cnst))
      rhandsontable(cnst, stretchH = "all", useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    
  })
  
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
  
  output$emf.dt.res <- renderRHandsontable({
    
    dt.res <- emf.dt.res.data()
    
    if (!is.null(dt.res)) {
      
      renderer <- "
      function (instance, td, row, col, prop, value, cellProperties) {
      
      Handsontable.renderers.TextRenderer.apply(this, arguments);
      
      if (parseInt(value, 10) < 0) {
      td.style.background = 'pink';
      }
      
      }" 

      rhandsontable(dt.res, stretchH = FALSE, useTypes = FALSE) %>%
        hot_cols(renderer = renderer)
    }
    
})
  
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
  
  
  
  

    
  # nmr (fast) -------------------------------------------------------
  
  
  # technical
  
  nm.sep <- reactive({
    
    switch(input$nm.sep,
           comma = ",",
           semicolon = ";",
           tab = "tab")
    
  })
  
  observeEvent(input$nm.file.bulk.input, {
    
    # stoichiometric coefficients
    
    if (nrow(as.data.table(input$nm.file.bulk.input)[name %like% "^(input\\_)*stoich(iometric)*\\_coefficients(\\.csv|\\.txt)*"]) > 0){
      input.source$nm.dt.coef.bulk <- TRUE
    }
    
    if (nrow(as.data.table(input$nm.file.bulk.input)[name %like% "\\.xlsx$"]) > 0){
      
      shts <- getSheetNames(input$nm.file.bulk.input$datapath)
      
      if (length(shts[shts %like% "stoich(iometric)*_coefficients"]))
        input.source$nm.dt.coef.bulk <- TRUE
      
    }
    
    # concentrations
    
    if (nrow(as.data.table(input$nm.file.bulk.input)[name %like% "^(input\\_)*concentrations(\\.csv|\\.txt)*"]) > 0){
      input.source$nm.dt.conc.bulk <- TRUE
    }
    
    if (nrow(as.data.table(input$nm.file.bulk.input)[name %like% "\\.xlsx$"]) > 0){
      
      shts <- getSheetNames(input$nm.file.bulk.input$datapath)
      
      if (length(shts[shts %like% "^(input_|output_)*concentrations"]))
        input.source$nm.dt.conc.bulk <- TRUE
      
    }
    
    # constants
    
    if (nrow(as.data.table(input$nm.file.bulk.input)[name %like% "^(input\\_)*k\\_constants\\_log10(\\.csv|\\.txt)*"]) > 0){
      input.source$nm.cnst.bulk <- TRUE
    }
    
    if (nrow(as.data.table(input$nm.file.bulk.input)[name %like% "\\.xlsx$"]) > 0){
      
      shts <- getSheetNames(input$nm.file.bulk.input$datapath)
      
      if (length(shts[shts %like% "^((input_)*k_constants_log10|constants_evaluated)"]))
        input.source$nm.cnst.bulk <- TRUE
      
    }
    
    # chemical shifts
    
    if (nrow(as.data.table(input$nm.file.bulk.input)[name %like% "^(input\\_)*chemical\\_shifts*(\\.csv|\\.txt)*"]) > 0){
      input.source$dt.nm.bulk <- TRUE
    }
    
    if (nrow(as.data.table(input$nm.file.bulk.input)[name %like% "\\.xlsx$"]) > 0){
      
      shts <- getSheetNames(input$nm.file.bulk.input$datapath)
      
      if (length(shts[shts %like% "chemical\\_shifts*" & !(shts %like% "ind(ividual)*\\_(chemical\\_)*shifts*")]))
        input.source$dt.nm.bulk <- TRUE
      
    }
    
    # individual chemical shifts
    
    if (nrow(as.data.table(input$nm.file.bulk.input)[name %like% "^(input\\_)*ind(ividual)*\\_(chemical\\_)*shifts*(\\.csv|\\.txt)*"]) > 0){
      input.source$nm.dt.ind.bulk <- TRUE
      input.source$nm.dt.ind.memory <- FALSE
    }
    
    if (nrow(as.data.table(input$nm.file.bulk.input)[name %like% "\\.xlsx$"]) > 0){
      
      shts <- getSheetNames(input$nm.file.bulk.input$datapath)
      
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
  
  nm.dt.coef.data <- reactive({
    
    if (!is.null(input$nm.dt.coef)) {
      
      dt.coef <- hot_to_r(input$nm.dt.coef)
      
    } else {
      
      if (is.null(values[["nm.dt.coef"]])) {
        
        dt.coef <- as.data.table(matrix(rep(1, 16), 4))
        setnames(dt.coef, paste0("molecule", 1:4))
        
        # dt.coef <- as.data.table(dt.coef)
        dt.coef <- cbind(dt.coef, name = paste0("product", 1:4))
        
      } else {
        
        dt.coef <- values[["nm.dt.coef"]]
        
      }
      
    }
    
    dt.coef <- as.data.table(dt.coef)
    
    setnames(dt.coef, c(nm.part.names.data()[1:(ncol(dt.coef) - 1)], "name"))
    
    values[["nm.dt.coef"]] <- dt.coef
    
    dt.coef
    
  })
  
  nm.dt.conc.data <- reactive({
    
    if (!is.null(input$nm.dt.conc)) {
      
      dt.conc <- hot_to_r(input$nm.dt.conc)
      
    } else {
      
      if (is.null(values[["nm.dt.conc"]])) {
        
        dt.conc <- as.data.table(matrix(rep(1e-03, 20), ncol = 4))
        setnames(dt.conc, paste0("molecule", 1:4))
        
      } else {
        
        dt.conc <- values[["nm.dt.conc"]]
        
      }
      
    }
    
    dt.conc <- as.data.table(dt.conc)
    setnames(dt.conc, nm.part.names.data()[1:ncol(dt.conc)])
    
    values[["nm.dt.conc"]] <- dt.conc
    
    dt.conc
    
  })
  
  nm.part.eq.data <- reactive({
    
    if (!is.null(input$nm.part.eq)) {
      
      part.eq <- hot_to_r(input$nm.part.eq)
      
    } else {
      
      if (is.null(values[["nm.part.eq"]])) {
        
        part.eq <- as.data.table(matrix(rep("tot", 4), ncol = 4))
        setnames(part.eq, paste0("molecule", 1:4))
        
      } else {
        
        part.eq <- values[["nm.part.eq"]]
        
      }
      
    }
    
    part.eq <- as.data.table(part.eq)
    
    values[["nm.part.eq"]] <- part.eq
    
    part.eq
    
  })
  
  nm.cnst.data <- reactive({
    
    if (!is.null(input$nm.cnst)) {
      
      cnst <- hot_to_r(input$nm.cnst)
      
    } else {
      
      if (is.null(values[["nm.cnst"]])) {
        
        cnst <- as.data.table(matrix(rep(1, 4), ncol = 1))
        setnames(cnst, "k_constants_log10")
        
      } else {
        
        cnst <- values[["nm.cnst"]]
        
      }
      
    }
    
    cnst <- as.data.table(cnst)
    
    values[["nm.cnst"]] <- cnst
    # browser()
    cnst
    
  })
  
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
    
    in.file.bulk <- input$nm.file.bulk.input
    in.file.xlsx <- NULL
    in.file <- NULL
    
    # bulk input
    
    if (nrow(as.data.table(input$nm.file.bulk.input)[name %like% "^(constants*_names*|targets*)(\\.csv|\\.txt)*"]) > 0){
      
      in.file <- as.data.table(input$nm.file.bulk.input)[name %like% "^(constants*_names*|targets*)(\\.csv|\\.txt)*"][1]
      in.file <- as.data.frame(in.file)
      
    }
    
    in.file.xlsx <- as.data.table(input$nm.file.bulk.input)[name %like% "\\.xlsx$"]
    
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
  
  output$nm.dt.coef <- renderRHandsontable({
    
    in.file <- input$file.nm.dt.coef
    in.file.bulk <- input$nm.file.bulk.input
    in.file.xlsx <- NULL
    
    # bulk input
    
    if (input.source$nm.dt.coef.bulk) {
      
      try(nm.cnst.tune.load(), silent = TRUE)
      
      in.file <- as.data.table(input$nm.file.bulk.input)[name %like% "^(input\\_)*stoich(iometric)*\\_coefficients(\\.csv|\\.txt)*"][1]
      in.file <- as.data.frame(in.file)
      
      in.file.xlsx <- as.data.table(input$nm.file.bulk.input)[name %like% "\\.xlsx$"]
      
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
        dt.coef <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
      } else if (nm.sep() == ",") {
        dt.coef <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
      } else if (nm.sep() == "tab") {
        dt.coef <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
      }
      
      setDT(dt.coef)
      
      cln <- colnames(dt.coef)
      setnames(dt.coef, cln, str_replace(cln, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0x2e), as.raw(0xbf)))), ""))
      
      validate(
        
        need(is.data.frame(dt.coef), "Your file doesn't look like a stoich. coefficients file") %then%
          need(dt.coef[1, 1][!(dt.coef[1, 1] %like% "[a-zA-Z]")], "Your file doesn't look like a stoich. coefficients file") %then%
          need(nrow(dt.coef) + ncol(dt.coef) == length(unique(c(colnames(dt.coef), dt.coef$name))), "Duplicate component names")
        
      )
      
      # dt.coef <- as.data.table(dt.coef)
      
      tmp <- colnames(dt.coef)
      updateTextInput(session, "nm.part.names", value = paste(tmp[1:(length(tmp) - 1)], collapse = ", "))
      
      
    } else if (!is.null(in.file.xlsx)) {
      
      shts <- getSheetNames(in.file.xlsx$datapath)
      
      shts <- shts[shts %like% "^(input_|output_)*stoich_coefficients"]
      shts <- sort(shts)
      
      dt.coef <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1]), silent = TRUE)
      
      validate(
        
        need(is.data.frame(dt.coef), "Your file doesn't look like a stoich. coefficients file") %then%
          need(dt.coef[1, 1][!(dt.coef[1, 1] %like% "[a-zA-Z]")], "Your file doesn't look like a stoich. coefficients file") %then%
          need(nrow(dt.coef) + ncol(dt.coef) == length(unique(c(colnames(dt.coef), dt.coef$name))), "Duplicate component names")
        
      )
      
      # dt.coef <- as.data.table(dt.coef)
      
      tmp <- colnames(dt.coef)
      updateTextInput(session, "nm.part.names", value = paste(tmp[1:(length(tmp) - 1)], collapse = ", "))
      
    } else {
      
      dt.coef <- nm.dt.coef.data()
      
    }
    
    setnames(dt.coef, c(nm.part.names.data()[1:(ncol(dt.coef) - 1)], "name"))
    
    if (!is.null(dt.coef))
      rhandsontable(dt.coef, stretchH = "all", useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    
  })
  
  output$nm.dt.conc <- renderRHandsontable({
    
    in.file <- input$file.nm.dt.conc
    in.file.bulk <- input$nm.file.bulk.input
    in.file.xlsx <- NULL
    
    # bulk input
    
    if (input.source$nm.dt.conc.bulk) {
      
      in.file <- as.data.table(input$nm.file.bulk.input)[name %like% "^(input\\_)*concentrations(\\.csv|\\.txt)*$"][1]
      in.file <- as.data.frame(in.file)
      
      in.file.xlsx <- as.data.table(input$nm.file.bulk.input)[name %like% "\\.xlsx$"]
      
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
        dt.conc <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1, check.names = FALSE), silent = TRUE)
      } else if (nm.sep() == ",") {
        dt.conc <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1, check.names = FALSE), silent = TRUE)
      } else if (nm.sep() == "tab") {
        dt.conc <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1, check.names = FALSE), silent = TRUE)
      }
      
      setDT(dt.conc)
      
      cln <- colnames(dt.conc)
      setnames(dt.conc, cln, str_replace(cln, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0x2e), as.raw(0xbf)))), ""))
      
      validate(need(is.data.frame(dt.conc), "Check the column delimiter or content of your file"))
      
      tmp <- colnames(dt.conc)
      updateTextInput(session, "nm.part.names", value = paste(tmp, collapse = ", "))
      
      
    } else if (!is.null(in.file.xlsx)) {
      
      shts <- getSheetNames(in.file.xlsx$datapath)
      
      shts <- shts[shts %like% "^(input_|output_)*concentrations"]
      shts <- sort(shts)
      
      dt.conc <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1], startRow = 2), silent = TRUE)
      
      validate(need(is.data.frame(dt.conc), "Check the column delimiter or content of your file"))
      
      tmp <- colnames(dt.conc)
      updateTextInput(session, "nm.part.names", value = paste(tmp, collapse = ", "))
      
    } else {
      
      dt.conc <- nm.dt.conc.data()
      
    }
    
    setnames(dt.conc, nm.part.names.data()[1:ncol(dt.conc)])
    
    if (!is.null(dt.conc))
      rhandsontable(dt.conc, stretchH = "all", useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    
  })
  
  output$nm.part.eq <- renderRHandsontable({
    
    in.file <- input$file.nm.dt.conc
    in.file.bulk <- input$nm.file.bulk.input
    in.file.xlsx <- NULL
    
    # bulk input
    
    if (input.source$nm.dt.conc.bulk) {
      
      in.file <- as.data.table(input$nm.file.bulk.input)[name %like% "^(input\\_)*concentrations(\\.csv|\\.txt)*$"][1]
      in.file <- as.data.frame(in.file)
      
      in.file.xlsx <- as.data.table(input$nm.file.bulk.input)[name %like% "\\.xlsx$"]
      
      if (nrow(in.file.xlsx) > 0) {
        
        in.file.xlsx <- as.data.frame(in.file.xlsx[1])
        
      } else {
        
        in.file.xlsx <- NULL
        
      }
      
      if (!is.null(in.file.xlsx))
        in.file <- NULL
      
    }
    
    # choose source
    
    part.eq <- nm.part.eq.data()
    
    if (!is.null(in.file)) {
      
      
      if (nm.sep() == ";") {
        
        part.eq <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", nrows = 1, header = FALSE), silent = TRUE)
        tmp <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1, header = FALSE)[1, ], silent = TRUE)
        
      } else if (nm.sep() == ",") {
        
        part.eq <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", nrows = 1, header = FALSE), silent = TRUE)
        tmp <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1, header = FALSE)[1, ], silent = TRUE)
        
      } else if (nm.sep() == "tab") {
        
        part.eq <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", nrows = 1, header = FALSE), silent = TRUE)
        tmp <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", skip = 1, header = FALSE)[1, ], silent = TRUE)
        
      }
      
      setDT(part.eq)
      part.eq[1, V1 := str_replace(V1, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0xbb), as.raw(0xbf)))), "")]
      
      validate(
        
        need(is.data.frame(part.eq), "Check the column delimiter or content of your file") %then%
          need(ncol(part.eq) == ncol(tmp), "Check the column delimiter or content of your file")
        
      )
      
      colnames(part.eq) <- unlist(tmp)
      
    } else if (!is.null(in.file.xlsx)) {
      
      shts <- getSheetNames(in.file.xlsx$datapath)
      
      shts <- shts[shts %like% "^(input_|output_)*concentrations"]
      shts <- sort(shts)
      
      part.eq <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1], colNames = FALSE, rows = 1), silent = TRUE)
      tmp <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1], colNames = FALSE, rows = 2), silent = TRUE)
      
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
  
  output$nm.cnst <- renderRHandsontable({
    
    in.file <- input$file.nm.cnst
    in.file.bulk <- input$nm.file.bulk.input
    in.file.xlsx <- NULL
    
    # bulk input
    
    if (input.source$nm.cnst.bulk) {
      
      in.file <- as.data.table(input$nm.file.bulk.input)[name %like% "^(input\\_)*k\\_constants\\_log10(\\.csv|\\.txt)*$"][1]
      in.file <- as.data.frame(in.file)
      
      in.file.xlsx <- as.data.table(input$nm.file.bulk.input)[name %like% "\\.xlsx$"]
      
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
        cnst <- try(read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
      } else if (nm.sep() == ",") {
        cnst <- try(read.csv(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
      } else if (nm.sep() == "tab") {
        cnst <- try(read.delim(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE), silent = TRUE)
      }
      
      setDT(cnst)
      
      cln <- colnames(cnst)
      setnames(cnst, cln, str_replace(cln, paste0("^", rawToChar(c(as.raw(0xef), as.raw(0x2e), as.raw(0xbf)))), ""))
      
      validate(
        need(is.data.frame(cnst), "Check the column delimiter or content of your file") %then%
          need(length(colnames(cnst)[colnames(cnst) %like% "^Constant$|^k_constants_log10$|^cnst$"]) == 1
               , "Check the column delimiter or content of your file")
      )
      
    } else if (!is.null(in.file.xlsx)) {
      
      shts <- getSheetNames(in.file.xlsx$datapath)
      
      shts <- shts[shts %like% "^((input_)*k_constants_log10|constants_evaluated)"]
      shts <- sort(shts, decreasing = TRUE)
      
      cnst <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1]), silent = TRUE)
      
      validate(
        need(is.data.frame(cnst), "Check the column delimiter or content of your file") %then%
          need(length(colnames(cnst)[colnames(cnst) %like% "^Constant$|^k_constants_log10$|^cnst$"]) == 1
               , "Check the column delimiter or content of your file")
      )
      
    } else {
      
      cnst <- nm.cnst.data()
      
    }
    
    setDT(cnst)
    
    cln <- colnames(cnst)
    cln <- cln[cln %like% "^Constant$|^k_constants_log10$|^cnst$"]
    
    cnst <- cnst[, cln, with = FALSE]
    
    if (!is.null(cnst))
      rhandsontable(cnst, stretchH = "all", useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    
  })
  
  output$dt.nm <- renderRHandsontable({
    
    in.file <- input$file.dt.nm
    in.file.bulk <- input$nm.file.bulk.input
    in.file.xlsx <- NULL
    
    # bulk input
    
    if (input.source$dt.nm.bulk) {
      
      in.file <- as.data.table(input$nm.file.bulk.input)[name %like% "^(input\\_)*chemical\\_shifts*(\\.csv|\\.txt)*$"][1]
      in.file <- as.data.frame(in.file)
      
      in.file.xlsx <- as.data.table(input$nm.file.bulk.input)[name %like% "\\.xlsx$"]
      
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
    in.file.bulk <- input$nm.file.bulk.input
    in.file.xlsx <- NULL
    
    # bulk input
    
    if (input.source$nm.dt.ind.bulk) {
      
      in.file <- as.data.table(input$nm.file.bulk.input)[name %like% "^(input\\_)*ind(ividual)*\\_(chemical\\_)*shifts*(\\.csv|\\.txt)*$"][1]
      in.file <- as.data.frame(in.file)
      
      in.file.xlsx <- as.data.table(input$nm.file.bulk.input)[name %like% "\\.xlsx$"]
      
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
  
  output$nm.dt.res <- renderRHandsontable({
    
    dt.res <- nm.dt.res.data()
    
    if (!is.null(dt.res)) {
      
      renderer <- "
      function (instance, td, row, col, prop, value, cellProperties) {
      
      Handsontable.renderers.TextRenderer.apply(this, arguments);
      
      if (parseInt(value, 10) < 0) {
      td.style.background = 'pink';
      }
      
      }" 

      rhandsontable(dt.res, stretchH = FALSE, useTypes = FALSE) %>%
        hot_cols(renderer = renderer)
    }
    
})
  
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
  
  output$nm.cnst.dev <- renderRHandsontable({
    
    cnst.dev <- nm.cnst.dev.data()
    
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
  
  output$nm.cor.m <- renderRHandsontable({
    
    cor.m <- nm.cor.m.data()
    
    if (!is.null(cor.m))
      
      rhandsontable(cor.m, stretchH = FALSE, useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    
  })
  
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
  
  output$nm.adj.r.squared <- renderRHandsontable({
    
    nm.adj.r.squared <- nm.adj.r.squared.data()
    
    if (!is.null(nm.adj.r.squared))
      
      rhandsontable(nm.adj.r.squared, stretchH = FALSE, useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    
  })
  
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
  

  
  # curve fitting -------------------------------------
  
  
  # technical
  
  cur.sep <- reactive({
    
    switch(input$cur.sep,
           comma = ",",
           semicolon = ";",
           tab = "tab")
    
  })
  
  cur.curves.iterator <- reactiveVal(as.integer(1))
  
  cur.curve.rows <- reactiveValues(values = character())
  
  # cur.formula.values <- reactiveValues()
  
  
  # controls ----------------- #
  
  cur.curve.gaussian <- function(fn.type.id, btn.id, cur.id, fn.id, cur.params = NULL) {
    
    if (is.null(cur.params))
      cur.params <- list(amplitude = 0
                         , expvalue = 0
                         , hwhm = 0)
    
    cur.curve.ui <- fluidRow(column(2
                                    , h4("Gaussian")
                                    , class = "kev-densed-input-row")
                             , column(2
                                      , textInput(inputId = paste0(fn.type.id, fn.id, "_name")
                                                  , label = "Name"
                                                  , value = paste("Curve", fn.id))
                                      , class = "kev-densed-input-row")
                             , column(2
                                      , textInput(inputId = paste0(fn.type.id, fn.id, "_amplitude")
                                                  , label = "Amplitude"
                                                  , value = cur.params$amplitude)
                                      , class = "kev-densed-input-row")
                             , column(2
                                      , textInput(inputId = paste0(fn.type.id, fn.id, "_expvalue")
                                                  , label = "Exp.value"
                                                  , value = cur.params$expvalue)
                                      , class = "kev-densed-input-row")
                             , column(2
                                      , textInput(inputId = paste0(fn.type.id, fn.id, "_hwhm")
                                                  , label = "HWHM"
                                                  , value = cur.params$hwhm)
                                      , class = "kev-densed-input-row")
                             , column(2
                                      , actionButton(btn.id
                                                     , ""
                                                     , icon = icon("trash")
                                                     , style = "margin-top: 25px;"))
                             , id = cur.id)
    
    cur.curve.ui

  }
  

  cur.curve.lorentzian <- function(fn.type.id, btn.id, cur.id, fn.id, cur.params = NULL) {
    
    if (is.null(cur.params))
      cur.params <- list(amplitude = 0
                         , expvalue = 0
                         , hwhm = 0)
    
    cur.curve.ui <- fluidRow(column(2
                                    , h4("Lorentzian")
                                    , class = "kev-densed-input-row")
                             , column(2
                                      , textInput(inputId = paste0(fn.type.id, fn.id, "_name")
                                                  , label = "Name"
                                                  , value = paste("Curve", fn.id))
                                      , class = "kev-densed-input-row")
                             , column(2
                                      , textInput(inputId = paste0(fn.type.id, fn.id, "_amplitude")
                                                  , label = "Amplitude"
                                                  , value = cur.params$amplitude)
                                      , class = "kev-densed-input-row")
                             , column(2
                                      , textInput(inputId = paste0(fn.type.id, fn.id, "_expvalue")
                                                  , label = "Exp.value"
                                                  , value = cur.params$expvalue)
                                      , class = "kev-densed-input-row")
                             , column(2
                                      , textInput(inputId = paste0(fn.type.id, fn.id, "_hwhm")
                                                  , label = "HWHM"
                                                  , value = cur.params$hwhm)
                                      , class = "kev-densed-input-row")
                             , column(2
                                      , actionButton(btn.id
                                                     , ""
                                                     , icon = icon("trash")
                                                     , style = "margin-top: 25px;"))
                             , id = cur.id)
    
    cur.curve.ui
    
  }
  

  cur.curve.insert <- function(fn.type.id, cur.params = NULL, fn.id = NULL) {
    
    cur.itr <- cur.curves.iterator()
    if (is.null(fn.id)) fn.id <- cur.itr
    
    cur.id <- paste0(fn.type.id, fn.id, "_curve.row")
    btn.id <- paste0(fn.type.id, fn.id, "_remove.btn")
    
    if (str_to_lower(fn.type.id) == "gaussian") {
      
      cur.curve.ui <- cur.curve.gaussian(fn.type.id, btn.id, cur.id, fn.id, cur.params)
      
    } else if (str_to_lower(fn.type.id) == "lorentzian") {
      
      cur.curve.ui <- cur.curve.lorentzian(fn.type.id, btn.id, cur.id, fn.id, cur.params)
      
    }
    
    cur.curves.iterator(cur.itr + as.integer(1))
    cur.curve.rows$values <- c(cur.curve.rows$values, cur.id)
    
    insertUI(selector = "#cur_new_curves_place"
             , where = "beforeBegin"
             , ui = cur.curve.ui
    )
    
    observeEvent(input[[btn.id]], {
      
      removeUI(paste0("#", escapeRegex(str_replace(btn.id, "\\_remove\\.btn", "_curve.row"))))
      
    })
      
  }
  
  
  #
  
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
  
  
  # data --------------------- #
  
  # input data
  
  cur.dt.init.data <- reactive({
    
    if (is.null(values$cur.dt.init)){
      
      set.seed(1)
      
      values$cur.dt.init <- data.table(label = 1:121, value = dnorm(seq(-3, 3, .05)) * (1 + rnorm(121, sd = 1e-2)))
      cur.dt.par.data()
      
    }

    values$cur.dt.init
    
  })

  cur.dt.par.data <- reactive({
    
    if (is.null(values$cur.dt.par))
      values$cur.dt.par <- data.table(name = "", design = "", param = "task", value = input$cur.task)

    values$cur.dt.par
    
  })
  
  observeEvent({
    input$file.cur.bulk.input 
    input$cur.sep
    }, {
  
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
    
    validate(
      
      need(is.data.frame(dt.init), "Your file doesn't look like a data file (table-like)") %then%
        need(ncol(dt.init) == 2
             , paste("The data file for curve fitting should contain exactly 1 column `label` for labels"
                     , "and 1 column `value` for observed values"))
      
    )
    
    if (!is.null(dt.init)) {
      
      setDT(dt.init)
      values[["cur.dt.init"]] <- dt.init
      
    }
      
    
    # load params ----- #
    
    in.file.bulk <- input$file.cur.bulk.input
    in.file.xlsx <- NULL
    in.file <- NULL
    
    # bulk input
    
    if (!is.null(in.file.bulk)) {
      
      in.file <- as.data.table(in.file.bulk)[name %like% "^(input\\_)*params(\\.csv|\\.txt)*"][1]
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
      
      shts <- shts[shts %like% "^(input_|output_)*params*"]
      shts <- sort(shts)
      
      dt.par <- try(read.xlsx(in.file.xlsx$datapath, sheet = shts[1]), silent = TRUE)
      
    } else {
      
      dt.par <- NULL
      
    }
    
    validate(
      
      need(is.null(dt.par) | is.data.frame(dt.par), paste("Params file exists but is not properly formatted"
                                                          , "Check examples via `Check Examples` button")) %then%
        need(is.null(dt.par) | ncol(dt.par) > 1, paste("Params file should contain > 1 columns. Check column delimiter"))
    )
    
    if (!is.null(dt.par)) {
      
      setDT(dt.par)
      values[["cur.dt.par"]] <- dt.par
      
    }
    
  }
  , ignoreInit = TRUE)
  

  # calculating -------------- #
  
  observeEvent(values$cur.dt.par, {
    
    values$cur.status <- cur.data.runner(mode = "app"
                                        , sep = cur.sep()
                                        , subdir = ""
                                        , file = NULL
                                        , save.res = FALSE
                                        , dt.list = list(dt.cur = cur.dt.init.data()
                                                         , dt.par = values$cur.dt.par))

    
    names <- values$cur.status@dt.par[!is.na(design) & design != "", name] %>% unique()
    
    if (length(names) > 0) {
      
      if (length(cur.curve.rows$values) > 0) {
        
        for (cr in cur.curve.rows$values) {
          
          removeUI(paste0("#", escapeRegex(cr)))
          cur.curve.rows$values <- setdiff(cur.curve.rows$values, cr)
          
        }
      }
      
      for (nm in names) {

        params <- as.list(values$cur.status@dt.par[name == nm, value])
        names(params) <- values$cur.status@dt.par[name == nm, param]
        
        cur.curve.insert(values$cur.status@dt.par[name == nm, design][1]
                         , params
                         , fn.id = nm)
        
      }
      
    }
    
    
    
  }, ignoreNULL = FALSE)
  
  observeEvent(input$cur.exec.btn, {
    
    values$cur.status <- cur.model(values$cur.status)
    
  })
  
  
  # rendering ---------------- #
  
  output$cur.dt.init <- renderRHandsontable({
    
    dt.init <- cur.dt.init.data()

    if (!is.null(dt.init)) {
      
      if (nrow(dt.init) > 25) {
        
        rhandsontable(dt.init, stretchH = "all", useTypes = FALSE, height = 600) %>%
          hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
        
      } else {
        
        rhandsontable(dt.init, stretchH = "all", useTypes = FALSE, height = NULL) %>%
          hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
        
      }
      
    }

  })
  
  output$plot.cur <- renderPlotly({
    
    validate(
      
      need(!is.null(values$cur.status), "Error loading or creating curves parameters")
      
    ) 
    
    dt <- values$cur.status@dt.init
    frm <- cur.formula.create(values$cur.status@dt.par, dt)
    
    extr.effects <- cur.formula.effects(dt, frm$formula, frm$start.values)
    
    cln <- colnames(extr.effects)
    cln <- cln[cln %like% "^(Curve .*|label)$"]
    
    lbl.perc <- (dt[, max(label)] - dt[, min(label)]) / 100
    
    g <-
      ggplot() +
      geom_area(data = extr.effects, aes(x = label, y = observed, group = 1), color = "darkgrey", size = 1, fill = "grey") +
      geom_line(data = extr.effects, aes(x = label, y = predicted, group = 1), color = "darkblue", size = 1, linetype = 2) +
      geom_line(data = melt(extr.effects[, cln, with = FALSE], id.vars = "label", variable.name = "Curves")
                , aes(x = label, y = value, group = Curves, color = Curves)) +
      geom_rect(aes(xmin = dt[, min(label)] - 2 * lbl.perc, xmax = values$cur.status@window.borders[1]
                    , ymin = 0, ymax = dt[, max(value) * 1.1]), alpha = .1) +
      geom_rect(aes(xmin = values$cur.status@window.borders[2], xmax = dt[, max(label)] + 2 * lbl.perc
                    , ymin = 0, ymax = dt[, max(value) * 1.1]), alpha = .1) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      theme(legend.title = element_blank()) +
      labs(x = "Labels", y = "Values")
    
    g <- ggplotly(g)

    g
    
  })
  
  
  # end of main server part ----------------------------
  
  
  # equilibrium download ---------------- #
  
  output$dt.coef.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "input_stoichiometric_coefficients.csv"
      
    },
    
    content = function(file) {
      
      if (eq.sep() == ";") {
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
      
      if (eq.sep() == ";") {
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
      
      if (eq.sep() == ";") {
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

      if (eq.sep() == ";") {
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
      
      if (eq.sep() == ";") {
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
      
      if (eq.sep() == ";") {
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
      
      if (eq.sep() == ";") {
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
  
  output$kev.eq.data.zip <- downloadHandler(
    # ----
    filename = function() {
      
      "kev.concentrations.data.zip"
      
    },
    
    content = function(file) {
      
      data.files <- c(
        
        dt.coef = "input_stoichiometric_coefficients.csv"
        , cnst = "input_k_constants_log10.csv"
        , dt.conc = "input_concentrations.csv"
        , dt.conc.tot = "total_concentrations.csv"
        , dt.res = "equilibrium_concentrations.csv"
        , dt.frac = paste0(bs.name.data(), "_fractions.csv")
        , dt.err = "percent_error.csv"
        , bs.name = "component_names.csv"


      )
      
      # temporary directory to avoid permission issues
      
      curdir <- getwd()
      tmpdir <- tempdir()
      setwd(tmpdir)
      print(tempdir())
      
      for (i in length(data.files):1) {
        
        # check if all files are present (in case run before evaluation)
        
        dt <- NULL
        try(dt <- eval(expr = parse(text = paste0(names(data.files)[i], ".data()"))), silent = TRUE)
        
        if (eq.sep() == ";") {
          
          if (!is.null(dt)) {
            
            if (data.files[i] == "input_concentrations.csv") {
              
              dt <- dt.conc.data()
              dt <- rbind(data.table(t(data.table(colnames(dt)))), dt, use.names = FALSE)
              
              setnames(dt, unlist(part.eq.data()))
              
            }
            
            if ((data.files[i] %like% "(particle|component)_names(\\.csv|\\.txt)$")) {
              
              write.table(dt, data.files[i], sep = ";", dec = ",", row.names = FALSE, col.names = FALSE)
              
            } else {
             
              write.csv2(dt, data.files[i], row.names = FALSE)
               
            }
            
          } else {
            
            data.files <- data.files[-i]
            
          }
          
        } else {
          
          if (!is.null(dt)) {
            
            if (data.files[i] == "input_concentrations.csv") {
              
              dt <- dt.conc.data()
              dt <- rbind(data.table(t(data.table(colnames(dt)))), dt, use.names = FALSE)
              
              setnames(dt, unlist(part.eq.data()))
              
            }
            
            if ((data.files[i] %like% "(particle|component)_names(\\.csv|\\.txt)$")) {
              
              write.table(dt, data.files[i], sep = ",", dec = ".", row.names = FALSE, col.names = FALSE)
              
            } else {
              
              write.csv(dt, data.files[i], row.names = FALSE)
              
            }
            
          } else {
            
            data.files <- data.files[-i]
            
          }
        }
        
      }
      
      # create zip
      
      utils::zip(file, data.files)
      
      # remove garbage from the disc
      
      for (i in data.files) {
        
        if (file.exists(i))
          file.remove(i)
        
      }
        
      setwd(curdir)
      
    }
    
  )
  # ----
  
  output$kev.eq.data.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "kev.concentrations.data.xlsx"
      
    },
    
    content = function(file) {
      
      data.files <- c(
        
        dt.coef = "input_stoich_coefficients"
        , cnst = "input_k_constants_log10"
        , dt.conc = "input_concentrations"
        , dt.conc.tot = "total_concentrations"
        , dt.res = "equilibrium_concentrations"
        , dt.frac = paste0(bs.name.data(), "_fractions")
        , dt.err = "percent_error"
        , bs.name = "component_names"
        
      )
      
      dt.list <- list()
      
      for (i in 1:length(data.files)) {
        
        # check if all files are present (in case run before evaluation)
        
        dt <- NULL
        try(dt <- eval(expr = parse(text = paste0(names(data.files)[i], ".data()"))), silent = TRUE)
        
        if (!is.null(dt)) {
          
          if (data.files[i] == "input_concentrations") {
            
            dt <- dt.conc.data()
            dt <- rbind(data.table(t(data.table(colnames(dt)))), dt, use.names = FALSE)
            
            setnames(dt, unlist(part.eq.data()))
            
          }
          
          dt.list[[eval(data.files[i])]] <- dt
          
        }
        
      }
      
      write.xlsx(dt.list, file)
      
    }
    
  )
  # ----
  

  # absorbance download ---------------- #
  
  output$ab.dt.coef.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "input_stoichiometric_coefficients.csv"
      
    },
    
    content = function(file) {
      
      if (ab.sep() == ";") {
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
      
      if (ab.sep() == ";") {
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
      
      if (ab.sep() == ";") {
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
      
      if (ab.sep() == ";") {
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
      
      if (ab.sep() == ";") {
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
      
      if (ab.sep() == ";") {
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
      
      if (ab.sep() == ";") {
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
      
      if (ab.sep() == ";") {
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
      
      if (ab.sep() == ";") {
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
      
      if (ab.sep() == ";") {
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
  
  output$ab.adj.r.squared.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "adj_r_squared.csv"
      
    },
    
    content = function(file) {
      
      if (ab.sep() == ";") {
        write.csv2(ab.adj.r.squared.data(), file, row.names = FALSE)
      } else {
        write.csv(ab.adj.r.squared.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$ab.adj.r.squared.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "adj_r_squared.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(ab.adj.r.squared.data(), file)
      
    }
    
  )
  # ----
  
  output$mol.coef.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "mol_ext_coefficients_calculated.csv"
      
    },
    
    content = function(file) {
      
      if (ab.sep() == ";") {
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
        , ab.adj.r.squared = "adj_r_squared.csv"
        , mol.coef = "mol_ext_coefficients_calculated.csv"
        , target = "target.csv"
        
      )
      
      # temporary directory to avoid permission issues
      
      curdir <- getwd()
      tmpdir <- tempdir()
      setwd(tmpdir)
      print(tempdir())
      
      
      for (i in length(data.files):1) {
        
        # check if all files are present (in case run before evaluation)
        
        dt <- NULL
        try(dt <- eval(expr = parse(text = paste0(names(data.files)[i], ".data()"))), silent = TRUE)
        
        if (ab.sep() == ";") {

          if (!is.null(dt)) {
            
            if (data.files[i] == "input_concentrations.csv") {
              
              dt <- ab.dt.conc.data()
              dt <- rbind(data.table(t(data.table(colnames(dt)))), dt, use.names = FALSE)
              
              setnames(dt, unlist(ab.part.eq.data()))
              
            }
            
            write.csv2(dt, data.files[i], row.names = FALSE)
            
          } else {
            
            data.files <- data.files[-i]
            
          }
          
        } else {
          
          if (!is.null(dt)) {
            
            if (data.files[i] == "input_concentrations.csv") {
              
              dt <- ab.dt.conc.data()
              dt <- rbind(data.table(t(data.table(colnames(dt)))), dt, use.names = FALSE)
              
              setnames(dt, unlist(ab.part.eq.data()))
              
            }
            
            write.csv(dt, data.files[i], row.names = FALSE)
            
          } else {
            
            data.files <- data.files[-i]
            
          }
        }
        
      }
      
      # create zip
      
      utils::zip(file, data.files)
      
      # remove garbage from the disc
      
      for (i in data.files) {
      
        if (file.exists(i))
          file.remove(i)

      }
      
      setwd(curdir)

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
        , target = "target"
        , ab.dt.res = "equilibrium_concentrations"
        , dt.ab.abs = "absorbance_calc_abs_errors"
        , dt.ab.rel = "absorbance_calc_rel_errors"
        , cnst.dev = "constants_evaluated"
        , cor.m = "correlation_matrix"
        , ab.adj.r.squared = "adj_r_squared"
        , mol.coef = "mol_ext_coefficients_calc"
        
      )
      
      dt.list <- list()
      
      for (i in 1:length(data.files)) {
        
        # check if all files are present (in case run before evaluation)
        
        dt <- NULL
        try(dt <- eval(expr = parse(text = paste0(names(data.files)[i], ".data()"))), silent = TRUE)
        
        if (!is.null(dt)) {
          
          if (data.files[i] == "input_concentrations") {
            
            dt <- ab.dt.conc.data()
            dt <- rbind(data.table(t(data.table(colnames(dt)))), dt, use.names = FALSE)
            
            setnames(dt, unlist(ab.part.eq.data()))
            
          }
          
          dt.list[[eval(data.files[i])]] <- dt
          
        }
        
      }
      
      write.xlsx(dt.list, file)
      
    }
    
  )
  # ----
  
  
  # extinction coefficients downoad ---------------- #
  
  output$sp.dt.mol.full.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "molar_extinction_coefficients.csv"
      
    },
    
    content = function(file) {
      
      if (sp.sep() == ";") {
        write.csv2(sp.dt.mol.full.data(), file, row.names = FALSE)
      } else {
        write.csv(sp.dt.mol.full.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$sp.dt.mol.full.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "molar_extinction_coefficients.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(sp.dt.mol.full.data(), file)
      
    }
    
  )
  # ----

  
  # emf download ---------------- #
  
  output$emf.dt.coef.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "input_stoichiometric_coefficients.csv"
      
    },
    
    content = function(file) {
      
      if (emf.sep() == ";") {
        write.csv2(emf.dt.coef.data(), file, row.names = FALSE)
      } else {
        write.csv(emf.dt.coef.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$emf.dt.coef.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "input_stoichiometric_coefficients.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(emf.dt.coef.data(), file)
      
    }
    
  )
  # ----
  
  output$emf.cnst.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "input_k_constants_log10.csv"
      
    },
    
    content = function(file) {
      
      if (emf.sep() == ";") {
        write.csv2(emf.cnst.data(), file, row.names = FALSE)
      } else {
        write.csv(emf.cnst.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$emf.cnst.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "input_k_constants_log10.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(emf.cnst.data(), file)
      
    }
    
  )
  # ----
  
  output$emf.dt.conc.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "input_concentrations.csv"
      
    },
    
    content = function(file) {
      
      tmp <- emf.dt.conc.data()
      tmp <- rbind(data.table(t(data.table(colnames(tmp)))), tmp, use.names = FALSE)
      
      setnames(tmp, unlist(emf.part.eq.data()))
      
      if (emf.sep() == ";") {
        write.csv2(tmp, file, row.names = FALSE)
      } else {
        write.csv(tmp, file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$emf.dt.conc.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "input_concentrations.xlsx"
      
    },
    
    content = function(file) {
      
      tmp <- emf.dt.conc.data()
      tmp <- rbind(data.table(t(data.table(colnames(tmp)))), tmp, use.names = FALSE)
      
      setnames(tmp, unlist(emf.part.eq.data()))
      
      write.xlsx(tmp, file)
      
    }
    
  )
  # ----
  
  output$dt.emf.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "input_emf.csv"
      
    },
    
    content = function(file) {
      
      if (emf.sep() == ";") {
        write.csv2(dt.emf.data(), file, row.names = FALSE)
      } else {
        write.csv(dt.emf.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$dt.emf.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "input_emf.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(dt.emf.data(), file)
      
    }
    
  )
  # ----
  
  output$emf.target.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "targets.csv"
      
    },
    
    content = function(file) {
      
      if (emf.sep() == ";") {
        write.csv2(emf.target.data(), file, row.names = FALSE)
      } else {
        write.csv(emf.target.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$emf.target.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "targets.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(emf.target.data(), file)
      
    }
    
  )
  # ----
  
  output$emf.dt.res.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "equilibrium_concentrations.csv"
      
    },
    
    content = function(file) {
      
      if (emf.sep() == ";") {
        write.csv2(emf.dt.res.data(), file, row.names = FALSE)
      } else {
        write.csv(emf.dt.res.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$emf.dt.res.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "equilibrium_concentrations.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(emf.dt.res.data(), file)
      
    }
    
  )
  # ----
  
  output$dt.emf.abs.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "emf_calculated_abs_errors.csv"
      
    },
    
    content = function(file) {
      
      if (emf.sep() == ";") {
        write.csv2(dt.emf.abs.data(), file, row.names = FALSE)
      } else {
        write.csv(dt.emf.abs.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$dt.emf.abs.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "emf_calculated_abs_errors.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(dt.emf.abs.data(), file)
      
    }
    
  )
  # ----
  
  output$dt.emf.rel.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "emf_calculated_rel_errors.csv"
      
    },
    
    content = function(file) {
      
      if (emf.sep() == ";") {
        write.csv2(dt.emf.rel.data(), file, row.names = FALSE)
      } else {
        write.csv(dt.emf.rel.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$dt.emf.rel.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "emf_calculated_rel_errors.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(dt.emf.rel.data(), file)
      
    }
    
  )
  # ----
  
  output$emf.cnst.dev.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "constants_evaluated.csv"
      
    },
    
    content = function(file) {
      
      if (emf.sep() == ";") {
        write.csv2(emf.cnst.dev.data(), file, row.names = FALSE)
      } else {
        write.csv(emf.cnst.dev.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$emf.cnst.dev.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "constants_evaluated.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(emf.cnst.dev.data(), file)
      
    }
    
  )
  # ----
  
  output$emf.cor.m.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "correlation_matrix.csv"
      
    },
    
    content = function(file) {
      
      if (emf.sep() == ";") {
        write.csv2(emf.cor.m.data(), file, row.names = FALSE)
      } else {
        write.csv(emf.cor.m.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$emf.cor.m.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "correlation_matrix.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(emf.cor.m.data(), file)
      
    }
    
  )
  # ----
  
  output$emf.adj.r.squared.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "adj_r_squared.csv"
      
    },
    
    content = function(file) {
      
      if (emf.sep() == ";") {
        write.csv2(emf.adj.r.squared.data(), file, row.names = FALSE)
      } else {
        write.csv(emf.adj.r.squared.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$emf.adj.r.squared.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "adj_r_squared.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(emf.adj.r.squared.data(), file)
      
    }
    
  )
  # ----
  
  output$kev.emf.data.zip <- downloadHandler(
    # ----
    filename = function() {
      
      "kev.emf.constants.data.zip"
      
    },
    
    content = function(file) {
      
      data.files <- c(
        
        emf.dt.coef = "input_stoichiometric_coefficients.csv"
        , emf.cnst = "input_k_constants_log10.csv"
        , emf.dt.conc = "input_concentrations.csv"
        , dt.emf = "input_emf.csv"
        , emf.dt.res = "equilibrium_concentrations.csv"
        , dt.emf.abs = "emf_calculated_abs_errors.csv"
        , dt.emf.rel = "emf_calculated_rel_errors.csv"
        , emf.cnst.dev = "constants_evaluated.csv"
        , emf.cor.m = "correlation_matrix.csv"
        , emf.adj.r.squared = "adj_r_squared.csv"
        , emf.target = "target.csv"
        
      )
      
      # temporary directory to avoid permission issues
      
      curdir <- getwd()
      tmpdir <- tempdir()
      setwd(tmpdir)
      print(tempdir())
      
      for (i in length(data.files):1) {
        
        # check if all files are present (in case run before evaluation)
        
        dt <- NULL
        try(dt <- eval(expr = parse(text = paste0(names(data.files)[i], ".data()"))), silent = TRUE)
        
        if (emf.sep() == ";") {
          
          if (!is.null(dt)) {
            
            if (data.files[i] == "input_concentrations.csv") {
              
              dt <- emf.dt.conc.data()
              dt <- rbind(data.table(t(data.table(colnames(dt)))), dt, use.names = FALSE)
              
              setnames(dt, unlist(emf.part.eq.data()))
              
            }
            
            write.csv2(dt, data.files[i], row.names = FALSE)
            
          } else {
            
            data.files <- data.files[-i]
            
          }
          
        } else {
          
          if (!is.null(dt)) {
            
            if (data.files[i] == "input_concentrations.csv") {
              
              dt <- emf.dt.conc.data()
              dt <- rbind(data.table(t(data.table(colnames(dt)))), dt, use.names = FALSE)
              
              setnames(dt, unlist(emf.part.eq.data()))
              
            }
            
            write.csv(dt, data.files[i], row.names = FALSE)
            
          } else {
            
            data.files <- data.files[-i]
            
          }
        }
        
      }
      
      # create zip
      
      utils::zip(file, data.files)
      
      # remove garbage from the disc

      for (i in data.files) {
      
        if (file.exists(i))
          file.remove(i)
        
      }
      
      setwd(curdir)

    }
    
  )
  # ----
  
  output$kev.emf.data.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "kev.emf.constants.data.xlsx"
      
    },
    
    content = function(file) {
      
      data.files <- c(
        
        emf.dt.coef = "input_stoich_coefficients"
        , emf.cnst = "input_k_constants_log10"
        , emf.dt.conc = "input_concentrations"
        , dt.emf = "input_emf"
        , emf.target = "targets"
        , emf.dt.res = "equilibrium_concentrations"
        , dt.emf.abs = "emf_calc_abs_errors"
        , dt.emf.rel = "emf_calc_rel_errors"
        , emf.cnst.dev = "constants_evaluated"
        , emf.cor.m = "correlation_matrix"
        , emf.adj.r.squared = "adj_r_squared"

      )
      
      dt.list <- list()
      
      for (i in 1:length(data.files)) {
        
        # check if all files are present (in case run before evaluation)
        
        dt <- NULL
        try(dt <- eval(expr = parse(text = paste0(names(data.files)[i], ".data()"))), silent = TRUE)
        
        if (!is.null(dt)) {
          
          if (data.files[i] == "input_concentrations") {
            
            dt <- emf.dt.conc.data()
            dt <- rbind(data.table(t(data.table(colnames(dt)))), dt, use.names = FALSE)
            
            setnames(dt, unlist(emf.part.eq.data()))
            
          }
          
          dt.list[[eval(data.files[i])]] <- dt
          
        }
        
      }
      
      write.xlsx(dt.list, file)
      
    }
    
  )
  # ----
  
  
  # nmr (fast) download ---------------- #
  
  output$nm.dt.coef.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "input_stoichiometric_coefficients.csv"
      
    },
    
    content = function(file) {
      
      if (nm.sep() == ";") {
        write.csv2(nm.dt.coef.data(), file, row.names = FALSE)
      } else {
        write.csv(nm.dt.coef.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$nm.dt.coef.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "input_stoichiometric_coefficients.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(nm.dt.coef.data(), file)
      
    }
    
  )
  # ----
  
  output$nm.cnst.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "input_k_constants_log10.csv"
      
    },
    
    content = function(file) {
      
      if (nm.sep() == ";") {
        write.csv2(nm.cnst.data(), file, row.names = FALSE)
      } else {
        write.csv(nm.cnst.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$nm.cnst.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "input_k_constants_log10.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(nm.cnst.data(), file)
      
    }
    
  )
  # ----
  
  output$nm.dt.conc.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "input_concentrations.csv"
      
    },
    
    content = function(file) {
      
      tmp <- nm.dt.conc.data()
      tmp <- rbind(data.table(t(data.table(colnames(tmp)))), tmp, use.names = FALSE)
      
      setnames(tmp, unlist(nm.part.eq.data()))
      
      if (nm.sep() == ";") {
        write.csv2(tmp, file, row.names = FALSE)
      } else {
        write.csv(tmp, file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$nm.dt.conc.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "input_concentrations.xlsx"
      
    },
    
    content = function(file) {
      
      tmp <- nm.dt.conc.data()
      tmp <- rbind(data.table(t(data.table(colnames(tmp)))), tmp, use.names = FALSE)
      
      setnames(tmp, unlist(nm.part.eq.data()))
      
      write.xlsx(tmp, file)
      
    }
    
  )
  # ----
  
  output$dt.nm.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "input_chemical_shifts.csv"
      
    },
    
    content = function(file) {
      
      if (nm.sep() == ";") {
        write.csv2(dt.nm.data(), file, row.names = FALSE)
      } else {
        write.csv(dt.nm.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$dt.nm.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "input_chemical_shifts.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(dt.nm.data(), file)
      
    }
    
  )
  # ----
  
  output$nm.dt.ind.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "input_individual_shifts.csv"
      
    },
    
    content = function(file) {
      
      if (nm.sep() == ";") {
        write.csv2(nm.dt.ind.data(), file, row.names = FALSE)
      } else {
        write.csv(nm.dt.ind.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$nm.dt.ind.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "input_individual_shifts.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(nm.dt.ind.data(), file)
      
    }
    
  )
  # ----
  
  output$nm.dt.res.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "equilibrium_concentrations.csv"
      
    },
    
    content = function(file) {
      
      if (nm.sep() == ";") {
        write.csv2(nm.dt.res.data(), file, row.names = FALSE)
      } else {
        write.csv(nm.dt.res.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$nm.dt.res.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "equilibrium_concentrations.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(nm.dt.res.data(), file)
      
    }
    
  )
  # ----
  
  output$dt.nm.abs.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "chemical_shifts_calculated_abs_errors.csv"
      
    },
    
    content = function(file) {
      
      if (nm.sep() == ";") {
        write.csv2(dt.nm.abs.data(), file, row.names = FALSE)
      } else {
        write.csv(dt.nm.abs.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$dt.nm.abs.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "chemical_shifts_calculated_abs_errors.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(dt.nm.abs.data(), file)
      
    }
    
  )
  # ----
  
  output$dt.nm.rel.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "chemical_shifts_calculated_rel_errors.csv"
      
    },
    
    content = function(file) {
      
      if (nm.sep() == ";") {
        write.csv2(dt.nm.rel.data(), file, row.names = FALSE)
      } else {
        write.csv(dt.nm.rel.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$dt.nm.rel.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "chemical_shifts_calculated_rel_errors.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(dt.nm.rel.data(), file)
      
    }
    
  )
  # ----
  
  output$nm.cnst.dev.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "constants_evaluated.csv"
      
    },
    
    content = function(file) {
      
      if (nm.sep() == ";") {
        write.csv2(nm.cnst.dev.data(), file, row.names = FALSE)
      } else {
        write.csv(nm.cnst.dev.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$nm.cnst.dev.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "constants_evaluated.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(nm.cnst.dev.data(), file)
      
    }
    
  )
  # ----
  
  output$nm.cor.m.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "correlation_matrix.csv"
      
    },
    
    content = function(file) {
      
      if (nm.sep() == ";") {
        write.csv2(nm.cor.m.data(), file, row.names = FALSE)
      } else {
        write.csv(nm.cor.m.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$nm.cor.m.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "correlation_matrix.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(nm.cor.m.data(), file)
      
    }
    
  )
  # ----
  
  output$nm.adj.r.squared.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "adj_r_squared.csv"
      
    },
    
    content = function(file) {
      
      if (nm.sep() == ";") {
        write.csv2(nm.adj.r.squared.data(), file, row.names = FALSE)
      } else {
        write.csv(nm.adj.r.squared.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$nm.adj.r.squared.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "adj_r_squared.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(nm.adj.r.squared.data(), file)
      
    }
    
  )
  # ----
  
  output$nm.ind.shift.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "individual_shifts_calculated.csv"
      
    },
    
    content = function(file) {
      
      if (nm.sep() == ";") {
        write.csv2(nm.ind.shift.data(), file, row.names = FALSE)
      } else {
        write.csv(nm.ind.shift.data(), file, row.names = FALSE)
      }
      
    }
    
  )
  # ----
  
  output$nm.ind.shift.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "individual_shifts_calculated.xlsx"
      
    },
    
    content = function(file) {
      
      write.xlsx(nm.ind.shift.data(), file)
      
    }
    
  )
  # ----
  
  output$kev.nm.data.zip <- downloadHandler(
    # ----
    filename = function() {
      
      "kev.constants.data.zip"
      
    },
    
    content = function(file) {
      
      data.files <- c(
        
        nm.dt.coef = "input_stoichiometric_coefficients.csv"
        , nm.cnst = "input_k_constants_log10.csv"
        , nm.dt.conc = "input_concentrations.csv"
        , dt.nm = "input_chemical_shifts.csv"
        , nm.dt.ind = "input_individual_shifts.csv"
        , nm.dt.res = "equilibrium_concentrations.csv"
        , dt.nm.abs = "chemical_shifts_calculated_abs_errors.csv"
        , dt.nm.rel = "chemical_shifts_calculated_rel_errors.csv"
        , nm.cnst.dev = "constants_evaluated.csv"
        , nm.cor.m = "correlation_matrix.csv"
        , nm.adj.r.squared = "adj_r_squared.csv"
        , nm.ind.shift = "individual_shifts_calculated.csv"
        , nm.target = "target.csv"
        
      )
      
      # temporary directory to avoid permission issues
      
      curdir <- getwd()
      tmpdir <- tempdir()
      setwd(tmpdir)
      print(tempdir())
      
      
      for (i in length(data.files):1) {
        
        # check if all files are present (in case run before evaluation)
        
        dt <- NULL
        try(dt <- eval(expr = parse(text = paste0(names(data.files)[i], ".data()"))), silent = TRUE)
        
        if (nm.sep() == ";") {
          
          if (!is.null(dt)) {
            
            if (data.files[i] == "input_concentrations.csv") {
              
              dt <- nm.dt.conc.data()
              dt <- rbind(data.table(t(data.table(colnames(dt)))), dt, use.names = FALSE)
              
              setnames(dt, unlist(nm.part.eq.data()))
              
            }
            
            write.csv2(dt, data.files[i], row.names = FALSE)
            
          } else {
            
            data.files <- data.files[-i]
            
          }
          
        } else {
          
          if (!is.null(dt)) {
            
            if (data.files[i] == "input_concentrations.csv") {
              
              dt <- nm.dt.conc.data()
              dt <- rbind(data.table(t(data.table(colnames(dt)))), dt, use.names = FALSE)
              
              setnames(dt, unlist(nm.part.eq.data()))
              
            }
            
            write.csv(dt, data.files[i], row.names = FALSE)
            
          } else {
            
            data.files <- data.files[-i]
            
          }
        }
        
      }
      
      # create zip
      
      utils::zip(file, data.files)
      
      # remove garbage from the disc
      
      for (i in data.files) {
        
        if (file.exists(i))
          file.remove(i)
        
      }
      
      setwd(curdir)
      
    }
    
  )
  # ----
  
  output$kev.nm.data.xlsx <- downloadHandler(
    # ----
    filename = function() {
      
      "kev.constants.data.xlsx"
      
    },
    
    content = function(file) {
      
      data.files <- c(
        
        nm.dt.coef = "input_stoich_coefficients"
        , nm.cnst = "input_k_constants_log10"
        , nm.dt.conc = "input_concentrations"
        , dt.nm = "input_chemical_shifts"
        , nm.dt.ind = "input_individual_shifts"
        , nm.target = "target"
        , nm.dt.res = "equilibrium_concentrations"
        , dt.nm.abs = "chemical_shifts_calc_abs_err"
        , dt.nm.rel = "chemical_shifts_calc_rel_err"
        , nm.cnst.dev = "constants_evaluated"
        , nm.cor.m = "correlation_matrix"
        , nm.adj.r.squared = "adj_r_squared"
        , nm.ind.shift = "individual_shifts_calc"
        
      )
      
      dt.list <- list()
      
      for (i in 1:length(data.files)) {
        
        # check if all files are present (in case run before evaluation)
        
        dt <- NULL
        try(dt <- eval(expr = parse(text = paste0(names(data.files)[i], ".data()"))), silent = TRUE)
        
        if (!is.null(dt)) {
          
          if (data.files[i] == "input_concentrations") {
            
            dt <- nm.dt.conc.data()
            dt <- rbind(data.table(t(data.table(colnames(dt)))), dt, use.names = FALSE)
            
            setnames(dt, unlist(nm.part.eq.data()))
            
          }
          
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




