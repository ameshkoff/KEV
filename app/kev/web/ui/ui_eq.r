# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #

ui_eq <- function(google.an) {
  
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
                            , rHandsontableOutput("eq.dt.coef")
                            # , ui_dt.coef("eq")
                            , fileInput("file.eq.dt.coef", "Choose CSV File",
                                        accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")
                            )
                            , fluidRow(class = "download-row"
                                       , downloadButton("eq.dt.coef.csv", "csv")
                                       , downloadButton("eq.dt.coef.xlsx", "xlsx"))
                            , p("")
                            , textInput("eq.part.names", "Component names, comma separated", paste(paste0("molecule", 1:4), collapse = ", "))
                     )
                     , column(2
                              , h4("K: lg constants")
                              , rHandsontableOutput("eq.cnst")
                              , fileInput("file.eq.cnst", "Choose CSV File",
                                          accept = c(
                                            "text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv")
                              )
                              , fluidRow(class = "download-row"
                                         , downloadButton("eq.cnst.csv", "csv")
                                         , downloadButton("eq.cnst.xlsx", "xlsx"))
                     )
                     , column(5
                              , h4("Concentrations")
                              , tabsetPanel(type = "tabs", id = "eq.conc.tab"
                                            , tabPanel("Input", value = "input"
                                                       , rHandsontableOutput("eq.dt.conc")
                                                       , rHandsontableOutput("eq.part.eq")
                                                       , fluidRow(class = "download-row"
                                                                  , downloadButton("eq.dt.conc.csv", "csv")
                                                                  , downloadButton("eq.dt.conc.xlsx", "xlsx")))
                                            , tabPanel("Total", value = "total"
                                                       , rHandsontableOutput("eq.dt.conc.tot")
                                                       , fluidRow(class = "download-row"
                                                                  , downloadButton("eq.dt.conc.tot.csv", "csv")
                                                                  , downloadButton("eq.dt.conc.tot.xlsx", "xlsx")))
                                            , tabPanel("pC range (optional)", value = "pc"
                                                       , h5("Variable component and its pC range")
                                                       , textInput("eq.pc.name", "", "molecule1")
                                                       , sliderInput("eq.pc.range", "", min = 0, max = 15, value = c(0, 3))
                                                       , h5("Total concentations of other components")
                                                       , rHandsontableOutput("eq.dt.conc.pc")
                                                       , fluidRow(class = "download-row"
                                                                  , actionButton("eq.pc.update.btn", "Update Input Concentrations")))
                              )
                              , fileInput("file.eq.dt.conc", "Choose CSV File",
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
                                 , rHandsontableOutput("eq.dt.res")
                                 , fluidRow(class = "download-row"
                                            , downloadButton("eq.dt.res.csv", "csv")
                                            , downloadButton("eq.dt.res.xlsx", "xlsx"))))
                 
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
             
           ))
  
}
