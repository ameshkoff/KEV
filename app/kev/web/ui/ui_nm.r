# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #

ui_nm <- function() {
 
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
                                   , fileInput("file.nm.bulk.input", "Choose CSV files or XLSX file with multiple sheets",
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

}


