# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #

ui_emf <- function() {
 
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
           
  )   
}


