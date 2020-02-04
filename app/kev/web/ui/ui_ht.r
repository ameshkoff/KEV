# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #

ui_ht <- function() {
 
  tabPanel(title = "Calorimetry"
           , id = "page.ht"
           
           , fluidPage(
             
             fluidRow(column(12, p(HTML("KEV: Chemistry Constant Evaluator<br/>"))))
             
             , titlePanel(HTML("Equilibrium Constants <i>via Calorimetry</i>"))
             
             , fluidRow(column(
               12
               , wellPanel(
                 fluidRow(column(1, img(src = "ht-icon.png", class = "kev-icon"))
                          , column(2
                                   , h4("Column delimiter")
                                   , radioButtons("ht.sep", "", inline = TRUE
                                                  , c("," = "comma"
                                                      , ";" = "semicolon"
                                                      , "tab" = "tab")))
                          , column(3
                                   , HTML("<h4>Constants to evaluate</h4><p>Component names, comma separated</p>")
                                   , textInput("ht.cnst.tune", "", "molecule1"))
                          , column(3
                                   , HTML(paste("<h4>Threshold</h4><p>Search algorithm precision"
                                                ,"0&nbsp;&#60;&nbsp;&#950;&nbsp;&#60;&nbsp;1</p>"))
                                   , textInput("ht.threshold", "", "1e-7"))
                          , column(3
                                   , HTML("<h4>Search density</h4><p>Do not change unless you fully understand what you are doing</p>")
                                   , textInput("ht.search.density", "", "1"))
                 )
               )))
             
             , fluidRow(column(
               12
               , wellPanel(
                 fluidRow(column(12
                                 , h3("Bulk upload / download (optional)")))
                 
                 , fluidRow(column(5
                                   , h4("Upload all data")
                                   , fileInput("file.ht.bulk.input", "Choose CSV files or XLSX file with multiple sheets",
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
                                              , actionButton(inputId = "ht.example.data"
                                                             , label = HTML("&nbsp;Check examples")
                                                             , icon = icon("database")
                                                             , onclick = paste0("window.open('https://gitlab.com/"
                                                                                , "a.meshkov/KEV/tree/"
                                                                                , "master/input/calorimetry', '_blank')")))))
                 )
               )))
             
             , fluidRow(
               
               column(
                 12
                 , wellPanel(
                   h3("Upload or type input data")
                   , fluidRow(
                     column(5
                            , h4("Stoichiometric coefficients")
                            , rHandsontableOutput("ht.dt.coef")
                            , fileInput("file.ht.dt.coef", "Choose CSV File",
                                        accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")
                            )
                            , fluidRow(class = "download-row"
                                       , downloadButton("ht.dt.coef.csv", "csv")
                                       , downloadButton("ht.dt.coef.xlsx", "xlsx"))
                            , p("")
                            , textInput("ht.part.names", "Component names, comma separated"
                                        , paste(paste0("molecule", 1:4), collapse = ", "))
                     )
                     , column(2
                              , h4("K: lg constants")
                              , rHandsontableOutput("ht.cnst")
                              , fileInput("file.ht.cnst", "Choose CSV File",
                                          accept = c(
                                            "text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv")
                              )
                              , fluidRow(class = "download-row"
                                         , downloadButton("ht.cnst.csv", "csv")
                                         , downloadButton("ht.cnst.xlsx", "xlsx"))
                     )
                     , column(5
                              , h4("Concentrations")
                              , rHandsontableOutput("ht.dt.conc")
                              , rHandsontableOutput("ht.part.eq")
                              , fileInput("file.ht.dt.conc", "Choose CSV File",
                                          accept = c(
                                            "text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv")
                              )
                              , fluidRow(class = "download-row"
                                         , downloadButton("ht.dt.conc.csv", "csv")
                                         , downloadButton("ht.dt.conc.xlsx", "xlsx"))
                     )
                   )
                   , fluidRow(
                     column(6
                            , h4("Heats")
                            , p("Known heats")
                            , rHandsontableOutput("dt.heat")
                            , fileInput("file.dt.heat", "Choose CSV File",
                                        accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")
                            )
                            , fluidRow(class = "download-row"
                                       , downloadButton("dt.heat.csv", "csv")
                                       , downloadButton("dt.heat.xlsx", "xlsx"))
                            
                     )
                     , column(6
                              , h4("Enthalpies")
                              , p("Known Enthalpies")
                              , rHandsontableOutput("dt.enth")
                              , fileInput("file.dt.enth", "Choose CSV File",
                                                    accept = c(
                                                      "text/csv",
                                                      "text/comma-separated-values,text/plain",
                                                      ".csv"))
                              , fluidRow(class = "download-row"
                                         , downloadButton("dt.enth.csv", "csv")
                                         , downloadButton("dt.enth.xlsx", "xlsx"))
                     )
                   )
                   , fluidRow(
                     column(4
                            , HTML("<h4>Calorimeter type</h4><p>DSC, Ampoule or Overfilled</p>")
                            , textInput("ht.calorimeter.type", "", "DSC"))
                     , column(4
                              , HTML("<h4>Active/Initial volume</h4><p>&nbsp;</p>")
                              , numericInput("ht.init.vol", "", 1))
                     , column(4
                              , HTML("<h4>Component</h4><p>To get coefficient</p>")
                              , textInput("ht.cmp.tune", "", "molecule1"))
                   )
                 )
               )
             )
             
             , fluidRow(column(
               12
               , wellPanel(
                 fluidRow(column(12
                                 , actionButton("ht.conc.exec.btn", "Evaluate", class = "kev-ev-button")
                 ))
               )))
             
             , fluidRow(column(
               12
               , wellPanel(
                 fluidRow(column(12
                                 , h4("Equilibrium concentrations")
                                 , rHandsontableOutput("ht.dt.res")
                                 , fluidRow(class = "download-row"
                                            , downloadButton("ht.dt.res.csv", "csv")
                                            , downloadButton("ht.dt.res.xlsx", "xlsx"))))
                 
                 , fluidRow(column(12
                                   , h4("Calculated Heats")
                                   , tabsetPanel(type = "tabs"
                                                 , tabPanel("Data"
                                                            , rHandsontableOutput("dt.heat.calc")
                                                            , fluidRow(class = "download-row"
                                                                       , downloadButton("dt.heat.calc.csv", "csv")
                                                                       , downloadButton("dt.heat.calc.xlsx", "xlsx")))
                                                 , tabPanel("Plot"
                                                            , plotlyOutput("plot.dt.heat"))
                                   )))
                 
                 , fluidRow(column(12)
                            , column(6
                                     , h4("Evaluated Constants")
                                     , rHandsontableOutput("ht.cnst.dev")
                                     , fluidRow(class = "download-row"
                                                , downloadButton("ht.cnst.dev.csv", "csv")
                                                , downloadButton("ht.cnst.dev.xlsx", "xlsx")))
                            , column(3
                                     , h4("Correlation Matrix")
                                     , rHandsontableOutput("ht.cor.m")
                                     , fluidRow(class = "download-row"
                                                , downloadButton("ht.cor.m.csv", "csv")
                                                , downloadButton("ht.cor.m.xlsx", "xlsx")))
                            , column(3
                                     , h4(HTML("Adjusted R<sup>2</sup>"))
                                     , rHandsontableOutput("ht.adj.r.squared")
                                     , fluidRow(class = "download-row"
                                                , downloadButton("ht.adj.r.squared.csv", "csv")
                                                , downloadButton("ht.adj.r.squared.xlsx", "xlsx"))))
                 
                 , fluidRow(column(12
                                   , h4("Enthalpies with St.Errors")
                                   , rHandsontableOutput("dt.enth.calc")
                                   , fluidRow(class = "download-row"
                                              , downloadButton("dt.enth.calc.csv", "csv")
                                              , downloadButton("dt.enth.calc.xlsx", "xlsx"))))
                 
                 
               ))
             )
             
           )
           
  )
   
}


