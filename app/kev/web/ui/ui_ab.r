# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #

ui_ab <- function() {
 
  tabPanel(title = "Spectrophotometry"
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
                                   , fileInput("file.ab.bulk.input", "Choose CSV files or XLSX file with multiple sheets",
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
           
  )
   
}


