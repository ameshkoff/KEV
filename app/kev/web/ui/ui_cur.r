# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #

ui_cur <- function(cur.task.list, cur.curves.list, cur.algorithms) {
 
  tabPanel("Curve Fitting"
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
                          , column(4
                                   , h4("Task")
                                   , selectInput("cur.task"
                                                 , ""
                                                 , cur.task.list
                                                 , selected = "spectrophotometry:uv-vis")
                          ), column(4
                                    , h4("Algorithm")
                                    , selectInput("cur.algorithm"
                                                  , ""
                                                  , cur.algorithms[, label]
                                                  , selected = "Gauss-Newton")
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
                                 , fluidRow(column(6
                                                   , textInput("cur.plot.labels"
                                                               , NULL
                                                               , value = "X Axis Label"))
                                            , column(6
                                                     , textInput("cur.plot.values"
                                                                 , NULL
                                                                 , value = "Y Axis Label")))
                                 , fluidRow(column(6
                                                   , numericInput("cur.window.left"
                                                                  , "Left Bound"
                                                                  , 0))
                                            , column(6
                                                     , numericInput("cur.window.right"
                                                                    , "Right Bound"
                                                                    , 0)))
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
                                                   , htmlOutput("cur.title")
                                                   , plotlyOutput("cur.plot.curves"
                                                                  , height = "600px"
                                                   ))
                                        , tabPanel("Data"
                                                   , fluidRow(column(5
                                                                     , h4("Curve Areas")
                                                                     , rHandsontableOutput("cur.auc"))
                                                              , column(7
                                                                       , h4("Parameters")
                                                                       , rHandsontableOutput("cur.dt.par")))
                                                   , fluidRow(column(4
                                                                     , h4("Input Data")
                                                                     , rHandsontableOutput("cur.dt.init"))
                                                              , column(8
                                                                       , h4("Fitted Curves")
                                                                       , rHandsontableOutput("cur.object.effects"))
                                                   )
                                        )
                          )
                          
                          , fluidRow(class = "download-row"
                                     , downloadButton("kev.cur.data.bottom.zip", "zip")
                                     , downloadButton("kev.cur.data.bottom.xlsx", "xlsx"))))
                 
               ))
               
             )
             
           ))
}


