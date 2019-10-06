# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #

ui_sp <- function() {
 
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
           
  )   
}


