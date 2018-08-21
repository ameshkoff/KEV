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



# frontend ------------------------------------------------- #

ui <- navbarPage("KEV",
                 tabPanel("Equilibrium concentrations"
                          , id = "page.eq.conc"
                          
                          , fluidPage(
                            
                            includeCSS("styles.css")
                            
                            , titlePanel("KEV: Chemistry Constant evaluator")
                            
                            , fluidRow(column(12), p(HTML("<br/>")))
                            
                            , titlePanel("Part 1: Equilibrium concentrations")
                            
                            , fluidRow(
                              
                              column(12,
                                     wellPanel(
                                       h4("Upload input data or insert by hand")
                                       , h4("Stechiometric coefficients")
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
                                       
                                     ) 
                              )
                            )
                          )),
                 tabPanel("To do"),
                 tabPanel("To do")
)


# backend -------------------------------------------------- #

server <- function(input, output) {
   
  values <- reactiveValues()
  
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
    
    values[["dt.coef"]] <- dt.coef
    
    dt.coef
    
  })
  
  output$dt.coef <- renderRHandsontable({
    
    in.file <- input$file.dt.coef
    
    dt.coef <- dt.coef.data()
    
    if (!is.null(in.file)) {
      
      # browser()
      dt.coef <- read.csv2(in.file$datapath, stringsAsFactors = FALSE, colClasses = "character")
      
    }
      
    if (!is.null(dt.coef))
      rhandsontable(dt.coef, stretchH = "all", useTypes = FALSE) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    
  })
  
  output$dt.coef.csv <- downloadHandler(
    # ----
    filename = function() {
      
      "dt.coef.csv"
      
    },
    
    content = function(file) {
      
      write.csv2(dt.coef.data(), file, row.names = FALSE)
      
    }
    
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

