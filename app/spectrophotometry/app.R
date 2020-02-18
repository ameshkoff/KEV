# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: AMeshkov                                           #
# Date: 2020                                                 #
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

# prepare environment

main.folder <- "../kev/"

google.an <- paste0(main.folder, "google-analytics.html")
debug.mode <- FALSE

if (Sys.info()["sysname"] %like% "indows") Sys.setenv("R_ZIPCMD" = "c:/Rtools/bin/zip.exe")
if (debug.mode) google.an <- ""

options(shiny.sanitize.errors = TRUE)
`%then%` <- shiny:::`%OR%`

userguide.date <- "20190331"


# load algorithm

source("algo/spectrophotometry/ab_runner.r", chdir = TRUE)
source("algo/molar.extinction.coefficients/sp_runner.r", chdir = TRUE)

# load ui modules

source("web/ui/ui_ab.r", chdir = TRUE)
source("web/ui/ui_sp.r", chdir = TRUE)



# frontend ------------------------------------------------- #

ui <- tagList(
  
  tags$head(

    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Saira+Extra+Condensed:400,500,700")
    , tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Muli:400,700")

  )
  
  , navbarPage(title = "KEV"
               , collapsible = TRUE
               , position = "static-top"
               , windowTitle = "KEV: Constant Evaluator"
               , theme = "kev.css"
               # , selected = "page.ab"

  , navbarMenu("Equilibrium Concentrations"
               , HTML("<li><a href='https://k-ev.org/kev' target='_blank'>Equilibrium Concentrations</a></li>")
  )
    
  , navbarMenu("Equilibrium Constants"
     , ui_ab()
     , ui_sp()
     , HTML("<li><a href='https://k-ev.org/emf' target='_blank'>E.M.F. (Potentiometry)</a></li>")
     , HTML("<li><a href='https://k-ev.org/nmr' target='_blank'>NMR (Fast Exchange)</a></li>")
     , HTML("<li><a href='https://k-ev.org/calorimetry' target='_blank'>Calorimetry</a></li>")
     )
  
  , navbarMenu("Curve Fitting"
               , HTML("<li><a href='https://k-ev.org/curve' target='_blank'>Curve Fitting</a></li>")
  )
  
  , navbarMenu("More",
               tabPanel(HTML(paste0(
                 "<li><a href='https://gitlab.com/a.meshkov/KEV/raw/master/userguide/User_Guide_"
                 , userguide.date
                 ,".pdf?inline=false' target='_blank'>Help</a></li>"
                 ,"<li><a href='https://k-ev.org' target='_blank'>Home"))
               ))

  )
  , tags$footer(class = "kev-footer", HTML("Copyright 2018-2020 &copy; G.Gamov, A.Meshkov | GNU GPL 3.0 | <a href='../#contact'>Contact us</a>"))

)


# backend -------------------------------------------------- #

server <- function(input, output, session) {

  source(paste0(main.folder, "web/server/logic.r"), local = TRUE)
  
  values <- reactiveValues()
  
  input.source <- reactiveValues(
    
    ab.dt.coef.bulk = FALSE
    , ab.dt.conc.bulk = FALSE
    , ab.cnst.bulk = FALSE
    , dt.ab.bulk = FALSE
    , dt.mol.bulk = FALSE
    , dt.mol.memory = FALSE
    
  )
  
  # logic
  
  # source("web/server/logic_eq.r", local = TRUE)
  source("web/server/logic_ab.r", local = TRUE)
  source("web/server/logic_sp.r", local = TRUE)

  # end of main server part ----------------------------
  
  # source download handlers
  
  source(paste0(main.folder, "web/server/download.handlers.r"), local = TRUE)
  
  source("web/server/download.handlers_ab.r", local = TRUE)
  source("web/server/download.handlers_sp.r", local = TRUE)

}


# run

shinyApp(ui = ui, server = server)




