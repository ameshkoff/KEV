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

cur.task.list <- list("Spectrophotometry:UV-Vis" = "spectrophotometry:uv-vis"
                       , "Spectrophotometry:IR" = "spectrophotometry:ir"
                       , "Other" = "other")

cur.curves.list <- list("Add Curve", "Gaussian", "Lorentzian")
cur.algorithms <- data.table(value = c("gaussnewton", "neldermead")
                             , label = c("Gauss-Newton", "Nelder-Mead"))


# load algorithm

source("eq_runner.r", chdir = TRUE)
source("ab_runner.r", chdir = TRUE)
source("sp_runner.r", chdir = TRUE)
source("emf_runner.r", chdir = TRUE)
source("nm_runner.r", chdir = TRUE)
source("cur_runner.r", chdir = TRUE)

# load ui modules

source("web/ui/ui_eq.r")
source("web/ui/ui_ab.r")
source("web/ui/ui_sp.r")
source("web/ui/ui_emf.r")
source("web/ui/ui_nm.r")
source("web/ui/ui_cur.r")



# frontend ------------------------------------------------- #

ui <- tagList(
  
  tags$head(

    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Saira+Extra+Condensed:400,500,700")
    , tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Muli:400,700")

  ),
  
  navbarPage(title = "KEV",
             collapsible = TRUE,
             position = "static-top",
             windowTitle = "KEV: Constant Evaluator",
             theme = "kev.css",

  ui_eq(google.an),

  navbarMenu("Equilibrium Constants"
     , ui_ab()
     , ui_sp()
     , ui_emf()
     , ui_nm()),

  ui_cur(cur.task.list, cur.curves.list, cur.algorithms)
  
  , navbarMenu("More",
               tabPanel(HTML(paste0(
                 "<li><a href='https://gitlab.com/a.meshkov/KEV/raw/master/userguide/User_Guide_"
                 , userguide.date
                 ,".pdf?inline=false' target='_blank'>Help</a></li>"
                 ,"<li><a href='https://k-ev.org' target='_blank'>Home"))
               ))

  )
  , tags$footer(class = "kev-footer", HTML("Copyright 2018, 2019 &copy; G.Gamov, A.Meshkov | GNU GPL 3.0 | <a href='../#contact'>Contact us</a>"))

)


# backend -------------------------------------------------- #

server <- function(input, output, session) {

  source("web/server/logic.r", local = TRUE)
  
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
    
    , cur.is.file.loaded = TRUE
    
  )

  # logic
  
  source("web/server/logic_eq.r", local = TRUE)
  source("web/server/logic_ab.r", local = TRUE)
  source("web/server/logic_sp.r", local = TRUE)
  source("web/server/logic_emf.r", local = TRUE)
  source("web/server/logic_nm.r", local = TRUE)
  source("web/server/logic_cur.r", local = TRUE)
  
  # end of main server part ----------------------------
  
  # source download handlers
  
  source("web/server/download.handlers.r", local = TRUE)
  
  source("web/server/download.handlers_eq.r", local = TRUE)
  source("web/server/download.handlers_ab.r", local = TRUE)
  source("web/server/download.handlers_sp.r", local = TRUE)
  source("web/server/download.handlers_emf.r", local = TRUE)
  source("web/server/download.handlers_nm.r", local = TRUE)
  source("web/server/download.handlers_cur.r", local = TRUE)
  
}


# run

shinyApp(ui = ui, server = server)




