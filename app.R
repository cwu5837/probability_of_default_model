setwd("H:/trans mtx showcase")

# load data
load("data/00 loan.RData")
load("data/00 macro.RData")
load("data/Z Score.RData")
load("data/df_backtest.RData")
load("data/macro_var.RData")
load("data/macro_trans.RData")
load("data/stats.test.RData")
load("data/sign.RData")
load("data/final_model.RData")
load("data/pred_Z.RData")
load("data/pred_DF.RData")


# load required packages
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(DT)
library(googleVis)
library(gsubfn)

# load ui_sidebar and ui_body
source("ui_sidebar.R", local = TRUE)
source("ui_body.R", local = TRUE)

ui<- dashboardPage(
  
  ##### HEADER #####
  dashboardHeader(
    title = "Probability of Default Model" ,
    titleWidth = 300,
                  tags$li(a(href = 'https://github.com/cwu5837',
                            icon("github"),
                            title = "Back to My Github"),
                          class = "dropdown")
                  ),
  
  ##### SIDEBAR ######
  sidebar,
  
  ##### BODY #####
  body
)

server <- function(input, output) {
  
  source("functions.R", local = TRUE)
  source("1_1 Method.R", local = TRUE)
  source("2_1 Z Score.R", local = TRUE)
  source("2_2 DF Rate Backtesting.R", local = TRUE)
  source("2_3 Z vs Macro.R", local = TRUE)
  source("3_1 Var Selection.R", local = TRUE)
  source("3_2 Model Selection.R", local = TRUE)
  source("4_1 2 Factor Model.R", local = TRUE)
  source("4_2 3 Factor Model.R", local = TRUE)
  
}

shinyApp(ui, server)