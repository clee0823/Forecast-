library(shiny)
library(shinyjs)
library(shinythemes)
library(tidyverse)
library(shinydashboard)

library(DT)
library(data.table)
library(dplyr)
library(widyr)
library(gridExtra)
library(tibbletime)
library(tm)
library(timetk)
library(tidyquant)

library(forecast)
library(tseries)
library(quantmod)
library(bsts)

# Plot
library(ggplot2)
library(glue)

ui <- tagList(
            titlePanel("Basic Time Series Forecasting"),
            
            navbarPage(theme = shinytheme("darkly"),
               
                            tabPanel("Welcome", 
                                    sidebarPanel(verticalLayout(
                                       #tags$h4("Check the Stock:"),
                                       textInput("stockId", "Stock symbol"),
                                       actionButton("action", "Search"),
                                       tags$h5("Click to See:"),
                                       actionButton('show_kd','KD line'),
                                       actionButton('show_arima', 'ARMA'),
                                       actionButton('show_bsts', 'BSTS'),
                                       actionButton('show_error', 'Prediction Errors'),
                                       actionButton('select', 'Stock table')
                                       #tags$h5("Information:")
                                       #h6('ARMA & BSTS Prediction'),
                                       #DT::dataTableOutput('arma_table'),
                                      # verbatimTextOutput('mape_text')
                                    )),      
                                    
                                    sidebarPanel(
                                       hr(),
                                       tags$h5('Prediction Error'),
                                       #DT::dataTableOutput('arma_table'),
                                       verbatimTextOutput('mape_text')
                                       #verbatimTextOutput('systime')
                                    ),
                                    sidebarPanel(
                                       hr(),
                                       tags$h5('Processing time'),
                                       verbatimTextOutput('systime')
                                    ),
                                       
                                    mainPanel(
                                       tabsetPanel(
                                       tabPanel("Trend",
                                                #h5('Stock Trend'),
                                                plotOutput('trend_plot',width = 450,height = 300) #,width = 700,height = 300
                                       ),
                                       tabPanel('Plots', verticalLayout(
                                                strong('KD lines'),
                                                plotOutput('kd_plot',width = 450,height = 300),
                                                strong('ARMA'),
                                                plotOutput('arima_plot',width = 450,height = 300),
                                                strong('BSTS'),
                                                plotOutput('bsts_plot',width = 450,height = 300)
                                                )),
                                       
                                    tabPanel( "Tables", 
                                          #sidebarPanel(
                                          strong('ARMA Predicted 5-day table'),
                                          DT::dataTableOutput('arma_table'),
                                          strong('Bayesian Predicted 5-day table'),
                                          DT::dataTableOutput('bsts_table'),
                                          strong('Stock Table'),
                                          DT::dataTableOutput('x1')
                                          #verbatimTextOutput('bsts_text')
                                       
                                    ),
                                 tabPanel('Download',
                                          mainPanel(
                                             #plotOutput('trend_plot'), # overall trend of the data
                                             #tableOutput('x1'),
                                             # download file
                                             hr(),
                                             tags$style(".fa-chart-pie {color:#E87722}"),
                                             h5('Download data here'),
                                             fluidRow(
                                                p(class = 'text-center', downloadButton('x2', 'Download Stock Data')))
                                          )
                                          ))
                                    
                            ))))#fluid = FALSE