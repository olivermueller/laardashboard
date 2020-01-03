#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(scales)
theme_set(theme_classic())

shinyUI(dashboardPage(

    ## Header -------
    dashboardHeader(title = "LAAR: Trainee View"),
    
    ## Sidebar -------    
    dashboardSidebar(disable = TRUE),
    
    ## Body -------  
    dashboardBody(

        fluidRow(
            box(width = 12,
                textOutput("queryText")
            )
        ),        
        fluidRow(
            valueBoxOutput("vbox_time_individual"),
            valueBoxOutput("vbox_performance_individual")
        ),
        fluidRow(
            box(width = 12, title = "Time to completion", solidHeader = TRUE,
                plotOutput("plot_time_individual"))
        ),
        fluidRow(
            box(width = 12, title = "Score per activity", solidHeader = TRUE,
                plotOutput("plot_score_individual"))
        ),
        fluidRow(
            box(width = 12, title = "Score per competence", solidHeader = TRUE,
                plotOutput("plot_score_competences_individual"))
        ),
        fluidRow(
            box(width = 12, "Note: Grey horiziontal bars indicate average time or sccore over all trainees.")
        )
        )

))
