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
    dashboardHeader(title = "LAAR: Trainer View"),
    
    ## Sidebar -------  
    dashboardSidebar(
        
        sidebarMenu(
            menuItem("Time Overview", tabName = "time", icon = icon("clock")),
            menuItem("Performance Overview", tabName = "performance", icon = icon("award")),
            menuItem("Individual Analysis", tabName = "individual", icon = icon("user"))
        )
        
    ),
    
    ## Body -------  
    dashboardBody(
        tabItems(
            
            # Time dashboard tab content
            tabItem(tabName = "time",
                    fluidRow(
                        valueBoxOutput("vbox_number_of_observations"),
                        valueBoxOutput("vbox_avg_time_to_completion"),
                        valueBoxOutput("vbox_sd_time_to_completion")
                        ),
                    fluidRow(
                        box(width = 12, title = "Time to completion", solidHeader = TRUE,
                            plotOutput("plot_time_barchart"))
                    ),
                    fluidRow(
                        box(width = 3, title = "Time to completion Corner #1", solidHeader = TRUE, plotOutput("plot_time_histogram_c01")),
                        box(width = 3, title = "Time to completion Corner #2", solidHeader = TRUE, plotOutput("plot_time_histogram_c02")),
                        box(width = 3, title = "Time to completion Corner #3", solidHeader = TRUE, plotOutput("plot_time_histogram_c03")),
                        box(width = 3, title = "Time to completion Corner #4", solidHeader = TRUE, plotOutput("plot_time_histogram_c04"))
                    )
                    ),
            
            # Performance dashboard tab content
            tabItem(tabName = "performance",
                    fluidRow(
                        valueBoxOutput("vbox_number_of_observations2"),
                        valueBoxOutput("vbox_avg_score"),
                        valueBoxOutput("vbox_sd_score")
                    ),
                    fluidRow(
                        box(width = 12, title = "Average score per corner (% correctly performed activities)", solidHeader = TRUE,
                            plotOutput("plot_score_corners"))
                    ),
                    fluidRow(
                        box(width = 12, title = "Average score per activity (measured on Corner #2 only)", solidHeader = TRUE,
                            plotOutput("plot_score_activities"))
                    ),
                    fluidRow(
                        box(width = 12, title = "Average score per competence (according to ETTE/ESCO)", solidHeader = TRUE,
                            plotOutput("plot_score_competences"))
                    )
            ),
            
            # Individual dashboard tab content
            tabItem(tabName = "individual", 
                    fluidRow(
                        box(width = 12, title = "Select trainee", solidHeader = TRUE,
                            selectInput("trainee", "", ""))
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
                        box(width = 12, title = "Average score per activity", solidHeader = TRUE,
                            plotOutput("plot_score_individual"))
                    ),
                    fluidRow(
                        box(width = 12, title = "Average score per competence", solidHeader = TRUE,
                            plotOutput("plot_score_competences_individual"))
                    ),
                    fluidRow(
                        box(width = 12, "Note: Grey horiziontal bars indicate average time or sccore over all trainees.")
                    )
            )
        )
    )

))
