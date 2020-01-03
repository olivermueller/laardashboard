#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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

# Load data
joined <- read_csv("joined.csv")
joined_s <- read_csv("joined_s.csv")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    joined_f <- reactive({
        query <- parseQueryString(session$clientData$url_search) 
        print(str_replace(query, "user=", ""))
        filter(joined, session == str_replace(query, "user=", ""))
    })

    # Parse the GET query string
    output$queryText <- renderText({
        query <- parseQueryString(session$clientData$url_search)
        str_c("User: ", str_replace(query, "user=", ""))
    })
    
    output$vbox_time_individual <- renderValueBox({
        valueBox(
            round(joined_f()$time_for_corner_1+joined_f()$time_for_corner_2+joined_f()$time_for_corner_3+joined_f()$time_for_corner_4, 2), "Overall time to completion (in sec.)", icon = icon("list"),
            color = "blue"
        )
    })
    
    output$vbox_performance_individual <- renderValueBox({
        valueBox(
            round((joined_f()$score_corner_1+joined_f()$score_corner_2+joined_f()$score_corner_3+joined_f()$score_corner_4)/4.0, 2), "Overall score", icon = icon("list"),
            color = "blue"
        )
    })
    
    output$plot_time_individual <- renderPlot({
        ggplot(data=joined_f()) +
            geom_col(mapping = aes(x="#1", y=time_for_corner_1), fill = "steelblue") +
            geom_col(mapping = aes(x="#2", y=time_for_corner_2), fill = "steelblue") +
            geom_col(mapping = aes(x="#3", y=time_for_corner_3), fill = "steelblue") +
            geom_col(mapping = aes(x="#4", y=time_for_corner_4), fill = "steelblue") +
            geom_errorbar(data=joined_s, color="darkgrey", size=2, mapping = aes(x="#1", ymin=time_for_corner_1_avg, ymax=time_for_corner_1_avg)) +
            geom_errorbar(data=joined_s, color="darkgrey", size=2, mapping = aes(x="#2", ymin=time_for_corner_2_avg, ymax=time_for_corner_2_avg)) +
            geom_errorbar(data=joined_s, color="darkgrey", size=2, mapping = aes(x="#3", ymin=time_for_corner_3_avg, ymax=time_for_corner_3_avg)) +
            geom_errorbar(data=joined_s, color="darkgrey", size=2, mapping = aes(x="#4", ymin=time_for_corner_4_avg, ymax=time_for_corner_4_avg)) +
            scale_x_discrete(name = "Corner") +
            scale_y_continuous(limits = c(0, 300), name = "Time (in sec.)")
    })
    
    output$plot_score_individual <- renderPlot({
        ggplot(data=joined_f()) +
            geom_col(mapping = aes(x="#01: Take egg", y=score_activity_01), fill = "steelblue") +
            geom_col(mapping = aes(x="#02: Choose egg of correct size", y=score_activity_02), fill = "steelblue") +
            geom_col(mapping = aes(x="#03: Check egg for outside damage", y=score_activity_03), fill = "steelblue") +
            geom_col(mapping = aes(x="#04: Check egg for inside damage", y=score_activity_04), fill = "steelblue") +
            geom_col(mapping = aes(x="#05: Place egg in right direction", y=score_activity_05), fill = "steelblue") +
            geom_col(mapping = aes(x="#06: Take correct pivot", y=score_activity_06), fill = "steelblue") +
            geom_col(mapping = aes(x="#07: Check pivot for damage", y=score_activity_07), fill = "steelblue") +
            geom_col(mapping = aes(x="#08: Disposed damaged pivot, if needed", y=score_activity_08), fill = "steelblue") +
            geom_col(mapping = aes(x="#09: Place pivot in right direction", y=score_activity_09), fill = "steelblue") +
            geom_col(mapping = aes(x="#10: Check if pivot fits", y=score_activity_10), fill = "steelblue") +
            geom_col(mapping = aes(x="#11: Wobble or hammer pivot, if needed", y=score_activity_11), fill = "steelblue") +
            geom_col(mapping = aes(x="#12: Choose correct splint pen", y=score_activity_12), fill = "steelblue") +
            geom_col(mapping = aes(x="#13: Check if split pen closes properly", y=score_activity_13), fill = "steelblue") +
            geom_col(mapping = aes(x="#14: Put splint pen into pivot", y=score_activity_14), fill = "steelblue") +
            geom_text(mapping = aes(x="#01: Take egg", y=score_activity_01, label=round(score_activity_01,2)), vjust=1.6, color="white") +
            geom_text(mapping = aes(x="#02: Choose egg of correct size", y=score_activity_02, label=round(score_activity_02,2)), vjust=1.6, color="white") +
            geom_text(mapping = aes(x="#03: Check egg for outside damage", y=score_activity_03, label=round(score_activity_03,2)), vjust=1.6, color="white") +
            geom_text(mapping = aes(x="#04: Check egg for inside damage", y=score_activity_04, label=round(score_activity_04,2)), vjust=1.6, color="white") +
            geom_text(mapping = aes(x="#05: Place egg in right direction", y=score_activity_05, label=round(score_activity_05,2)), vjust=1.6, color="white") +
            geom_text(mapping = aes(x="#06: Take correct pivot", y=score_activity_06, label=round(score_activity_06,2)), vjust=1.6, color="white") +
            geom_text(mapping = aes(x="#07: Check pivot for damage", y=score_activity_07, label=round(score_activity_07,2)), vjust=1.6, color="white") +
            geom_text(mapping = aes(x="#08: Disposed damaged pivot, if needed", y=score_activity_08, label=round(score_activity_08,2)), vjust=1.6, color="white") +
            geom_text(mapping = aes(x="#09: Place pivot in right direction", y=score_activity_09, label=round(score_activity_09,2)), vjust=1.6, color="white") +
            geom_text(mapping = aes(x="#10: Check if pivot fits", y=score_activity_10, label=round(score_activity_10,2)), vjust=1.6, color="white") +
            geom_text(mapping = aes(x="#11: Wobble or hammer pivot, if needed", y=score_activity_11, label=round(score_activity_11,2)), vjust=1.6, color="white") +
            geom_text(mapping = aes(x="#12: Choose correct splint pen", y=score_activity_12, label=round(score_activity_12,2)), vjust=1.6, color="white") +
            geom_text(mapping = aes(x="#13: Check if split pen closes properly", y=score_activity_13, label=round(score_activity_13,2)), vjust=1.6, color="white") +
            geom_text(mapping = aes(x="#14: Put splint pen into pivot", y=score_activity_14, label=round(score_activity_14,2)), vjust=1.6, color="white") +
            geom_errorbar(data=joined_s, color="darkgrey", size=2, mapping = aes(x="#01: Take egg", ymin=score_activity_01_avg, ymax=score_activity_01_avg)) +
            geom_errorbar(data=joined_s, color="darkgrey", size=2, mapping = aes(x="#02: Choose egg of correct size", ymin=score_activity_02_avg, ymax=score_activity_02_avg)) +
            geom_errorbar(data=joined_s, color="darkgrey", size=2, mapping = aes(x="#03: Check egg for outside damage", ymin=score_activity_03_avg, ymax=score_activity_03_avg)) +
            geom_errorbar(data=joined_s, color="darkgrey", size=2, mapping = aes(x="#04: Check egg for inside damage", ymin=score_activity_04_avg, ymax=score_activity_04_avg)) +
            geom_errorbar(data=joined_s, color="darkgrey", size=2, mapping = aes(x="#05: Place egg in right direction", ymin=score_activity_05_avg, ymax=score_activity_05_avg)) +
            geom_errorbar(data=joined_s, color="darkgrey", size=2, mapping = aes(x="#06: Take correct pivot", ymin=score_activity_06_avg, ymax=score_activity_06_avg)) +
            geom_errorbar(data=joined_s, color="darkgrey", size=2, mapping = aes(x="#07: Check pivot for damage", ymin=score_activity_07_avg, ymax=score_activity_07_avg)) +
            geom_errorbar(data=joined_s, color="darkgrey", size=2, mapping = aes(x="#08: Disposed damaged pivot, if needed", ymin=score_activity_08_avg, ymax=score_activity_08_avg)) +
            geom_errorbar(data=joined_s, color="darkgrey", size=2, mapping = aes(x="#09: Place pivot in right direction", ymin=score_activity_09_avg, ymax=score_activity_09_avg)) +
            geom_errorbar(data=joined_s, color="darkgrey", size=2, mapping = aes(x="#10: Check if pivot fits", ymin=score_activity_10_avg, ymax=score_activity_10_avg)) +
            geom_errorbar(data=joined_s, color="darkgrey", size=2, mapping = aes(x="#11: Wobble or hammer pivot, if needed", ymin=score_activity_11_avg, ymax=score_activity_11_avg)) +
            geom_errorbar(data=joined_s, color="darkgrey", size=2, mapping = aes(x="#12: Choose correct splint pen", ymin=score_activity_12_avg, ymax=score_activity_12_avg)) +
            geom_errorbar(data=joined_s, color="darkgrey", size=2, mapping = aes(x="#13: Check if split pen closes properly", ymin=score_activity_13_avg, ymax=score_activity_13_avg)) +
            geom_errorbar(data=joined_s, color="darkgrey", size=2, mapping = aes(x="#14: Put splint pen into pivot", ymin=score_activity_14_avg, ymax=score_activity_14_avg)) +
            scale_x_discrete(name = "Activity") +
            scale_y_continuous(limits = c(0, 1), name = "Score") +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })
    
    output$plot_score_competences_individual <- renderPlot({
        ggplot(data=joined_f()) +
            geom_col(mapping = aes(x="10.02: Inspect equipment visualy for damage", y=score_competence_10_02), fill = "steelblue") +
            geom_col(mapping = aes(x="10.03: Choose right mounting accessories", y=score_competence_10_03), fill = "steelblue") +
            geom_col(mapping = aes(x="10.04: Choose right mounting methods", y=score_competence_10_04), fill = "steelblue") +
            geom_col(mapping = aes(x="10.05: Mount and rig equipment according to instructions", y=score_competence_10_05), fill = "steelblue") +
            geom_col(mapping = aes(x="10.08: Secure equipment and accessories", y=score_competence_10_08), fill = "steelblue") +
            geom_col(mapping = aes(x="10.10: Take action if something goes wrong", y=score_competence_10_10), fill = "steelblue") +
            geom_text(mapping = aes(x="10.02: Inspect equipment visualy for damage", y=score_competence_10_02, label=round(score_competence_10_02, 2)), vjust=1.6, color="white") +
            geom_text(mapping = aes(x="10.03: Choose right mounting accessories", y=score_competence_10_03, label=round(score_competence_10_03, 2)), vjust=1.6, color="white") +
            geom_text(mapping = aes(x="10.04: Choose right mounting methods", y=score_competence_10_04, label=round(score_competence_10_04, 2)), vjust=1.6, color="white") +
            geom_text(mapping = aes(x="10.05: Mount and rig equipment according to instructions", y=score_competence_10_05, label=round(score_competence_10_05, 2)), vjust=1.6, color="white") +
            geom_text(mapping = aes(x="10.08: Secure equipment and accessories", y=score_competence_10_08, label=round(score_competence_10_08, 2)), vjust=1.6, color="white") +
            geom_text(mapping = aes(x="10.10: Take action if something goes wrong", y=score_competence_10_10, label=round(score_competence_10_10, 2)), vjust=1.6, color="white") +
            geom_errorbar(data=joined_s, color="darkgrey", size=2, mapping = aes(x="10.02: Inspect equipment visualy for damage", ymin=score_activity_01_avg, ymax=score_activity_01_avg)) +
            geom_errorbar(data=joined_s, color="darkgrey", size=2, mapping = aes(x="10.03: Choose right mounting accessories", ymin=score_activity_02_avg, ymax=score_activity_02_avg)) +
            geom_errorbar(data=joined_s, color="darkgrey", size=2, mapping = aes(x="10.04: Choose right mounting methods", ymin=score_activity_03_avg, ymax=score_activity_03_avg)) +
            geom_errorbar(data=joined_s, color="darkgrey", size=2, mapping = aes(x="10.05: Mount and rig equipment according to instructions", ymin=score_activity_04_avg, ymax=score_activity_04_avg)) +
            geom_errorbar(data=joined_s, color="darkgrey", size=2, mapping = aes(x="10.08: Secure equipment and accessories", ymin=score_activity_05_avg, ymax=score_activity_05_avg)) +
            geom_errorbar(data=joined_s, color="darkgrey", size=2, mapping = aes(x="10.10: Take action if something goes wrong", ymin=score_activity_06_avg, ymax=score_activity_06_avg)) +
            scale_x_discrete(name = "Competence") +
            scale_y_continuous(limits = c(0, 1), name = "Score") +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })
})
