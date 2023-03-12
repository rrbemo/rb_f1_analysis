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
library(shinyWidgets)
library(shinycssloaders)
library(plotly)

# Define UI for application that draws a histogram
dashboardPage(
    dashboardHeader(title = "Ryan Bemowski F1 Analysis"),
    dashboardSidebar(),
    dashboardBody(
        fluidRow(
            column(width = 8, 
                box(width = 12,
                    plotlyOutput("analysis_plot1")),
                box(width = 12,
                    plotlyOutput("analysis_plot2")),
                box(width = 12,
                    plotlyOutput("analysis_plot3")),
                box(width = 12,
                    plotlyOutput("analysis_plot4"))),
            box(width = 4,
                uiOutput("ui_driver_options"))
        )
    )
)
