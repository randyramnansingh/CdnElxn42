
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

require(shiny) # For rendering shiny app 
require(leaflet)
require(ggplot2) # For rendering map visualisation

shinyUI(pageWithSidebar(
  
  # Application title
  titlePanel("Twitter Sentiment Analysis - Canada Elections 2015"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition = "input.conditionedPanels == 'Overall Twitter Sentiment by Geography'" ,
                       radioButtons("cd", "Candidates:",
                                    list("Stephen Harper" = "c1",
                                         "Tom Mulcair" = "c2",
                                         "Justin Trudeau" = "c3",
                                         "Gilles Duceppe" = "c4",
                                         "Elizabeth May" = "c5",
                                         "Jean-Francois Fortin" = "c6"), #c6 is dead, no data regarding this party
                                    selected = "c1")
      )
    ),
    
    conditionalPanel(condition = "input.conditionedPanels == 'Variation of Twitter Sentiment with other attributes'" ,
                     selectInput("Attributes", 
                                 label = "Select Attributes",
                                 choices = list("Population Density of Provinces", "Geographic Area of the Provinces", "Both"),
                                 selected = "Both")
    )
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Overall Twitter Sentiment by Geography", leafletOutput("MapandChart"),
               tags$div("____________________________________________________________________________________________________________________________"),
               "Timeseries analysis of Twitter Sentiment", plotOutput("TimeSeries")),
      id = "conditionedPanels"
    )
  )
  
))
